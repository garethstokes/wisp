# Agent Runs: Event-Sourced Execution Model

## Overview

This design introduces server-owned agent runs with full event logging, enabling:
- **Audit/Replay**: Every agent interaction is recorded with full context (prompts, tool calls, results)
- **Pause/Resume**: Agents can pause mid-execution (waiting for human input, async tools) and resume later
- **Multi-agent orchestration**: Runs can spawn child runs, events can reference parent events (causality DAG)

Based on principles from [12-factor-agents](https://github.com/humanlayer/12-factor-agents):
- Factor 3: Own your context window
- Factor 5: Unify execution state and business state
- Factor 6: Launch/pause/resume with simple APIs
- Factor 12: Agent as stateless reducer

## Core Concepts

### Runs and Sessions

- **Run**: A single agent turn (one CLI invocation or API call). The atomic unit of execution.
- **Session**: A collection of runs sharing conversational context. Sessions remain a grouping convenience; runs are the source of truth.
- **Parent Run**: Runs can spawn child runs (agent orchestration). `parent_run_id` links to the spawning run.

### Agents as Tool Functions

Agents are typed functions with explicit inputs and outputs:

```
Agent[InputTools, OutputTools]
```

**Input Tools** (what triggers/feeds an agent):
- `message_from_user` — User typed something
- `awake_from_system` — System trigger (cron, webhook, event)
- `system_message` — Context injection
- `tool_result` — Result from a previously requested tool
- `human_response` — Human answered an `ask_human` request

**Output Tools** (what an agent can request):

Base tools (every agent):
- `respond_to_user` — Return message to user, completes the run
- `ask_human` — Pause run, wait for human input
- `spawn_agent` — Launch a child run
- `done` — Explicitly end the run

Agent-specific tools:
- Concierge: `update_activities`, `query_activities`, `query_people`
- Scheduler: `query_calendar`, `find_free_slots`
- Insights: `search_activities`, `get_summary`, `get_people_insights`

## Data Model

### Run Type

```haskell
data RunId = RunId Text
data SessionId = SessionId Text

data Run = Run
  { runId :: RunId
  , runParentRunId :: Maybe RunId
  , runAgent :: Text
  , runSessionId :: Maybe SessionId
  , runCreatedAt :: UTCTime
  , runUpdatedAt :: UTCTime
  , runStatus :: RunStatus
  , runEvents :: [RunEvent]
  }

data RunStatus
  = Running     -- Currently executing
  | Waiting     -- Paused, waiting for external input
  | Completed   -- Successfully finished
  | Failed      -- Terminated with error
```

### Event Types (Append-Only)

```haskell
data RunEvent
  = InputEvent
      { eventId :: Text
      , eventParentEventId :: Maybe Text
      , eventTimestamp :: UTCTime
      , eventTool :: Text           -- "message_from_user", "tool_result", etc.
      , eventData :: Value
      }
  | ContextAssembled
      { eventId :: Text
      , eventParentEventId :: Maybe Text
      , eventTimestamp :: UTCTime
      , eventContext :: Value       -- Calendar, activities, etc.
      }
  | LlmCalled
      { eventId :: Text
      , eventParentEventId :: Maybe Text
      , eventTimestamp :: UTCTime
      , eventModel :: Text
      , eventSystemPrompt :: Text
      , eventUserPrompt :: Text
      , eventRawResponse :: Text
      }
  | ToolRequested
      { eventId :: Text
      , eventParentEventId :: Maybe Text  -- Points to LlmCalled
      , eventTimestamp :: UTCTime
      , eventToolName :: Text
      , eventToolArgs :: Value
      }
  | ToolSucceeded
      { eventId :: Text
      , eventParentEventId :: Maybe Text  -- Points to ToolRequested
      , eventTimestamp :: UTCTime
      , eventToolName :: Text
      , eventResult :: Value
      }
  | ToolFailed
      { eventId :: Text
      , eventParentEventId :: Maybe Text  -- Points to ToolRequested
      , eventTimestamp :: UTCTime
      , eventToolName :: Text
      , eventError :: Text
      }
```

### Database Schema

```sql
CREATE TABLE agent_runs (
  id TEXT PRIMARY KEY,
  parent_run_id TEXT REFERENCES agent_runs(id) ON DELETE SET NULL,
  agent_id TEXT NOT NULL,
  session_id TEXT,
  status TEXT NOT NULL DEFAULT 'running',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE run_events (
  id TEXT PRIMARY KEY,
  run_id TEXT NOT NULL REFERENCES agent_runs(id) ON DELETE CASCADE,
  parent_event_id TEXT REFERENCES run_events(id) ON DELETE SET NULL,
  event_seq INT NOT NULL,
  event_type TEXT NOT NULL,
  event_data JSONB NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),

  UNIQUE (run_id, event_seq)
);

CREATE INDEX idx_runs_session ON agent_runs(session_id) WHERE session_id IS NOT NULL;
CREATE INDEX idx_runs_parent ON agent_runs(parent_run_id) WHERE parent_run_id IS NOT NULL;
CREATE INDEX idx_runs_status ON agent_runs(status);
CREATE INDEX idx_runs_agent ON agent_runs(agent_id);
CREATE INDEX idx_events_run ON run_events(run_id, event_seq);
CREATE INDEX idx_events_parent ON run_events(parent_event_id) WHERE parent_event_id IS NOT NULL;
CREATE INDEX idx_events_type ON run_events(run_id, event_type);
```

## Agent Loop

### Tool Classification

Every output tool falls into one of three categories:

- **Terminal**: Ends the run (`respond_to_user`, `done`)
- **Async**: Pauses the run (`ask_human`, `spawn_agent`)
- **Sync**: Executes immediately and loops (`query_activities`, etc.)

### Core Loop

```haskell
runAgent :: RunId -> AgentDef -> App ()
runAgent runId agent = do
  events <- getRunEvents runId
  let status = deriveRunStatus events

  case status of
    Completed -> pure ()
    Failed -> pure ()
    Waiting -> pure ()
    Running -> do
      -- 1. Assemble context
      context <- assembleContext agent
      contextEventId <- appendEvent runId $ ContextAssembled { eventContext = context, .. }

      -- 2. Build prompts
      let systemPrompt = buildSystemPrompt agent context
          userPrompt = eventsToPrompt events

      -- 3. Call LLM
      rawResponse <- callClaudeWithSystem apiKey model systemPrompt userPrompt
      llmEventId <- appendEvent runId $ LlmCalled { .. }

      -- 4. Parse tool call
      toolCall <- parseToolCall rawResponse
      requestEventId <- appendEvent runId $ ToolRequested
        { eventParentEventId = Just llmEventId, .. }

      -- 5. Execute or pause
      case classifyTool (toolCallName toolCall) of
        Terminal -> do
          result <- executeTool toolCall
          appendEvent runId $ ToolSucceeded { eventParentEventId = Just requestEventId, .. }

        Async -> pure ()  -- Exit loop, wait for external input

        Sync -> do
          result <- executeTool toolCall
          appendEvent runId $ ToolSucceeded { eventParentEventId = Just requestEventId, .. }
          appendEvent runId $ InputEvent
            { eventTool = "tool_result", eventData = result, eventParentEventId = Just requestEventId, .. }
          runAgent runId agent  -- Continue loop
```

### Status Derivation

Status is computed from events (cached in DB for queries):

```haskell
deriveRunStatus :: [RunEvent] -> RunStatus
deriveRunStatus events = case lastMay events of
  Nothing -> Running
  Just (ToolSucceeded { eventToolName = t })
    | t == "respond_to_user" -> Completed
    | t == "done"            -> Completed
    | otherwise              -> Running
  Just (ToolRequested { eventToolName = t })
    | t == "ask_human"   -> Waiting
    | t == "spawn_agent" -> Waiting
    | otherwise          -> Running
  Just (ToolFailed {}) -> Running  -- Error fed back to LLM
  Just (InputEvent {}) -> Running
  Just _ -> Running
```

## Context Window Construction

### Events to Prompt

Convert event log to what the LLM sees (12-factor style XML tags):

```haskell
eventsToPrompt :: [RunEvent] -> Text
eventsToPrompt events = T.intercalate "\n\n" $ mapMaybe eventToSection events

eventToSection :: RunEvent -> Maybe Text
eventToSection = \case
  InputEvent { eventTool = tool, eventData = d } ->
    Just $ "<" <> tool <> ">\n" <> encodeToYaml d <> "\n</" <> tool <> ">"

  ToolRequested { eventToolName = name, eventToolArgs = args } ->
    Just $ "<" <> name <> ">\n" <> encodeToYaml args <> "\n</" <> name <> ">"

  ToolSucceeded { eventToolName = name, eventResult = result } ->
    Just $ "<" <> name <> "_result>\n" <> encodeToYaml result <> "\n</" <> name <> "_result>"

  ToolFailed { eventToolName = name, eventError = err } ->
    Just $ "<" <> name <> "_error>\n" <> err <> "\n</" <> name <> "_error>"

  _ -> Nothing  -- LlmCalled, ContextAssembled are audit-only
```

### System Prompt

```haskell
{-# LANGUAGE QuasiQuotes #-}
import NeatInterpolation (text)

buildSystemPrompt :: AgentDef -> Value -> Text
buildSystemPrompt agent context = [text|
  You are $agentDesc.

  ## Available Tools
  $toolDefs

  ## Current State
  $contextYaml

  ## Response Format
  Respond with a tool call in XML format: <tool_name>...args...</tool_name>
|]
  where
    agentDesc = agentDescription agent
    toolDefs = formatToolDefs (agentOutputTools agent)
    contextYaml = encodeToYaml context
```

## Server API

```
POST   /runs                    -- Create and execute a new run
GET    /runs/:id                -- Get run with full event log
POST   /runs/:id/resume         -- Resume a waiting run
GET    /runs/:id/events         -- Get events only
GET    /runs?session_id=X       -- List runs for a session
GET    /runs?agent_id=X         -- List runs for an agent
GET    /runs?status=waiting     -- Find paused runs
```

### Create Run

```
POST /runs
{
  "agent": "wisp/concierge",
  "input_tool": "message_from_user",
  "input_data": "Show me quarantined items",
  "session_id": "default",
  "parent_run_id": null
}

Response:
{
  "run_id": "run_xyz789",
  "status": "completed",
  "events": [ ... ]
}
```

### Resume Run

```
POST /runs/:id/resume
{
  "input_tool": "human_response",
  "input_data": "Yes, approve those changes"
}
```

## CLI Changes

```bash
wisp chat -a wisp/concierge -m "hello"
  # -> POST /runs { agent, input_tool: "message_from_user", ... }

wisp runs                        # List recent runs
wisp runs <id>                   # Show full event log
wisp runs <id> --replay          # Re-execute from events
wisp runs <id> --resume "yes"    # Resume a waiting run
```

## Migration Path

### Phase 1: Foundation (no breaking changes)
1. Add `agent_runs` and `run_events` tables
2. Add `Domain.Run` module with types
3. Add `Infra.Db.Run` module with queries
4. Add event append logic alongside existing `insertConversation`

### Phase 2: Instrument existing agents
5. Wrap existing `handleChat` to create runs and append events
6. Both systems run in parallel (existing behavior + event logging)

### Phase 3: Unified tool model
7. Add `respond_to_user` to each agent's tool set
8. Update prompts to require explicit tool calls
9. Update response parsing for tool call format

### Phase 4: New API surface
10. Add `/runs` endpoints
11. Update CLI to use `/runs` API

### Phase 5: Cleanup
12. Deprecate `/chat` endpoint
13. Remove `insertConversation`
14. CLI session files become optional cache

## Key Benefits

1. **Full audit trail**: Every LLM call, tool execution, and result is recorded
2. **Replay**: Given events, reconstruct exactly what the LLM saw
3. **Pause/Resume**: Agents can wait for human input or async operations
4. **Multi-agent**: Runs spawn runs, events reference events (causality DAG)
5. **Trigger from anywhere**: Server owns state, CLI/HTTP/cron/agents can all create runs
6. **Debugging**: Inspect exact prompts, responses, and tool results for any run
