# Chat Improvements Design

## Overview

This design adds agent selection and session persistence to the chat system.

## Goals

1. **Agent selection** - Specify which agent to talk to via `wisp/concierge`, `wisp/scheduler`, etc.
2. **Session persistence** - Remember chat history in local files, send full history to server.
3. **Agent registry** - `wisp agents` command to list available agents with their tools and workflows.

---

## Agent Registry & Metadata

Each agent lives in its own module under `Agents/` and exports an `agentInfo` value.

### New types in `Domain/Agent.hs`:

```haskell
data AgentInfo = AgentInfo
  { agentId          :: Text
  , agentDescription :: Text
  , agentTools       :: [ToolInfo]
  , agentWorkflows   :: [Text]
  , agentImplemented :: Bool
  }

data ToolInfo = ToolInfo
  { toolName :: Text
  , toolType :: ToolType
  }

data ToolType = Deterministic | Decision
```

### Agent modules:

- `Agents/Concierge.hs` - already exists, add `agentInfo`
- `Agents/Scheduler.hs` - stub with `agentInfo`, `implemented = False`
- `Agents/Housekeeper.hs` - stub with `agentInfo`, `implemented = False`
- `Agents/Insights.hs` - stub with `agentInfo`, `implemented = False`

### New endpoint:

`GET /agents` returns all agent info as JSON.

### New CLI command:

`wisp agents` lists all agents with their tools/workflows.

---

## Agent Selection in Chat

### CLI syntax:

```bash
wisp chat --agent "wisp/concierge" --message "Hello"
wisp chat -a "wisp/concierge" -m "Hello"           # Short form
wisp chat --agent "wisp/concierge"                  # Interactive mode (future)
```

Missing `--agent` returns error: "Please specify an agent with --agent (e.g., wisp/concierge)"

### optparse-applicative parser:

```haskell
data ChatOptions = ChatOptions
  { chatAgent   :: Text
  , chatMessage :: Maybe Text  -- Nothing = interactive mode (future)
  }

chatParser :: Parser Command
chatParser = Chat <$> (ChatOptions
  <$> strOption (long "agent" <> short 'a' <> metavar "AGENT" <> help "Agent path (e.g., wisp/concierge)")
  <*> optional (strOption (long "message" <> short 'm' <> metavar "MSG" <> help "Message to send")))
```

### Server payload:

```json
{
  "agent": "wisp/concierge",
  "messages": [...]
}
```

### Agent dispatch:

```haskell
-- Agents/Dispatcher.hs
dispatchChat :: Text -> [ChatMessage] -> App (Either Text ChatResponse)
dispatchChat "wisp/concierge"   msgs = Concierge.handleChat msgs
dispatchChat "wisp/scheduler"   _    = pure $ Left "Agent 'wisp/scheduler' not implemented"
dispatchChat "wisp/housekeeper" _    = pure $ Left "Agent 'wisp/housekeeper' not implemented"
dispatchChat "wisp/insights"    _    = pure $ Left "Agent 'wisp/insights' not implemented"
dispatchChat agent              _    = pure $ Left $ "Unknown agent: " <> agent
```

This pattern allows future namespaces like `custom/my-agent` or `plugins/...`.

---

## Session Management

### Session storage location:

`~/.wisp/sessions/<name>.json`

### CLI flags:

```bash
wisp chat -a "wisp/concierge" -m "Hello"                    # Uses "default" session
wisp chat -a "wisp/concierge" -m "Hello" --session work     # Uses "work" session
wisp chat -a "wisp/concierge" --session work --new          # Clears and starts fresh
wisp sessions                                                # List all sessions
wisp sessions --delete work                                  # Delete a session
```

### Session file structure (`~/.wisp/sessions/work.json`):

```json
{
  "agent": "wisp/concierge",
  "created_at": "2026-02-04T12:00:00Z",
  "updated_at": "2026-02-04T12:05:00Z",
  "messages": [
    {"role": "user", "content": "Show me quarantined items"},
    {"role": "assistant", "content": "...", "tool_call": {...}},
    {"role": "tool", "content": "{\"activities\": [...]}"},
    {"role": "assistant", "content": "Here are 3 quarantined items..."}
  ]
}
```

### Behavior:

- If session exists with different agent, error: "Session 'work' is bound to wisp/scheduler, not wisp/concierge"
- Session created on first message if doesn't exist
- `--new` overwrites existing session with fresh one

---

## Server Changes

### Updated `/chat` endpoint:

```haskell
-- Domain/Chat.hs
data ChatMessage = ChatMessage
  { messageRole    :: Text        -- "user" | "assistant" | "tool"
  , messageContent :: Text
  , messageToolCall :: Maybe Value -- tool call if assistant made one
  }

data ChatRequest = ChatRequest
  { chatAgent    :: Text
  , chatMessages :: [ChatMessage]
  }

data ChatResponse = ChatResponse
  { responseMessage  :: Text
  , responseToolCall :: Maybe Value  -- included so CLI can store it
  }
```

### Handler flow:

1. Parse request, extract agent and messages
2. Validate agent exists via `Agents.Dispatcher`
3. Check agent is implemented, return error if not
4. Dispatch to agent's `handleChat` with full message history
5. Return response (message + tool call if any)

### Agent `handleChat` signature changes:

```haskell
-- Before
handleChat :: Text -> App (Either Text Text)

-- After
handleChat :: [ChatMessage] -> App (Either Text ChatResponse)
```

The agent builds its prompt using the full conversation history, not just the last message.

---

## CLI Changes Summary

### New/modified commands:

| Command | Description |
|---------|-------------|
| `wisp agents` | List all agents with tools, workflows, implemented status |
| `wisp chat -a AGENT -m MSG` | Chat with specified agent |
| `wisp chat -a AGENT -m MSG --session NAME` | Chat using named session |
| `wisp chat -a AGENT --session NAME --new` | Clear session and start fresh |
| `wisp sessions` | List all sessions with agent, last updated |
| `wisp sessions --delete NAME` | Delete a session |

### New CLI types:

```haskell
data Command
  = ...
  | Agents
  | Chat ChatOptions
  | Sessions SessionsOptions

data ChatOptions = ChatOptions
  { chatAgent   :: Text
  , chatMessage :: Maybe Text
  , chatSession :: Text        -- default: "default"
  , chatNew     :: Bool        -- default: False
  }

data SessionsOptions = SessionsOptions
  { sessionsDelete :: Maybe Text
  }
```

### Session directory:

Created on first use at `~/.wisp/sessions/`

---

## File Changes Summary

### New files:

| File | Purpose |
|------|---------|
| `wisp-srv/src/Domain/Agent.hs` | `AgentInfo`, `ToolInfo`, `ToolType` types |
| `wisp-srv/src/Agents/Scheduler.hs` | Stub with `agentInfo`, not implemented |
| `wisp-srv/src/Agents/Housekeeper.hs` | Stub with `agentInfo`, not implemented |
| `wisp-srv/src/Agents/Insights.hs` | Stub with `agentInfo`, not implemented |
| `wisp-srv/src/Agents/Dispatcher.hs` | `dispatchChat`, routes to agent by path |

### Modified files:

| File | Changes |
|------|---------|
| `wisp-srv/src/Agents/Concierge.hs` | Add `agentInfo`, change `handleChat` signature |
| `wisp-srv/src/Domain/Chat.hs` | Add `ChatMessage`, update `ChatRequest`/`ChatResponse` |
| `wisp-srv/src/Http/Handlers/Chat.hs` | Use dispatcher, handle message array |
| `wisp-srv/src/Http/Routes.hs` | Add `GET /agents` |
| `wisp-cli/app/Main.hs` | New commands, session management, updated chat |
| `wisp-srv/wisp-srv.cabal` | Add new modules |
