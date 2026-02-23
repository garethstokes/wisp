# Agent Runs Phase 3: Instrument LLM Calls and Tool Execution

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Log `LlmCalled`, `ToolRequested`, `ToolSucceeded`, and `ToolFailed` events for every agent run, capturing full prompts and responses.

**Architecture:** Create an instrumented Claude client (`callClaudeLogged`) that logs `LlmCalled` events, and instrumented tool execution helpers that log tool events. Agents receive a `RunContext` and use these instrumented functions. This avoids rewriting agent logic while capturing all events.

**Tech Stack:** Haskell, `Domain.Run`, `Infra.Db.Run`, `Infra.Claude.Client`, existing agent modules

---

## Overview

Phase 3 adds detailed event logging inside agent execution:

1. **Create `callClaudeLogged`** — wraps `callClaudeWithSystem` to log `LlmCalled` events
2. **Create tool execution helpers** — `logToolRequest`, `logToolSuccess`, `logToolFailure`
3. **Extend `RunContext`** — add helper functions for agents to log events
4. **Update `withRunLogging`** — pass `RunContext` to agent handlers
5. **Update agents** — use instrumented functions (Concierge, Scheduler, Insights)
6. **Add tests** — verify events are logged correctly

---

### Task 1: Add callClaudeLogged to Agents.Run

**Files:**
- Modify: `wisp-srv/src/Agents/Run.hs`

**Step 1: Add imports**

Add to the imports section:

```haskell
import Control.Monad.Reader (asks)
import Data.Aeson (Value)
import App.Config (Config(..), ClaudeConfig(..))
import App.Monad (Env(..))
import Infra.Claude.Client (callClaudeWithSystem)
```

**Step 2: Add callClaudeLogged function**

Add after the `withRunLogging` function:

```haskell
-- | Call Claude with logging - logs LlmCalled event with full prompt/response
callClaudeLogged
  :: RunContext
  -> Text           -- ^ System prompt
  -> Text           -- ^ User prompt
  -> App (Either Text Text)
callClaudeLogged ctx systemPrompt userPrompt = do
  claudeCfg <- asks (claude . config)
  let modelName = model claudeCfg

  -- Call Claude
  result <- liftIO $ callClaudeWithSystem
    (apiKey claudeCfg)
    modelName
    systemPrompt
    userPrompt

  -- Log the LLM call (success or failure)
  now <- liftIO getCurrentTime
  case result of
    Left err -> do
      let event = LlmCalled
            { eventId = ""
            , eventParentEventId = Nothing
            , eventTimestamp = now
            , eventModel = modelName
            , eventSystemPrompt = systemPrompt
            , eventUserPrompt = userPrompt
            , eventRawResponse = "ERROR: " <> err
            }
      _ <- appendEvent (rcRunId ctx) event
      pure $ Left err
    Right response -> do
      let event = LlmCalled
            { eventId = ""
            , eventParentEventId = Nothing
            , eventTimestamp = now
            , eventModel = modelName
            , eventSystemPrompt = systemPrompt
            , eventUserPrompt = userPrompt
            , eventRawResponse = response
            }
      _ <- appendEvent (rcRunId ctx) event
      pure $ Right response
```

**Step 3: Export callClaudeLogged**

Update module exports:

```haskell
module Agents.Run
  ( withRunLogging
  , RunContext(..)
  , callClaudeLogged
  ) where
```

**Step 4: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add wisp-srv/src/Agents/Run.hs
git commit -m "feat(agents): add callClaudeLogged for LLM event logging"
```

---

### Task 2: Add tool logging helpers to Agents.Run

**Files:**
- Modify: `wisp-srv/src/Agents/Run.hs`

**Step 1: Add tool logging functions**

Add after `callClaudeLogged`:

```haskell
-- | Log a tool request event
logToolRequest
  :: RunContext
  -> Text           -- ^ Tool name
  -> Value          -- ^ Tool arguments
  -> App Text       -- ^ Returns event ID for linking
logToolRequest ctx toolName toolArgs = do
  now <- liftIO getCurrentTime
  let event = ToolRequested
        { eventId = ""
        , eventParentEventId = Nothing
        , eventTimestamp = now
        , eventToolName = toolName
        , eventToolArgs = toolArgs
        }
  appendEvent (rcRunId ctx) event

-- | Log a tool success event
logToolSuccess
  :: RunContext
  -> Text           -- ^ Tool name
  -> Value          -- ^ Result
  -> App ()
logToolSuccess ctx toolName result = do
  now <- liftIO getCurrentTime
  let event = ToolSucceeded
        { eventId = ""
        , eventParentEventId = Nothing
        , eventTimestamp = now
        , eventToolName = toolName
        , eventResult = result
        }
  _ <- appendEvent (rcRunId ctx) event
  pure ()

-- | Log a tool failure event
logToolFailure
  :: RunContext
  -> Text           -- ^ Tool name
  -> Text           -- ^ Error message
  -> App ()
logToolFailure ctx toolName errMsg = do
  now <- liftIO getCurrentTime
  let event = ToolFailed
        { eventId = ""
        , eventParentEventId = Nothing
        , eventTimestamp = now
        , eventToolName = toolName
        , eventError = errMsg
        }
  _ <- appendEvent (rcRunId ctx) event
  pure ()
```

**Step 2: Export the new functions**

Update module exports:

```haskell
module Agents.Run
  ( withRunLogging
  , RunContext(..)
  , callClaudeLogged
  , logToolRequest
  , logToolSuccess
  , logToolFailure
  ) where
```

**Step 3: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Agents/Run.hs
git commit -m "feat(agents): add tool logging helpers"
```

---

### Task 3: Update withRunLogging to pass RunContext

**Files:**
- Modify: `wisp-srv/src/Agents/Run.hs`

**Step 1: Change handler signature to receive RunContext**

Update `withRunLogging` to pass `RunContext` to the handler:

```haskell
-- | Wrap an agent's handleChat with run logging
-- Creates a run, logs the input event, delegates to the agent,
-- and logs the outcome (completed/failed)
withRunLogging
  :: Text                                                    -- ^ Agent ID
  -> Maybe Text                                              -- ^ Session ID
  -> [ChatMessage]                                           -- ^ Input messages
  -> (RunContext -> [ChatMessage] -> App (Either Text ChatResponse))  -- ^ Handler receives context
  -> App (Either Text ChatResponse)
withRunLogging agentId mSessionId messages handleChat = do
  -- Create the run
  runId <- createRun agentId mSessionId Nothing

  -- Build run context
  let ctx = RunContext
        { rcRunId = runId
        , rcSessionId = mSessionId
        }

  -- Log input event
  now <- liftIO getCurrentTime
  let inputEvent = InputEvent
        { eventId = ""
        , eventParentEventId = Nothing
        , eventTimestamp = now
        , eventTool = "message_from_user"
        , eventData = toJSON $ object
            [ "messages" .= messages
            ]
        }
  _ <- appendEvent runId inputEvent

  -- Delegate to actual handler with context
  result <- handleChat ctx messages

  -- Log outcome
  case result of
    Left err -> do
      updateRunStatus runId Failed
      pure $ Left err
    Right response -> do
      now' <- liftIO getCurrentTime
      let responseEvent = ToolSucceeded
            { eventId = ""
            , eventParentEventId = Nothing
            , eventTimestamp = now'
            , eventToolName = "respond_to_user"
            , eventResult = toJSON response
            }
      _ <- appendEvent runId responseEvent
      updateRunStatus runId Completed
      pure $ Right response
```

**Step 2: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build FAILS - Dispatcher needs updating (expected)

**Step 3: Commit**

```bash
git add wisp-srv/src/Agents/Run.hs
git commit -m "feat(agents): pass RunContext to handler in withRunLogging"
```

---

### Task 4: Update Dispatcher to pass RunContext (ignore it for now)

**Files:**
- Modify: `wisp-srv/src/Agents/Dispatcher.hs`

**Step 1: Update dispatchChat to accept but ignore RunContext**

The agents don't use RunContext yet, so we'll ignore it in the lambda:

```haskell
-- | Dispatch chat to the appropriate agent
-- timezone: Optional IANA timezone for converting dates to local time in agent context
dispatchChat :: Text -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
dispatchChat agent msgs tz = withRunLogging agent Nothing msgs $ \_ctx messages ->
  case agent of
    "wisp" -> Concierge.handleChat messages tz
    "wisp" -> Scheduler.handleChat messages tz
    "wisp/housekeeper" -> pure $ Left "Agent 'wisp/housekeeper' not yet implemented"
    "wisp" -> Insights.handleChat messages tz
    _ -> pure $ Left $ "Unknown agent: " <> agent
```

Note: Changed `\messages ->` to `\_ctx messages ->` to accept and ignore the context.

**Step 2: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 3: Run tests**

Run: `cabal test wisp-srv-test`
Expected: All tests pass

**Step 4: Commit**

```bash
git add wisp-srv/src/Agents/Dispatcher.hs
git commit -m "feat(agents): update dispatcher for RunContext (ignored for now)"
```

---

### Task 5: Create Concierge.handleChatWithContext

**Files:**
- Modify: `wisp-srv/src/Agents/Concierge.hs`

This task creates a new version of handleChat that uses RunContext for logging. The old handleChat remains for backwards compatibility until all agents are updated.

**Step 1: Add imports**

Add to imports section:

```haskell
import Agents.Run (RunContext(..), callClaudeLogged, logToolRequest, logToolSuccess, logToolFailure)
```

**Step 2: Add handleChatWithContext function**

Add after `handleChat` (around line 324):

```haskell
-- | Handle chat with run context for event logging
handleChatWithContext :: RunContext -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
handleChatWithContext ctx messages mTzName = do
  let userMessages = [m | m <- messages, messageRole m == "user"]
  case userMessages of
    [] -> pure $ Left "No user message provided"
    _ -> do
      -- Load timezone if provided
      mTz <- case mTzName of
        Nothing -> pure Nothing
        Just tzName -> liftIO $ loadTimezone tzName

      -- Assemble context
      calendar <- getTodaysCalendarEvents
      quarantined <- getActivitiesByStatus Activity.Quarantined 100
      surfaced <- getActivitiesByStatus Activity.Surfaced 100
      needsReview <- getActivitiesByStatus Activity.NeedsReview 100

      let systemPrompt = buildChatPrompt mTz calendar quarantined surfaced needsReview
      let conversationPrompt = buildConversationPrompt messages

      -- Call Claude with logging
      result <- callClaudeLogged ctx systemPrompt conversationPrompt

      case result of
        Left err -> pure $ Left err
        Right response -> do
          -- Parse response
          case parseLLMResponse response of
            Left err -> pure $ Left err
            Right llmResp -> do
              -- Execute tool call if present
              case llmToolCall llmResp of
                Nothing -> pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
                Just toolCall -> do
                  toolResult <- executeToolCallLogged ctx toolCall
                  case toolResult of
                    ToolSuccess _ -> pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
                    ToolError err -> pure $ Left err

-- | Execute tool call with logging
executeToolCallLogged :: RunContext -> ConciergeToolCall -> App ToolResult
executeToolCallLogged ctx toolCall = do
  let toolName = case toolCall of
        UpdateActivities {} -> "update_activities"
        QueryActivities {} -> "query_activities"
        QueryPeople {} -> "query_people"

  -- Log tool request
  _ <- logToolRequest ctx toolName (toJSON toolCall)

  -- Execute the tool
  result <- executeToolCall toolCall

  -- Log result
  case result of
    ToolSuccess val -> do
      logToolSuccess ctx toolName val
      pure result
    ToolError err -> do
      logToolFailure ctx toolName err
      pure result
```

**Step 3: Add ToJSON for ConciergeToolCall**

Add after the FromJSON instance (around line 124):

```haskell
instance ToJSON ConciergeToolCall where
  toJSON (UpdateActivities ids updates) = object
    [ "tool" .= ("update_activities" :: Text)
    , "activity_ids" .= ids
    , "updates" .= updates
    ]
  toJSON (QueryActivities filt) = object
    [ "tool" .= ("query_activities" :: Text)
    , "status" .= filterStatus filt
    , "limit" .= filterLimit filt
    , "since" .= filterSince filt
    , "before" .= filterBefore filt
    ]
  toJSON (QueryPeople filt) = object
    [ "tool" .= ("query_people" :: Text)
    , "email" .= peopleEmail filt
    , "search" .= peopleSearch filt
    , "limit" .= peopleLimit filt
    ]

instance ToJSON ActivityUpdates where
  toJSON ActivityUpdates {..} = object
    [ "status" .= updatesStatus
    , "classification" .= updatesClassification
    ]

instance ToJSON PartialClassification where
  toJSON PartialClassification {..} = object
    [ "activity_type" .= partialActivityType
    , "urgency" .= partialUrgency
    , "autonomy_tier" .= partialAutonomyTier
    , "confidence" .= partialConfidence
    , "personas" .= partialPersonas
    , "reasoning" .= partialReasoning
    , "suggested_actions" .= partialSuggestedActions
    , "option_framing" .= partialOptionFraming
    ]

instance ToJSON ActivityFilter where
  toJSON ActivityFilter {..} = object
    [ "status" .= filterStatus
    , "limit" .= filterLimit
    , "since" .= filterSince
    , "before" .= filterBefore
    ]

instance ToJSON PeopleFilter where
  toJSON PeopleFilter {..} = object
    [ "email" .= peopleEmail
    , "search" .= peopleSearch
    , "limit" .= peopleLimit
    ]
```

**Step 4: Export handleChatWithContext**

Update the module exports to include `handleChatWithContext`:

```haskell
module Agents.Concierge
  ( -- Deterministic flows
    classifyAllPending
  , classifyPending
    -- Decision flows
  , handleChat
  , handleChatWithContext
    -- Types
    ...
```

**Step 5: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 6: Commit**

```bash
git add wisp-srv/src/Agents/Concierge.hs
git commit -m "feat(concierge): add handleChatWithContext with event logging"
```

---

### Task 6: Create Scheduler.handleChatWithContext

**Files:**
- Modify: `wisp-srv/src/Agents/Scheduler.hs`

**Step 1: Add imports**

Add to imports section:

```haskell
import Agents.Run (RunContext(..), callClaudeLogged, logToolRequest, logToolSuccess, logToolFailure)
```

**Step 2: Add ToJSON for SchedulerToolCall**

Add after the FromJSON instances:

```haskell
instance ToJSON SchedulerToolCall where
  toJSON (GetAvailability q) = object
    [ "tool" .= ("get_availability" :: Text)
    , "date" .= availabilityDate q
    , "duration_minutes" .= availabilityDuration q
    ]
  toJSON (ListEvents q) = object
    [ "tool" .= ("list_events" :: Text)
    , "date" .= eventsDate q
    , "days" .= eventsDays q
    ]

instance ToJSON AvailabilityQuery where
  toJSON AvailabilityQuery {..} = object
    [ "date" .= availabilityDate
    , "duration_minutes" .= availabilityDuration
    ]

instance ToJSON EventsQuery where
  toJSON EventsQuery {..} = object
    [ "date" .= eventsDate
    , "days" .= eventsDays
    ]
```

**Step 3: Add handleChatWithContext function**

Add after `handleChat`:

```haskell
-- | Handle chat with run context for event logging
handleChatWithContext :: RunContext -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
handleChatWithContext ctx messages mTzName = do
  let userMessages = [m | m <- messages, messageRole m == "user"]
  case userMessages of
    [] -> pure $ Left "No user message provided"
    _ -> do
      mTz <- case mTzName of
        Nothing -> pure Nothing
        Just tzName -> liftIO $ loadTimezone tzName

      -- Get calendar context
      todayEvents <- getTodaysCalendarEvents
      upcomingEvents <- getUpcomingCalendarEvents 7

      let systemPrompt = buildChatPrompt mTz todayEvents upcomingEvents
      let conversationPrompt = buildConversationPrompt messages

      -- Call Claude with logging
      result <- callClaudeLogged ctx systemPrompt conversationPrompt

      case result of
        Left err -> pure $ Left err
        Right response -> do
          case parseLLMResponse response of
            Left err -> pure $ Left err
            Right llmResp -> do
              case llmToolCall llmResp of
                Nothing -> pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
                Just toolCall -> do
                  toolResult <- executeToolCallLogged ctx mTz toolCall
                  case toolResult of
                    ToolSuccess _ -> pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
                    ToolError err -> pure $ Left err

-- | Execute tool call with logging
executeToolCallLogged :: RunContext -> Maybe TZ -> SchedulerToolCall -> App ToolResult
executeToolCallLogged ctx mTz toolCall = do
  let toolName = case toolCall of
        GetAvailability {} -> "get_availability"
        ListEvents {} -> "list_events"

  _ <- logToolRequest ctx toolName (toJSON toolCall)
  result <- executeToolCall mTz toolCall

  case result of
    ToolSuccess val -> do
      logToolSuccess ctx toolName val
      pure result
    ToolError err -> do
      logToolFailure ctx toolName err
      pure result
```

**Step 4: Export handleChatWithContext**

Update module exports:

```haskell
module Agents.Scheduler
  ( -- Decision flows
    handleChat
  , handleChatWithContext
    -- Types
    ...
```

**Step 5: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 6: Commit**

```bash
git add wisp-srv/src/Agents/Scheduler.hs
git commit -m "feat(scheduler): add handleChatWithContext with event logging"
```

---

### Task 7: Create Insights.handleChatWithContext

**Files:**
- Modify: `wisp-srv/src/Agents/Insights.hs`

**Step 1: Add imports**

Add to imports section:

```haskell
import Agents.Run (RunContext(..), callClaudeLogged, logToolRequest, logToolSuccess, logToolFailure)
```

**Step 2: Add ToJSON for InsightsToolCall**

Add after the FromJSON instances:

```haskell
instance ToJSON InsightsToolCall where
  toJSON (SearchActivities q) = object
    [ "tool" .= ("search_activities" :: Text)
    , "query" .= searchTerm q
    , "limit" .= searchLimit q
    ]
  toJSON (GetSummary q) = object
    [ "tool" .= ("get_summary" :: Text)
    , "hours" .= summaryHours q
    ]
  toJSON (GetPeopleInsights q) = object
    [ "tool" .= ("get_people_insights" :: Text)
    , "search" .= Insights.peopleSearch q
    , "important_only" .= peopleImportantOnly q
    ]

instance ToJSON SearchQuery where
  toJSON SearchQuery {..} = object
    [ "query" .= searchTerm
    , "limit" .= searchLimit
    ]

instance ToJSON SummaryQuery where
  toJSON SummaryQuery {..} = object
    [ "hours" .= summaryHours
    ]

instance ToJSON PeopleQuery where
  toJSON PeopleQuery {..} = object
    [ "search" .= Insights.peopleSearch
    , "important_only" .= peopleImportantOnly
    ]
```

Note: Use `Insights.peopleSearch` to disambiguate from the imported `peopleSearch` from `Infra.Db.Person`.

**Step 3: Add handleChatWithContext function**

Add after `handleChat`:

```haskell
-- | Handle chat with run context for event logging
handleChatWithContext :: RunContext -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
handleChatWithContext ctx messages mTzName = do
  let userMessages = [m | m <- messages, messageRole m == "user"]
  case userMessages of
    [] -> pure $ Left "No user message provided"
    _ -> do
      mTz <- case mTzName of
        Nothing -> pure Nothing
        Just tzName -> liftIO $ loadTimezone tzName

      -- Get context for the agent
      recentActivities <- getRecentActivities 24
      stats <- getActivitySummaryStats

      let systemPrompt = buildChatPrompt mTz recentActivities stats
      let conversationPrompt = buildConversationPrompt messages

      -- Call Claude with logging
      result <- callClaudeLogged ctx systemPrompt conversationPrompt

      case result of
        Left err -> pure $ Left err
        Right response -> do
          case parseLLMResponse response of
            Left err -> pure $ Left err
            Right llmResp -> do
              case llmToolCall llmResp of
                Nothing -> pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
                Just toolCall -> do
                  toolResult <- executeToolCallLogged ctx mTz toolCall
                  case toolResult of
                    ToolSuccess _ -> pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
                    ToolError err -> pure $ Left err

-- | Execute tool call with logging
executeToolCallLogged :: RunContext -> Maybe TZ -> InsightsToolCall -> App ToolResult
executeToolCallLogged ctx mTz toolCall = do
  let toolName = case toolCall of
        SearchActivities {} -> "search_activities"
        GetSummary {} -> "get_summary"
        GetPeopleInsights {} -> "get_people_insights"

  _ <- logToolRequest ctx toolName (toJSON toolCall)
  result <- executeToolCall mTz toolCall

  case result of
    ToolSuccess val -> do
      logToolSuccess ctx toolName val
      pure result
    ToolError err -> do
      logToolFailure ctx toolName err
      pure result
```

**Step 4: Export handleChatWithContext**

Update module exports:

```haskell
module Agents.Insights
  ( -- Decision flows
    handleChat
  , handleChatWithContext
    -- Types
    ...
```

**Step 5: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 6: Commit**

```bash
git add wisp-srv/src/Agents/Insights.hs
git commit -m "feat(insights): add handleChatWithContext with event logging"
```

---

### Task 8: Update Dispatcher to use handleChatWithContext

**Files:**
- Modify: `wisp-srv/src/Agents/Dispatcher.hs`

**Step 1: Update dispatchChat to use new handlers**

```haskell
-- | Dispatch chat to the appropriate agent
-- timezone: Optional IANA timezone for converting dates to local time in agent context
dispatchChat :: Text -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
dispatchChat agent msgs tz = withRunLogging agent Nothing msgs $ \ctx messages ->
  case agent of
    "wisp" -> Concierge.handleChatWithContext ctx messages tz
    "wisp" -> Scheduler.handleChatWithContext ctx messages tz
    "wisp/housekeeper" -> pure $ Left "Agent 'wisp/housekeeper' not yet implemented"
    "wisp" -> Insights.handleChatWithContext ctx messages tz
    _ -> pure $ Left $ "Unknown agent: " <> agent
```

Note: Changed from `\_ctx` (ignored) to `\ctx` (used), and from `handleChat` to `handleChatWithContext`.

**Step 2: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 3: Run tests**

Run: `cabal test wisp-srv-test`
Expected: All tests pass

**Step 4: Commit**

```bash
git add wisp-srv/src/Agents/Dispatcher.hs
git commit -m "feat(agents): use handleChatWithContext for full event logging"
```

---

### Task 9: Remove insertConversation from agents

**Files:**
- Modify: `wisp-srv/src/Agents/Concierge.hs`
- Modify: `wisp-srv/src/Agents/Scheduler.hs`
- Modify: `wisp-srv/src/Agents/Insights.hs`

Now that we have full event logging, remove the legacy `insertConversation` calls from the new `handleChatWithContext` functions. The old `handleChat` functions can keep them for backwards compatibility.

**Step 1: Verify insertConversation is NOT in handleChatWithContext**

The `handleChatWithContext` functions we added don't call `insertConversation` - they use `callClaudeLogged` instead. Verify this is the case by checking the code.

**Step 2: Run tests to confirm nothing broke**

Run: `cabal test wisp-srv-test`
Expected: All tests pass

**Step 3: Commit**

```bash
git commit --allow-empty -m "chore: verify insertConversation not used in new handlers"
```

---

### Task 10: Final build and verification

**Step 1: Full build**

Run: `cabal build all`
Expected: All packages build successfully

**Step 2: Run all tests**

Run: `cabal test wisp-srv-test`
Expected: All tests pass

**Step 3: Verify commit history**

Run: `git log --oneline -10`

Expected commits (newest first):
- `feat(agents): use handleChatWithContext for full event logging`
- `feat(insights): add handleChatWithContext with event logging`
- `feat(scheduler): add handleChatWithContext with event logging`
- `feat(concierge): add handleChatWithContext with event logging`
- `feat(agents): update dispatcher for RunContext (ignored for now)`
- `feat(agents): pass RunContext to handler in withRunLogging`
- `feat(agents): add tool logging helpers`
- `feat(agents): add callClaudeLogged for LLM event logging`

---

## Summary

Phase 3 adds detailed event logging:

| Component | Purpose |
|-----------|---------|
| `callClaudeLogged` | Logs `LlmCalled` events with full prompts |
| `logToolRequest/Success/Failure` | Logs tool execution events |
| `RunContext` | Passed to agents for logging |
| `handleChatWithContext` | New handlers using instrumented functions |

**What's now logged (after Phase 3):**
- Input event (`message_from_user`)
- LLM call (`LlmCalled` with system prompt, user prompt, response)
- Tool request (`ToolRequested` with tool name and args)
- Tool result (`ToolSucceeded` or `ToolFailed`)
- Output event (`respond_to_user`)

**Next phases:**
- Phase 4: Add `/runs` API endpoints, update CLI
- Phase 5: Deprecate `/chat`, remove `insertConversation`
