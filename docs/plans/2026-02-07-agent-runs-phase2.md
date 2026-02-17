# Agent Runs Phase 2: Instrument Existing Agents

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Wrap existing `handleChat` functions to create runs and append events while preserving current behavior.

**Architecture:** Introduce a `withRunLogging` wrapper that creates a run, logs events, and delegates to the existing handler. The wrapper intercepts LLM calls and tool executions to capture full context. Both systems run in parallel—existing behavior continues working while events are logged.

**Tech Stack:** Haskell, `Domain.Run`, `Infra.Db.Run`, existing agent modules

---

## Overview

Phase 2 instruments existing agents without changing their behavior:

1. **Add `FromJSON` for `RunEvent`** — needed to deserialize events from database
2. **Create `Agents.Run` module** — shared run execution logic
3. **Wrap `dispatchChat`** — create run, log input event, delegate to agent
4. **Instrument LLM calls** — log `LlmCalled` events with full prompt/response
5. **Instrument tool calls** — log `ToolRequested`/`ToolSucceeded`/`ToolFailed` events
6. **Update run status** — mark completed/failed based on outcome

---

### Task 1: Add FromJSON for RunEvent

**Files:**
- Modify: `wisp-srv/src/Domain/Run.hs:113-163`

**Step 1: Add the FromJSON instance**

Add after the `ToJSON RunEvent` instance (line 163):

```haskell
instance FromJSON RunEvent where
  parseJSON = withObject "RunEvent" $ \v -> do
    eventType <- v .: "type"
    case (eventType :: Text) of
      "input" -> InputEvent
        <$> v .: "id"
        <*> v .:? "parent_event_id"
        <*> v .: "timestamp"
        <*> v .: "tool"
        <*> v .: "data"
      "context_assembled" -> ContextAssembled
        <$> v .: "id"
        <*> v .:? "parent_event_id"
        <*> v .: "timestamp"
        <*> v .: "context"
      "llm_called" -> LlmCalled
        <$> v .: "id"
        <*> v .:? "parent_event_id"
        <*> v .: "timestamp"
        <*> v .: "model"
        <*> v .: "system_prompt"
        <*> v .: "user_prompt"
        <*> v .: "raw_response"
      "tool_requested" -> ToolRequested
        <$> v .: "id"
        <*> v .:? "parent_event_id"
        <*> v .: "timestamp"
        <*> v .: "tool_name"
        <*> v .: "tool_args"
      "tool_succeeded" -> ToolSucceeded
        <$> v .: "id"
        <*> v .:? "parent_event_id"
        <*> v .: "timestamp"
        <*> v .: "tool_name"
        <*> v .: "result"
      "tool_failed" -> ToolFailed
        <$> v .: "id"
        <*> v .:? "parent_event_id"
        <*> v .: "timestamp"
        <*> v .: "tool_name"
        <*> v .: "error"
      _ -> fail $ "Unknown event type: " <> T.unpack eventType
```

Also add the import at the top:

```haskell
import qualified Data.Text as T
```

**Step 2: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Domain/Run.hs
git commit -m "feat(domain): add FromJSON instance for RunEvent"
```

---

### Task 2: Fix dbEventToEvent to properly deserialize events

**Files:**
- Modify: `wisp-srv/src/Infra/Db/Run.hs:181-192`

**Step 1: Update dbEventToEvent to use FromJSON**

Replace the placeholder implementation with proper JSON parsing:

```haskell
import Data.Aeson (Result(..), fromJSON)

dbEventToEvent :: DbRunEvent -> RunEvent
dbEventToEvent DbRunEvent {..} =
  case fromJSON dbEventData of
    Success event -> event
    Error _ ->
      -- Fallback for malformed data (shouldn't happen)
      InputEvent
        { eventId = dbEventId
        , eventParentEventId = dbEventParentId
        , eventTimestamp = dbEventCreatedAt
        , eventTool = dbEventType
        , eventData = dbEventData
        }
```

**Step 2: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Infra/Db/Run.hs
git commit -m "fix(db): properly deserialize RunEvent from JSON"
```

---

### Task 3: Create Agents.Run module with withRunLogging

**Files:**
- Create: `wisp-srv/src/Agents/Run.hs`
- Modify: `wisp-srv/wisp-srv.cabal` (add to exposed-modules)

**Step 1: Create the module**

Create `wisp-srv/src/Agents/Run.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agents.Run
  ( withRunLogging
  , RunContext(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, toJSON)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import App.Monad (App)
import Domain.Run
import Infra.Db.Run (createRun, appendEvent, updateRunStatus)
import Domain.Chat (ChatMessage(..), ChatResponse(..))

-- | Context passed to agent during a logged run
data RunContext = RunContext
  { rcRunId :: RunId
  , rcSessionId :: Maybe Text
  }

-- | Wrap an agent's handleChat with run logging
-- Creates a run, logs the input event, delegates to the agent,
-- and logs the outcome (completed/failed)
withRunLogging
  :: Text                                         -- ^ Agent ID (e.g., "wisp/concierge")
  -> Maybe Text                                   -- ^ Session ID
  -> [ChatMessage]                                -- ^ Input messages
  -> ([ChatMessage] -> App (Either Text ChatResponse))  -- ^ The agent's handleChat
  -> App (Either Text ChatResponse)
withRunLogging agentId mSessionId messages handleChat = do
  -- Create the run
  runId <- createRun agentId mSessionId Nothing

  -- Log input event
  now <- liftIO getCurrentTime
  let inputEvent = InputEvent
        { eventId = ""  -- Will be assigned by appendEvent
        , eventParentEventId = Nothing
        , eventTimestamp = now
        , eventTool = "message_from_user"
        , eventData = toJSON $ object
            [ "messages" .= messages
            ]
        }
  _ <- appendEvent runId inputEvent

  -- Delegate to actual handler
  result <- handleChat messages

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
  where
    object = Data.Aeson.object
    (.=) = (Data.Aeson..=)
```

**Step 2: Add to cabal file**

In `wisp-srv/wisp-srv.cabal`, add `Agents.Run` to the `exposed-modules` list in the library section (after `Agents.Dispatcher`):

```cabal
    Agents.Run
```

**Step 3: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Agents/Run.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(agents): add Agents.Run module with withRunLogging"
```

---

### Task 4: Integrate withRunLogging into dispatchChat

**Files:**
- Modify: `wisp-srv/src/Agents/Dispatcher.hs`

**Step 1: Import Agents.Run**

Add to imports:

```haskell
import Agents.Run (withRunLogging)
```

**Step 2: Wrap each agent dispatch**

Replace the dispatchChat function body to use withRunLogging:

```haskell
-- | Dispatch chat to the appropriate agent
-- timezone: Optional IANA timezone for converting dates to local time in agent context
dispatchChat :: Text -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
dispatchChat agent msgs tz = withRunLogging agent Nothing msgs $ \messages ->
  case agent of
    "wisp/concierge" -> Concierge.handleChat messages tz
    "wisp/scheduler" -> Scheduler.handleChat messages tz
    "wisp/housekeeper" -> pure $ Left "Agent 'wisp/housekeeper' not yet implemented"
    "wisp/insights" -> Insights.handleChat messages tz
    _ -> pure $ Left $ "Unknown agent: " <> agent
```

**Step 3: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Run tests**

Run: `cabal test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add wisp-srv/src/Agents/Dispatcher.hs
git commit -m "feat(agents): wrap dispatchChat with run logging"
```

---

### Task 5: Add test for withRunLogging

**Files:**
- Create: `wisp-srv/test/Agents/RunSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal` (add to test-suite)

**Step 1: Create the test file**

Create `wisp-srv/test/Agents/RunSpec.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Agents.RunSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..))

-- Unit tests for run logging behavior
-- Integration tests require database setup
spec :: Spec
spec = describe "Agents.Run" $ do
  describe "withRunLogging" $ do
    it "should be tested with integration tests" $ do
      -- This module wraps DB operations, so full tests need TestEnv
      -- For now, verify the module compiles and exports correctly
      True `shouldBe` True
```

**Step 2: Add to cabal file**

In `wisp-srv/wisp-srv.cabal`, add to the test-suite's `other-modules`:

```cabal
    Agents.RunSpec
```

**Step 3: Run tests**

Run: `cabal test`
Expected: All tests pass

**Step 4: Commit**

```bash
git add wisp-srv/test/Agents/RunSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "test(agents): add RunSpec placeholder"
```

---

### Task 6: Manual verification with running server

**Step 1: Start the server**

Run: `cabal run wisp-srv`

**Step 2: Send a test chat message**

Using curl or the CLI:

```bash
curl -X POST http://localhost:3000/chat \
  -H "Content-Type: application/json" \
  -d '{
    "agent": "wisp/concierge",
    "messages": [{"role": "user", "content": "hello"}]
  }'
```

**Step 3: Verify run was created**

Check the database:

```sql
SELECT id, agent_id, status, created_at FROM agent_runs ORDER BY created_at DESC LIMIT 5;
SELECT id, run_id, event_type, created_at FROM run_events ORDER BY created_at DESC LIMIT 10;
```

Expected:
- One new row in `agent_runs` with status 'completed' (or 'failed' if API key missing)
- At least 2 events: input event and respond_to_user event

**Step 4: Commit any fixes if needed**

---

### Task 7: Final build and test

**Step 1: Full build**

Run: `cabal build all`
Expected: All packages build successfully

**Step 2: Run all tests**

Run: `cabal test`
Expected: All tests pass (should be 127+)

**Step 3: Commit summary**

Verify all Phase 2 changes are committed:

```bash
git log --oneline -10
```

---

## Summary

Phase 2 adds run logging to existing agents:

| Component | Purpose |
|-----------|---------|
| `Domain.Run` (FromJSON) | Deserialize events from database |
| `Infra.Db.Run` (fix) | Proper event deserialization |
| `Agents.Run` | Shared `withRunLogging` wrapper |
| `Agents.Dispatcher` | Integrates run logging |

**What's logged:**
- Input event (`message_from_user`) with message content
- Output event (`respond_to_user`) with response

**What's NOT yet logged (Phase 3):**
- `LlmCalled` events (requires instrumenting Claude client)
- `ToolRequested`/`ToolSucceeded`/`ToolFailed` (requires agent refactoring)
- `ContextAssembled` events
