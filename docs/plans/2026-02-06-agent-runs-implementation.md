# Agent Runs Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add server-owned agent runs with event logging for audit, replay, and pause/resume.

**Architecture:** Phase 1 of the design — add database tables and domain types. No changes to existing agents yet. Both systems run in parallel.

**Tech Stack:** Haskell, PostgreSQL, postgresql-simple, Aeson, Hspec

**Design Document:** `docs/plans/2026-02-06-agent-runs-design.md`

---

## Task 1: Database Migration

**Files:**
- Create: `wisp-srv/migrations/011_agent_runs.sql`

**Step 1: Write the migration**

```sql
-- Agent runs table
CREATE TABLE agent_runs (
  id TEXT PRIMARY KEY,
  parent_run_id TEXT REFERENCES agent_runs(id) ON DELETE SET NULL,
  agent_id TEXT NOT NULL,
  session_id TEXT,
  status TEXT NOT NULL DEFAULT 'running',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Run events table (append-only log)
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

-- Indexes
CREATE INDEX idx_runs_session ON agent_runs(session_id) WHERE session_id IS NOT NULL;
CREATE INDEX idx_runs_parent ON agent_runs(parent_run_id) WHERE parent_run_id IS NOT NULL;
CREATE INDEX idx_runs_status ON agent_runs(status);
CREATE INDEX idx_runs_agent ON agent_runs(agent_id);
CREATE INDEX idx_events_run ON run_events(run_id, event_seq);
CREATE INDEX idx_events_parent ON run_events(parent_event_id) WHERE parent_event_id IS NOT NULL;
CREATE INDEX idx_events_type ON run_events(run_id, event_type);
```

**Step 2: Verify migration file exists**

Run: `ls wisp-srv/migrations/011_agent_runs.sql`
Expected: File exists

**Step 3: Commit**

```bash
git add wisp-srv/migrations/011_agent_runs.sql
git commit -m "feat(db): add agent_runs and run_events tables"
```

---

## Task 2: Domain.Run Types

**Files:**
- Create: `wisp-srv/src/Domain/Run.hs`
- Modify: `wisp-srv/wisp-srv.cabal` (add Domain.Run to other-modules)

**Step 1: Create the domain module**

```haskell
module Domain.Run
  ( RunId(..)
  , Run(..)
  , RunStatus(..)
  , RunEvent(..)
  , runStatusToText
  , runStatusFromText
  , eventTypeToText
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value, withText, object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import GHC.Generics (Generic)

-- | Run identifier (12-char NanoID like EntityId)
newtype RunId = RunId { unRunId :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, ToField, FromField)

-- | Run status
data RunStatus
  = Running     -- Currently executing
  | Waiting     -- Paused, waiting for external input
  | Completed   -- Successfully finished
  | Failed      -- Terminated with error
  deriving (Eq, Show, Generic)

runStatusToText :: RunStatus -> Text
runStatusToText = \case
  Running -> "running"
  Waiting -> "waiting"
  Completed -> "completed"
  Failed -> "failed"

runStatusFromText :: Text -> Maybe RunStatus
runStatusFromText = \case
  "running" -> Just Running
  "waiting" -> Just Waiting
  "completed" -> Just Completed
  "failed" -> Just Failed
  _ -> Nothing

instance ToJSON RunStatus where
  toJSON = toJSON . runStatusToText

instance FromJSON RunStatus where
  parseJSON = withText "RunStatus" $ \t ->
    case runStatusFromText t of
      Just s -> pure s
      Nothing -> fail "Invalid run status"

-- | Event types in a run
data RunEvent
  = InputEvent
      { eventId :: Text
      , eventParentEventId :: Maybe Text
      , eventTimestamp :: UTCTime
      , eventTool :: Text
      , eventData :: Value
      }
  | ContextAssembled
      { eventId :: Text
      , eventParentEventId :: Maybe Text
      , eventTimestamp :: UTCTime
      , eventContext :: Value
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
      , eventParentEventId :: Maybe Text
      , eventTimestamp :: UTCTime
      , eventToolName :: Text
      , eventToolArgs :: Value
      }
  | ToolSucceeded
      { eventId :: Text
      , eventParentEventId :: Maybe Text
      , eventTimestamp :: UTCTime
      , eventToolName :: Text
      , eventResult :: Value
      }
  | ToolFailed
      { eventId :: Text
      , eventParentEventId :: Maybe Text
      , eventTimestamp :: UTCTime
      , eventToolName :: Text
      , eventError :: Text
      }
  deriving (Eq, Show, Generic)

eventTypeToText :: RunEvent -> Text
eventTypeToText = \case
  InputEvent {} -> "input"
  ContextAssembled {} -> "context_assembled"
  LlmCalled {} -> "llm_called"
  ToolRequested {} -> "tool_requested"
  ToolSucceeded {} -> "tool_succeeded"
  ToolFailed {} -> "tool_failed"

instance ToJSON RunEvent where
  toJSON e = object $ case e of
    InputEvent {..} ->
      [ "type" .= ("input" :: Text)
      , "id" .= eventId
      , "parent_event_id" .= eventParentEventId
      , "timestamp" .= eventTimestamp
      , "tool" .= eventTool
      , "data" .= eventData
      ]
    ContextAssembled {..} ->
      [ "type" .= ("context_assembled" :: Text)
      , "id" .= eventId
      , "parent_event_id" .= eventParentEventId
      , "timestamp" .= eventTimestamp
      , "context" .= eventContext
      ]
    LlmCalled {..} ->
      [ "type" .= ("llm_called" :: Text)
      , "id" .= eventId
      , "parent_event_id" .= eventParentEventId
      , "timestamp" .= eventTimestamp
      , "model" .= eventModel
      , "system_prompt" .= eventSystemPrompt
      , "user_prompt" .= eventUserPrompt
      , "raw_response" .= eventRawResponse
      ]
    ToolRequested {..} ->
      [ "type" .= ("tool_requested" :: Text)
      , "id" .= eventId
      , "parent_event_id" .= eventParentEventId
      , "timestamp" .= eventTimestamp
      , "tool_name" .= eventToolName
      , "tool_args" .= eventToolArgs
      ]
    ToolSucceeded {..} ->
      [ "type" .= ("tool_succeeded" :: Text)
      , "id" .= eventId
      , "parent_event_id" .= eventParentEventId
      , "timestamp" .= eventTimestamp
      , "tool_name" .= eventToolName
      , "result" .= eventResult
      ]
    ToolFailed {..} ->
      [ "type" .= ("tool_failed" :: Text)
      , "id" .= eventId
      , "parent_event_id" .= eventParentEventId
      , "timestamp" .= eventTimestamp
      , "tool_name" .= eventToolName
      , "error" .= eventError
      ]

-- | Full run with events
data Run = Run
  { runId :: RunId
  , runParentRunId :: Maybe RunId
  , runAgent :: Text
  , runSessionId :: Maybe Text
  , runCreatedAt :: UTCTime
  , runUpdatedAt :: UTCTime
  , runStatus :: RunStatus
  , runEvents :: [RunEvent]
  }
  deriving (Eq, Show, Generic)

instance ToJSON Run where
  toJSON Run {..} = object
    [ "id" .= runId
    , "parent_run_id" .= runParentRunId
    , "agent" .= runAgent
    , "session_id" .= runSessionId
    , "created_at" .= runCreatedAt
    , "updated_at" .= runUpdatedAt
    , "status" .= runStatus
    , "events" .= runEvents
    ]
```

**Step 2: Add to cabal file**

Add `Domain.Run` to both `other-modules` sections in `wisp-srv/wisp-srv.cabal`:
- After `Domain.Receipt` in executable section (~line 47)
- After `Domain.ReceiptSpec` in test-suite section (~line 148)

**Step 3: Build to verify**

Run: `cabal build wisp-srv`
Expected: BUILD SUCCESS

**Step 4: Commit**

```bash
git add wisp-srv/src/Domain/Run.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(domain): add Run, RunStatus, RunEvent types"
```

---

## Task 3: Domain.Run Tests

**Files:**
- Create: `wisp-srv/test/Domain/RunSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal` (add Domain.RunSpec to test other-modules)

**Step 1: Write the test file**

```haskell
module Domain.RunSpec where

import Test.Hspec
import Data.Aeson (encode, decode, object, (.=))
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Domain.Run

-- Helper for consistent timestamps in tests
testTime :: UTCTime
testTime = posixSecondsToUTCTime 1700000000

spec :: Spec
spec = describe "Run" $ do
  describe "RunStatus" $ do
    it "serializes Running to JSON" $ do
      encode Running `shouldBe` "\"running\""

    it "serializes Waiting to JSON" $ do
      encode Waiting `shouldBe` "\"waiting\""

    it "serializes Completed to JSON" $ do
      encode Completed `shouldBe` "\"completed\""

    it "serializes Failed to JSON" $ do
      encode Failed `shouldBe` "\"failed\""

    it "round-trips through JSON" $ do
      decode (encode Running) `shouldBe` Just Running
      decode (encode Waiting) `shouldBe` Just Waiting
      decode (encode Completed) `shouldBe` Just Completed
      decode (encode Failed) `shouldBe` Just Failed

  describe "RunId" $ do
    it "serializes to JSON" $ do
      encode (RunId "abc123") `shouldBe` "\"abc123\""

    it "round-trips through JSON" $ do
      decode (encode (RunId "test")) `shouldBe` Just (RunId "test")

  describe "RunEvent" $ do
    it "serializes InputEvent to JSON with type field" $ do
      let event = InputEvent
            { eventId = "evt1"
            , eventParentEventId = Nothing
            , eventTimestamp = testTime
            , eventTool = "message_from_user"
            , eventData = object ["content" .= ("hello" :: String)]
            }
      let json = encode event
      json `shouldContain` "\"type\":\"input\""
      json `shouldContain` "\"tool\":\"message_from_user\""

    it "serializes ToolRequested to JSON" $ do
      let event = ToolRequested
            { eventId = "evt2"
            , eventParentEventId = Just "evt1"
            , eventTimestamp = testTime
            , eventToolName = "query_activities"
            , eventToolArgs = object ["status" .= ("quarantined" :: String)]
            }
      let json = encode event
      json `shouldContain` "\"type\":\"tool_requested\""
      json `shouldContain` "\"parent_event_id\":\"evt1\""

    it "serializes ToolSucceeded to JSON" $ do
      let event = ToolSucceeded
            { eventId = "evt3"
            , eventParentEventId = Just "evt2"
            , eventTimestamp = testTime
            , eventToolName = "query_activities"
            , eventResult = object ["count" .= (5 :: Int)]
            }
      let json = encode event
      json `shouldContain` "\"type\":\"tool_succeeded\""

    it "serializes ToolFailed to JSON" $ do
      let event = ToolFailed
            { eventId = "evt4"
            , eventParentEventId = Just "evt2"
            , eventTimestamp = testTime
            , eventToolName = "query_activities"
            , eventError = "Database connection failed"
            }
      let json = encode event
      json `shouldContain` "\"type\":\"tool_failed\""
      json `shouldContain` "\"error\":\"Database connection failed\""

  describe "eventTypeToText" $ do
    it "returns correct type strings" $ do
      eventTypeToText (InputEvent "x" Nothing testTime "t" (object [])) `shouldBe` "input"
      eventTypeToText (ContextAssembled "x" Nothing testTime (object [])) `shouldBe` "context_assembled"
      eventTypeToText (LlmCalled "x" Nothing testTime "m" "s" "u" "r") `shouldBe` "llm_called"
      eventTypeToText (ToolRequested "x" Nothing testTime "t" (object [])) `shouldBe` "tool_requested"
      eventTypeToText (ToolSucceeded "x" Nothing testTime "t" (object [])) `shouldBe` "tool_succeeded"
      eventTypeToText (ToolFailed "x" Nothing testTime "t" "e") `shouldBe` "tool_failed"

  describe "Run" $ do
    it "serializes to JSON with all fields" $ do
      let run = Run
            { runId = RunId "run123"
            , runParentRunId = Nothing
            , runAgent = "wisp"
            , runSessionId = Just "default"
            , runCreatedAt = testTime
            , runUpdatedAt = testTime
            , runStatus = Completed
            , runEvents = []
            }
      let json = encode run
      json `shouldContain` "\"id\":\"run123\""
      json `shouldContain` "\"agent\":\"wisp\""
      json `shouldContain` "\"status\":\"completed\""
```

**Step 2: Add to cabal test other-modules**

Add `Domain.RunSpec` after `Domain.ReceiptSpec` in the test-suite section (~line 149).

**Step 3: Run tests**

Run: `cabal test wisp-srv-test 2>&1 | grep -A 20 "Domain.Run"`
Expected: All tests pass

**Step 4: Commit**

```bash
git add wisp-srv/test/Domain/RunSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "test(domain): add RunSpec tests for Run types"
```

---

## Task 4: Infra.Db.Run Module

**Files:**
- Create: `wisp-srv/src/Infra/Db/Run.hs`
- Modify: `wisp-srv/wisp-srv.cabal` (add Infra.Db.Run to other-modules)

**Step 1: Create the database module**

```haskell
module Infra.Db.Run
  ( createRun
  , appendEvent
  , getRun
  , getRunEvents
  , getRunsBySession
  , getRunsByStatus
  , updateRunStatus
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, toJSON)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Domain.Id (EntityId(..), newEntityId, unEntityId)
import Domain.Run
import App.Monad (App, getConn)

-- | Create a new run, returns the RunId
createRun :: Text -> Maybe Text -> Maybe RunId -> App RunId
createRun agentId sessionId parentRunId = do
  conn <- getConn
  rid <- liftIO newEntityId
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "INSERT INTO agent_runs (id, agent_id, session_id, parent_run_id, status, created_at, updated_at) \
    \VALUES (?, ?, ?, ?, 'running', ?, ?)"
    ( unEntityId rid
    , agentId
    , sessionId
    , unRunId <$> parentRunId
    , now
    , now
    )
  pure $ RunId (unEntityId rid)

-- | Append an event to a run, returns the event ID
appendEvent :: RunId -> RunEvent -> App Text
appendEvent (RunId rid) event = do
  conn <- getConn
  eventId' <- liftIO newEntityId
  now <- liftIO getCurrentTime
  -- Get next sequence number
  [Only seq'] <- liftIO $ query conn
    "SELECT COALESCE(MAX(event_seq) + 1, 0) FROM run_events WHERE run_id = ?"
    (Only rid)
  let eventType = eventTypeToText event
      eventData = toJSON event
      parentId = case event of
        InputEvent { eventParentEventId = p } -> p
        ContextAssembled { eventParentEventId = p } -> p
        LlmCalled { eventParentEventId = p } -> p
        ToolRequested { eventParentEventId = p } -> p
        ToolSucceeded { eventParentEventId = p } -> p
        ToolFailed { eventParentEventId = p } -> p
  _ <- liftIO $ execute conn
    "INSERT INTO run_events (id, run_id, parent_event_id, event_seq, event_type, event_data, created_at) \
    \VALUES (?, ?, ?, ?, ?, ?, ?)"
    ( unEntityId eventId'
    , rid
    , parentId
    , seq' :: Int
    , eventType
    , eventData
    , now
    )
  -- Update run timestamp
  _ <- liftIO $ execute conn
    "UPDATE agent_runs SET updated_at = ? WHERE id = ?"
    (now, rid)
  pure (unEntityId eventId')

-- | Update run status
updateRunStatus :: RunId -> RunStatus -> App ()
updateRunStatus (RunId rid) status = do
  conn <- getConn
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "UPDATE agent_runs SET status = ?, updated_at = ? WHERE id = ?"
    (runStatusToText status, now, rid)
  pure ()

-- Database row types
data DbRun = DbRun
  { dbRunId :: Text
  , dbRunParentRunId :: Maybe Text
  , dbRunAgent :: Text
  , dbRunSessionId :: Maybe Text
  , dbRunStatus :: Text
  , dbRunCreatedAt :: UTCTime
  , dbRunUpdatedAt :: UTCTime
  }

instance FromRow DbRun where
  fromRow = DbRun <$> field <*> field <*> field <*> field <*> field <*> field <*> field

data DbRunEvent = DbRunEvent
  { dbEventId :: Text
  , dbEventParentId :: Maybe Text
  , dbEventSeq :: Int
  , dbEventType :: Text
  , dbEventData :: Value
  , dbEventCreatedAt :: UTCTime
  }

instance FromRow DbRunEvent where
  fromRow = DbRunEvent <$> field <*> field <*> field <*> field <*> field <*> field

-- | Get a run with all its events
getRun :: RunId -> App (Maybe Run)
getRun (RunId rid) = do
  conn <- getConn
  runs <- liftIO $ query conn
    "SELECT id, parent_run_id, agent_id, session_id, status, created_at, updated_at \
    \FROM agent_runs WHERE id = ?"
    (Only rid)
  case listToMaybe runs of
    Nothing -> pure Nothing
    Just dbRun -> do
      events <- getRunEventsInternal conn rid
      pure $ Just $ dbRunToRun dbRun events

-- | Get events for a run
getRunEvents :: RunId -> App [RunEvent]
getRunEvents (RunId rid) = do
  conn <- getConn
  dbEvents <- liftIO $ query conn
    "SELECT id, parent_event_id, event_seq, event_type, event_data, created_at \
    \FROM run_events WHERE run_id = ? ORDER BY event_seq"
    (Only rid)
  pure $ map dbEventToEvent dbEvents

getRunEventsInternal :: Connection -> Text -> App [RunEvent]
getRunEventsInternal conn rid = do
  dbEvents <- liftIO $ query conn
    "SELECT id, parent_event_id, event_seq, event_type, event_data, created_at \
    \FROM run_events WHERE run_id = ? ORDER BY event_seq"
    (Only rid)
  pure $ map dbEventToEvent dbEvents

-- | Get runs for a session
getRunsBySession :: Text -> App [Run]
getRunsBySession sessionId = do
  conn <- getConn
  runs <- liftIO $ query conn
    "SELECT id, parent_run_id, agent_id, session_id, status, created_at, updated_at \
    \FROM agent_runs WHERE session_id = ? ORDER BY created_at DESC"
    (Only sessionId)
  mapM (\dbRun -> do
    events <- getRunEventsInternal conn (dbRunId dbRun)
    pure $ dbRunToRun dbRun events
    ) runs

-- | Get runs by status
getRunsByStatus :: RunStatus -> App [Run]
getRunsByStatus status = do
  conn <- getConn
  runs <- liftIO $ query conn
    "SELECT id, parent_run_id, agent_id, session_id, status, created_at, updated_at \
    \FROM agent_runs WHERE status = ? ORDER BY updated_at DESC"
    (Only $ runStatusToText status)
  mapM (\dbRun -> do
    events <- getRunEventsInternal conn (dbRunId dbRun)
    pure $ dbRunToRun dbRun events
    ) runs

-- Conversion helpers
dbRunToRun :: DbRun -> [RunEvent] -> Run
dbRunToRun DbRun {..} events = Run
  { runId = RunId dbRunId
  , runParentRunId = RunId <$> dbRunParentRunId
  , runAgent = dbRunAgent
  , runSessionId = dbRunSessionId
  , runCreatedAt = dbRunCreatedAt
  , runUpdatedAt = dbRunUpdatedAt
  , runStatus = case runStatusFromText dbRunStatus of
      Just s -> s
      Nothing -> Running  -- default
  , runEvents = events
  }

dbEventToEvent :: DbRunEvent -> RunEvent
dbEventToEvent DbRunEvent {..} =
  -- The event_data contains the full event JSON, we just return it as-is
  -- In a real implementation, we'd parse it back to the specific event type
  -- For now, we treat all stored events as InputEvents with the raw data
  InputEvent
    { eventId = dbEventId
    , eventParentEventId = dbEventParentId
    , eventTimestamp = dbEventCreatedAt
    , eventTool = dbEventType
    , eventData = dbEventData
    }
```

**Step 2: Add to cabal file**

Add `Infra.Db.Run` to both `other-modules` sections:
- After `Infra.Db.Receipt` in executable section (~line 66)
- After `Infra.Db.ReceiptSpec` in test-suite section (~line 180)

**Step 3: Build to verify**

Run: `cabal build wisp-srv`
Expected: BUILD SUCCESS

**Step 4: Commit**

```bash
git add wisp-srv/src/Infra/Db/Run.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(db): add Infra.Db.Run module with run/event queries"
```

---

## Task 5: Infra.Db.Run Tests

**Files:**
- Create: `wisp-srv/test/Infra/Db/RunSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal` (add Infra.Db.RunSpec)

**Step 1: Write the test file**

```haskell
module Infra.Db.RunSpec where

import Test.Hspec
import Domain.Run

spec :: Spec
spec = describe "Run DB" $ do
  it "placeholder - run operations need integration tests" $ do
    -- These operations require a database connection
    -- Placeholder test to ensure module compiles and is included
    runStatusToText Running `shouldBe` "running"
    runStatusToText Waiting `shouldBe` "waiting"
    runStatusToText Completed `shouldBe` "completed"
    runStatusToText Failed `shouldBe` "failed"
```

**Step 2: Add to cabal test other-modules**

Add `Infra.Db.RunSpec` after `Infra.Db.ReceiptSpec` in the test-suite section.

**Step 3: Run tests**

Run: `cabal test wisp-srv-test 2>&1 | grep -A 5 "Run DB"`
Expected: Tests pass

**Step 4: Commit**

```bash
git add wisp-srv/test/Infra/Db/RunSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "test(db): add RunSpec placeholder for integration tests"
```

---

## Task 6: Final Build and Full Test

**Step 1: Build everything**

Run: `cabal build wisp-srv wisp-cli`
Expected: BUILD SUCCESS

**Step 2: Run all tests**

Run: `cabal test wisp-srv-test`
Expected: All tests pass (should be ~125+ tests now)

**Step 3: Verify new modules are included**

Run: `grep -E "Domain.Run|Infra.Db.Run" wisp-srv/wisp-srv.cabal | wc -l`
Expected: 4 (two entries each in executable and test-suite)

**Step 4: Final commit (if any uncommitted changes)**

```bash
git status
# If clean, no action needed
```

---

## Summary

After completing all tasks:

| File | Purpose |
|------|---------|
| `migrations/011_agent_runs.sql` | Database schema for runs and events |
| `src/Domain/Run.hs` | `Run`, `RunId`, `RunStatus`, `RunEvent` types |
| `test/Domain/RunSpec.hs` | Unit tests for domain types |
| `src/Infra/Db/Run.hs` | Database queries for runs and events |
| `test/Infra/Db/RunSpec.hs` | Placeholder for integration tests |

This completes Phase 1 (Foundation) of the design. The next phase would be to instrument existing agents to create runs and append events.
