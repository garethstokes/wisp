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
import Data.Aeson (Value, toJSON, Result(..), fromJSON)
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
    \VALUES (?, ?, ?, (SELECT COALESCE(MAX(event_seq) + 1, 0) FROM run_events WHERE run_id = ?), ?, ?, ?)"
    ( unEntityId eventId'
    , rid
    , parentId
    , rid  -- for the subquery
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
