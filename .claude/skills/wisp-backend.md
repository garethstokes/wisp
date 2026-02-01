# Wisp Backend Development Skill

## Overview

This skill provides coding guidelines for wisp-srv, a Haskell backend following the Simple Haskell style. Use this when implementing any backend component.

---

## Style Constraints

### Haskell Subset

We use a deliberately constrained subset of Haskell:

| Allowed | Not Allowed |
|---------|-------------|
| `ReaderT Env IO` as app monad | Monad transformer stacks |
| `Either` / `ExceptT` at boundaries | Custom effect systems |
| Simple ADTs | GADTs, TypeFamilies |
| `DeriveGeneric` | TemplateHaskell |
| Pattern matching | Lens library |

### Required Extensions

Every module should include:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
```

### Libraries

| Purpose | Library | Notes |
|---------|---------|-------|
| HTTP server | `scotty` | Minimal routing, no type-level API |
| DB access | `postgresql-simple` | Direct SQL, no ORM |
| JSON | `aeson` | Derive via Generic |
| Config | `yaml` | With env var overrides |
| Concurrency | `async` | For worker threads |
| Logging | `fast-logger` | Simple structured logging |
| HTTP client | `http-client` + `http-client-tls` | For Google/LLM APIs |
| IDs | `nanoid` | 12-char URL-safe IDs |
| Time | `time` | For timestamps |
| Multiline text | `neat-interpolation` | Nix-style `[trimming|...|]` blocks |

---

## Code Style

### Avoid Deep Nesting

Avoid deeply nested branching logic inside functions. Prefer small functions that compose.

```haskell
-- BAD: Deeply nested
processJob :: Job -> App WorkerResult
processJob job = do
  mactivity <- fetchEntity "activity" job.activityId
  case mactivity of
    Nothing -> pure $ WorkerFailed "Activity not found"
    Just (version, activity) -> do
      case activity.classification of
        Nothing -> pure $ WorkerFailed "Not classified"
        Just classification -> do
          cfg <- getConfig
          let newStatus = routeToStatus cfg.confidenceThreshold classification
          let updated = activity { status = newStatus }
          success <- updateEntity "activity" job.activityId version updated
          if success
            then do
              logReceipt job.activityId newStatus classification
              pure WorkerSuccess
            else pure $ WorkerRetry "Concurrent modification"

-- GOOD: Small composable functions
processJob :: Job -> App WorkerResult
processJob job = do
  result <- runExceptT $ do
    (version, activity) <- fetchActivityOrFail job.activityId
    classification <- getClassificationOrFail activity
    newStatus <- determineStatus classification
    updateActivityStatus job.activityId version activity newStatus
  pure $ either id (const WorkerSuccess) result

fetchActivityOrFail :: EntityId -> ExceptT WorkerResult App (Int, Activity)
fetchActivityOrFail aid = do
  mactivity <- lift $ fetchEntity "activity" aid
  maybe (throwError $ WorkerFailed "Activity not found") pure mactivity

getClassificationOrFail :: Activity -> ExceptT WorkerResult App Classification
getClassificationOrFail activity =
  maybe (throwError $ WorkerFailed "Not classified") pure activity.classification

determineStatus :: Classification -> ExceptT WorkerResult App Status
determineStatus classification = do
  cfg <- lift getConfig
  pure $ routeToStatus cfg.confidenceThreshold classification

updateActivityStatus :: EntityId -> Int -> Activity -> Status -> ExceptT WorkerResult App ()
updateActivityStatus aid version activity newStatus = do
  let updated = activity { status = newStatus }
  success <- lift $ updateEntity "activity" aid version updated
  unless success $ throwError $ WorkerRetry "Concurrent modification"
  lift $ logReceipt aid newStatus (fromJust activity.classification)
```

### Multiline Text with neat-interpolation

Use `neat-interpolation` for multiline strings. It strips leading whitespace intelligently (like Nix) and supports `${var}` interpolation.

```haskell
{-# LANGUAGE QuasiQuotes #-}

import NeatInterpolation (trimming)

-- Simple multiline text
sqlQuery :: Query
sqlQuery = [trimming|
  select entity_id, entity_version, payload
  from entities
  where entity_type = ?
  order by entity_version desc
  limit 1
|]

-- With interpolation
buildPrompt :: Text -> Text -> Text
buildPrompt emailContent senderName = [trimming|
  Classify this email for a personal assistant system.

  Sender: ${senderName}

  Email content:
  ${emailContent}

  Return JSON with classification fields.
|]
```

---

## Module Layout

```
wisp-srv/
  src/
    Domain/
      Id.hs              -- EntityId newtype
      Activity.hs        -- Activity type + JSON
      Person.hs          -- Person type + JSON
      Classification.hs  -- Classification types
      Job.hs             -- Job payload types
      Schema.hs          -- Schema versioning helpers
    Infra/
      Db/
        Connection.hs    -- Pool setup
        Entity.hs        -- Generic entity CRUD
        Jobs.hs          -- Job queue operations
        Receipts.hs      -- Audit log
        Auth.hs          -- OAuth token storage
        PollState.hs     -- Polling cursors
      Google/
        Auth.hs          -- OAuth flow
        Gmail.hs         -- Gmail API client
        Calendar.hs      -- Calendar API client
      Llm/
        Client.hs        -- Claude/OpenAI client
        Prompts.hs       -- Classification prompts
    App/
      Env.hs             -- Config + Env types
      Monad.hs           -- App type alias
      Poller.hs          -- Polling loop
      Workers.hs         -- Worker loop
      Workers/
        ProcessEmail.hs
        ProcessCalendar.hs
        Classify.hs
        Route.hs
      Chat.hs            -- Conversational agent
    Http/
      Server.hs          -- Scotty app setup
      Routes.hs          -- Route definitions
      Handlers/
        Health.hs
        Auth.hs
        Activities.hs
        People.hs
        Quarantine.hs
        Schedule.hs
        Chat.hs
    Main.hs              -- Entry point
  migrations/
    001_entities.sql
    002_jobs.sql
    003_receipts.sql
    004_auth.sql
    005_poll_state.sql
```

---

## EntityId

All entities use a 12-character Nanoid instead of UUID:

```haskell
-- Domain/Id.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Id where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.FromField (FromField)
import Nanoid (nanoid)

newtype EntityId = EntityId { unEntityId :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, ToField, FromField)

-- Generate a new 12-character EntityId
newEntityId :: IO EntityId
newEntityId = EntityId <$> nanoid 12
```

---

## Application Monad

All effectful code runs in `App`:

```haskell
-- App/Monad.hs
{-# LANGUAGE OverloadedStrings #-}

module App.Monad where

import Control.Monad.Reader

type App a = ReaderT Env IO a

runApp :: Env -> App a -> IO a
runApp = flip runReaderT
```

### Environment

```haskell
-- App/Env.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module App.Env where

import Database.PostgreSQL.Simple (Connection)
import System.Log.FastLogger (LoggerSet)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Data.Text (Text)

data Config = Config
  { serverHost :: Text
  , serverPort :: Int
  , dbUrl :: Text
  , googleClientId :: Text
  , googleClientSecret :: Text
  , pollingIntervalMinutes :: Int
  , confidenceThreshold :: Double
  } deriving (Generic, Show)

instance FromJSON Config

data Env = Env
  { config :: Config
  , dbConn :: Connection
  , logger :: LoggerSet
  }

-- Access helpers
getConfig :: App Config
getConfig = asks (.config)

getConn :: App Connection
getConn = asks (.dbConn)
```

---

## Persistence: Versioned JSON Entities

### Core Concept

Domain entities (Activity, Person) are stored in a single `entities` table as versioned JSON documents. Each update creates a new version (append-only). Version numbers are monotonically incremented per entity using a PostgreSQL trigger.

### Schema

```sql
-- migrations/001_entities.sql

create table entities (
  entity_type text not null,
  entity_id text not null,
  entity_version int not null,
  created_at timestamptz not null default now(),
  payload jsonb not null,
  primary key (entity_type, entity_id, entity_version)
);

create index entities_latest
  on entities (entity_type, entity_id, entity_version desc);

-- Schema name in JSON must match entity_type
alter table entities
add constraint entities_schema_name_matches
check ((payload #>> '{schema,name}') = entity_type);

-- Trigger function to auto-increment version per entity
create or replace function next_entity_version()
returns trigger as $$
begin
  select coalesce(max(entity_version), 0) + 1
  into new.entity_version
  from entities
  where entity_type = new.entity_type
    and entity_id = new.entity_id;
  return new;
end;
$$ language plpgsql;

create trigger entities_version_trigger
before insert on entities
for each row
when (new.entity_version is null)
execute function next_entity_version();
```

### JSON Document Format

Every entity payload includes schema metadata:

```json
{
  "schema": {
    "name": "activity",
    "version": 1
  },
  "id": "V1StGXR8_Z5j",
  ...rest of fields
}
```

### Entity Operations

```haskell
-- Infra/Db/Entity.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Infra.Db.Entity where

import Database.PostgreSQL.Simple
import Data.Aeson (ToJSON, FromJSON, toJSON, decode)
import Domain.Id (EntityId(..))
import App.Monad (App, getConn)
import Control.Monad.IO.Class (liftIO)

-- Insert new version of entity (version auto-assigned by trigger)
insertEntity :: (ToJSON a) => Text -> EntityId -> a -> App ()
insertEntity entityType entityId payload = do
  conn <- getConn
  liftIO $ execute conn
    "insert into entities (entity_type, entity_id, payload) \
    \values (?, ?, ?)"
    (entityType, entityId.unEntityId, toJSON payload)

-- Insert with explicit version (for optimistic concurrency)
insertEntityVersion :: (ToJSON a) => Text -> EntityId -> Int -> a -> App Bool
insertEntityVersion entityType entityId version payload = do
  conn <- getConn
  n <- liftIO $ execute conn
    "insert into entities (entity_type, entity_id, entity_version, payload) \
    \values (?, ?, ?, ?) \
    \on conflict do nothing"
    (entityType, entityId.unEntityId, version, toJSON payload)
  pure (n > 0)

-- Fetch latest version
fetchEntity :: (FromJSON a) => Text -> EntityId -> App (Maybe (Int, a))
fetchEntity entityType entityId = do
  conn <- getConn
  results <- liftIO $ query conn
    "select entity_version, payload from entities \
    \where entity_type = ? and entity_id = ? \
    \order by entity_version desc limit 1"
    (entityType, entityId.unEntityId)
  pure $ case results of
    [(v, p)] -> (,) v <$> decode p
    _ -> Nothing

-- Fetch all latest versions of a type
fetchAllEntities :: (FromJSON a) => Text -> App [(EntityId, Int, a)]
fetchAllEntities entityType = do
  conn <- getConn
  results <- liftIO $ query conn
    "select distinct on (entity_id) entity_id, entity_version, payload \
    \from entities where entity_type = ? \
    \order by entity_id, entity_version desc"
    (Only entityType)
  pure [(EntityId eid, v, a) | (eid, v, p) <- results, Just a <- [decode p]]

-- Fetch with filter on payload
fetchEntitiesWhere :: (FromJSON a) => Text -> Text -> App [(EntityId, Int, a)]
fetchEntitiesWhere entityType whereClause = do
  conn <- getConn
  results <- liftIO $ query conn
    (Query $ "select distinct on (entity_id) entity_id, entity_version, payload \
             \from entities where entity_type = ? and " <> encodeUtf8 whereClause <> " \
             \order by entity_id, entity_version desc")
    (Only entityType)
  pure [(EntityId eid, v, a) | (eid, v, p) <- results, Just a <- [decode p]]

-- Optimistic update: insert expected version + 1
updateEntity :: (ToJSON a) => Text -> EntityId -> Int -> a -> App Bool
updateEntity entityType entityId expectedVersion payload =
  insertEntityVersion entityType entityId (expectedVersion + 1) payload
```

---

## Domain Types

### Schema Helper

```haskell
-- Domain/Schema.hs
{-# LANGUAGE DeriveGeneric #-}

module Domain.Schema where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Schema = Schema
  { name :: Text
  , version :: Int
  } deriving (Generic, Show, Eq)

instance ToJSON Schema
instance FromJSON Schema
```

### Activity

```haskell
-- Domain/Activity.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Domain.Activity where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Domain.Id (EntityId)
import Domain.Schema (Schema(..))

data Activity = Activity
  { schema :: Schema
  , id :: EntityId
  , source :: Source
  , sourceId :: Text
  , raw :: Value
  , classification :: Maybe Classification
  , content :: Maybe Content
  , status :: Status
  , personId :: Maybe EntityId
  , calendar :: Maybe CalendarInfo
  } deriving (Generic, Show)

instance ToJSON Activity
instance FromJSON Activity

data Source = Email | Calendar | Conversation | Manual
  deriving (Generic, Show, Eq)

instance ToJSON Source where
  toJSON = \case
    Email -> "email"
    Calendar -> "calendar"
    Conversation -> "conversation"
    Manual -> "manual"

instance FromJSON Source where
  parseJSON = withText "Source" $ \case
    "email" -> pure Email
    "calendar" -> pure Calendar
    "conversation" -> pure Conversation
    "manual" -> pure Manual
    _ -> fail "Invalid source"

data Status
  = Pending
  | Quarantined
  | Processed
  | Surfaced
  | PendingReview
  | Archived
  deriving (Generic, Show, Eq)

instance ToJSON Status where
  toJSON = \case
    Pending -> "pending"
    Quarantined -> "quarantined"
    Processed -> "processed"
    Surfaced -> "surfaced"
    PendingReview -> "pending_review"
    Archived -> "archived"

instance FromJSON Status where
  parseJSON = withText "Status" $ \case
    "pending" -> pure Pending
    "quarantined" -> pure Quarantined
    "processed" -> pure Processed
    "surfaced" -> pure Surfaced
    "pending_review" -> pure PendingReview
    "archived" -> pure Archived
    _ -> fail "Invalid status"

data Classification = Classification
  { personas :: [Persona]
  , activityType :: ActivityType
  , urgency :: Urgency
  , autonomyTier :: Int
  , confidence :: Double
  } deriving (Generic, Show)

instance ToJSON Classification
instance FromJSON Classification

data Persona = Work | Home | Personal
  deriving (Generic, Show, Eq)

instance ToJSON Persona where
  toJSON = \case
    Work -> "work"
    Home -> "home"
    Personal -> "personal"

instance FromJSON Persona where
  parseJSON = withText "Persona" $ \case
    "work" -> pure Work
    "home" -> pure Home
    "personal" -> pure Personal
    _ -> fail "Invalid persona"

data ActivityType
  = Request
  | Information
  | ActionRequired
  | Fyi
  | Event
  deriving (Generic, Show, Eq)

instance ToJSON ActivityType where
  toJSON = \case
    Request -> "request"
    Information -> "information"
    ActionRequired -> "action_required"
    Fyi -> "fyi"
    Event -> "event"

instance FromJSON ActivityType where
  parseJSON = withText "ActivityType" $ \case
    "request" -> pure Request
    "information" -> pure Information
    "action_required" -> pure ActionRequired
    "fyi" -> pure Fyi
    "event" -> pure Event
    _ -> fail "Invalid activity type"

data Urgency = High | Normal | Low
  deriving (Generic, Show, Eq)

instance ToJSON Urgency where
  toJSON = \case
    High -> "high"
    Normal -> "normal"
    Low -> "low"

instance FromJSON Urgency where
  parseJSON = withText "Urgency" $ \case
    "high" -> pure High
    "normal" -> pure Normal
    "low" -> pure Low
    _ -> fail "Invalid urgency"

data Content = Content
  { title :: Text
  , summary :: Text
  , senderEmail :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON Content
instance FromJSON Content

data CalendarInfo = CalendarInfo
  { startsAt :: UTCTime
  , endsAt :: UTCTime
  } deriving (Generic, Show)

instance ToJSON CalendarInfo
instance FromJSON CalendarInfo

-- Smart constructor
mkActivity :: EntityId -> Source -> Text -> Value -> Activity
mkActivity aid src srcId raw = Activity
  { schema = Schema "activity" 1
  , id = aid
  , source = src
  , sourceId = srcId
  , raw = raw
  , classification = Nothing
  , content = Nothing
  , status = Pending
  , personId = Nothing
  , calendar = Nothing
  }
```

### Person

```haskell
-- Domain/Person.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Person where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Domain.Id (EntityId)
import Domain.Schema (Schema(..))
import Domain.Activity (Persona)

data Person = Person
  { schema :: Schema
  , id :: EntityId
  , email :: Text
  , displayName :: Text
  , personas :: [Persona]
  , relationship :: Maybe Text
  , organisation :: Maybe Text
  , notes :: Maybe Text
  , stats :: PersonStats
  } deriving (Generic, Show)

instance ToJSON Person
instance FromJSON Person

data PersonStats = PersonStats
  { firstContact :: UTCTime
  , lastContact :: UTCTime
  , contactCount :: Int
  } deriving (Generic, Show)

instance ToJSON PersonStats
instance FromJSON PersonStats

-- Smart constructor
mkPerson :: EntityId -> Text -> Text -> UTCTime -> Person
mkPerson pid email displayName now = Person
  { schema = Schema "person" 1
  , id = pid
  , email = email
  , displayName = displayName
  , personas = []
  , relationship = Nothing
  , organisation = Nothing
  , notes = Nothing
  , stats = PersonStats now now 1
  }
```

---

## Job Queue

### Schema

```sql
-- migrations/002_jobs.sql

create table jobs (
  id text primary key,
  job_type text not null,
  status text not null default 'queued',
  payload jsonb not null,
  run_after timestamptz,
  attempts int not null default 0,
  max_attempts int not null default 3,
  claimed_at timestamptz,
  last_error text,
  created_at timestamptz not null default now()
);

create index jobs_runnable
  on jobs (status, run_after)
  where status = 'queued';
```

### Job Types

```haskell
-- Domain/Job.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Domain.Job where

import Data.Aeson (ToJSON, FromJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)
import Domain.Id (EntityId)

data JobPayload
  = ProcessEmail { messageId :: Text, raw :: Value }
  | ProcessCalendarEvent { eventId :: Text, raw :: Value }
  | Classify { activityId :: EntityId }
  | Route { activityId :: EntityId }
  deriving (Generic, Show)

instance ToJSON JobPayload
instance FromJSON JobPayload

jobType :: JobPayload -> Text
jobType = \case
  ProcessEmail{} -> "process_email"
  ProcessCalendarEvent{} -> "process_calendar_event"
  Classify{} -> "classify"
  Route{} -> "route"
```

### Queue Operations

```haskell
-- Infra/Db/Jobs.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Infra.Db.Jobs where

import Database.PostgreSQL.Simple
import Data.Aeson (ToJSON, FromJSON, Value, toJSON)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Control.Monad.IO.Class (liftIO)
import Domain.Id (EntityId, newEntityId)
import App.Monad (App, getConn)

data Job = Job
  { id :: EntityId
  , payload :: Value
  } deriving (Show)

-- Enqueue a new job
enqueue :: (ToJSON a) => Text -> a -> App EntityId
enqueue jobType payload = do
  conn <- getConn
  jid <- liftIO newEntityId
  liftIO $ execute conn
    "insert into jobs (id, job_type, payload) values (?, ?, ?)"
    (jid.unEntityId, jobType, toJSON payload)
  pure jid

-- Enqueue with delay
enqueueDelayed :: (ToJSON a) => Text -> a -> NominalDiffTime -> App EntityId
enqueueDelayed jobType payload delay = do
  conn <- getConn
  jid <- liftIO newEntityId
  liftIO $ execute conn
    "insert into jobs (id, job_type, payload, run_after) \
    \values (?, ?, ?, now() + ?)"
    (jid.unEntityId, jobType, toJSON payload, delay)
  pure jid

-- Claim next available job (FOR UPDATE SKIP LOCKED)
claimJob :: Text -> App (Maybe Job)
claimJob jobType = do
  conn <- getConn
  liftIO $ withTransaction conn $ do
    results <- query conn
      "select id, payload from jobs \
      \where job_type = ? \
      \  and status = 'queued' \
      \  and (run_after is null or run_after <= now()) \
      \order by created_at \
      \limit 1 \
      \for update skip locked"
      (Only jobType)
    case results of
      [(jid, payload)] -> do
        execute conn
          "update jobs set status = 'claimed', claimed_at = now(), \
          \attempts = attempts + 1 where id = ?"
          (Only (jid :: Text))
        pure $ Just (Job (EntityId jid) payload)
      _ -> pure Nothing

-- Mark job complete
completeJob :: EntityId -> App ()
completeJob jid = do
  conn <- getConn
  liftIO $ execute conn
    "update jobs set status = 'completed' where id = ?"
    (Only jid.unEntityId)

-- Mark job failed
failJob :: EntityId -> Text -> App ()
failJob jid err = do
  conn <- getConn
  liftIO $ execute conn
    "update jobs set status = 'failed', last_error = ? where id = ?"
    (err, jid.unEntityId)

-- Requeue failed job (if under max attempts)
requeueJob :: EntityId -> App Bool
requeueJob jid = do
  conn <- getConn
  n <- liftIO $ execute conn
    "update jobs set status = 'queued', claimed_at = null \
    \where id = ? and attempts < max_attempts"
    (Only jid.unEntityId)
  pure (n > 0)
```

---

## Workers

### Worker Loop

```haskell
-- App/Workers.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module App.Workers where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Data.Aeson (FromJSON, fromJSON, Result(..))
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Infra.Db.Jobs
import App.Monad (App)

-- Worker result type
data WorkerResult
  = WorkerSuccess
  | WorkerFailed Text
  | WorkerRetry Text
  deriving (Show)

-- Run a worker loop for a job type
runWorker :: (FromJSON a) => Text -> (a -> App WorkerResult) -> App ()
runWorker jobType handler = forever $ do
  mjob <- claimJob jobType
  case mjob of
    Nothing -> liftIO $ threadDelay 1_000_000  -- 1 second
    Just job -> do
      case fromJSON job.payload of
        Error err -> do
          failJob job.id ("JSON decode error: " <> T.pack err)
        Success payload -> do
          result <- handler payload
          case result of
            WorkerSuccess -> completeJob job.id
            WorkerFailed err -> failJob job.id err
            WorkerRetry err -> do
              failJob job.id err
              void $ requeueJob job.id
```

### Classify Worker

```haskell
-- App/Workers/Classify.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Workers.Classify where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)
import Domain.Activity
import Domain.Job (JobPayload(..))
import Infra.Db.Entity
import Infra.Db.Jobs (enqueue)
import Infra.Llm.Client
import Infra.Llm.Prompts
import App.Monad (App)
import App.Workers (WorkerResult(..))

classifyWorker :: JobPayload -> App WorkerResult
classifyWorker (Classify activityId) = do
  -- Fetch activity
  mactivity <- fetchEntity "activity" activityId
  case mactivity of
    Nothing -> pure $ WorkerFailed "Activity not found"
    Just (version, activity) -> do
      -- Call LLM for classification
      classResult <- classifyWithLlm activity.raw
      case classResult of
        Left err -> pure $ WorkerRetry (T.pack $ show err)
        Right (classification, summary) -> do
          -- Update activity with classification
          let updated = activity
                { classification = Just classification
                , content = Just Content
                    { title = extractTitle activity.raw
                    , summary = summary
                    , senderEmail = extractSender activity.raw
                    }
                }
          success <- updateEntity "activity" activityId version updated
          if success
            then do
              -- Enqueue route job
              void $ enqueue "route" (Route activityId)
              pure WorkerSuccess
            else pure $ WorkerRetry "Concurrent modification"
classifyWorker _ = pure $ WorkerFailed "Invalid job payload for classify worker"

extractTitle :: Value -> Text
extractTitle raw = fromMaybe "(no subject)" $
  raw ^? key "subject" . _String

extractSender :: Value -> Maybe Text
extractSender raw = raw ^? key "from" . _String
```

### Route Worker

```haskell
-- App/Workers/Route.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Workers.Route where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.Activity
import Domain.Job (JobPayload(..))
import Infra.Db.Entity
import Infra.Db.Receipts
import App.Env (getConfig)
import App.Monad (App)
import App.Workers (WorkerResult(..))

routeWorker :: JobPayload -> App WorkerResult
routeWorker (Route activityId) = do
  mactivity <- fetchEntity "activity" activityId
  case mactivity of
    Nothing -> pure $ WorkerFailed "Activity not found"
    Just (version, activity) -> do
      case activity.classification of
        Nothing -> pure $ WorkerFailed "Activity not classified"
        Just classification -> do
          -- Determine status based on confidence and tier
          cfg <- getConfig
          let newStatus = routeToStatus cfg.confidenceThreshold classification

          -- Update activity
          let updated = activity { status = newStatus }
          success <- updateEntity "activity" activityId version updated

          if success
            then do
              -- Log receipt
              logReceipt activityId newStatus classification
              pure WorkerSuccess
            else pure $ WorkerRetry "Concurrent modification"
routeWorker _ = pure $ WorkerFailed "Invalid job payload for route worker"

routeToStatus :: Double -> Classification -> Status
routeToStatus threshold classification
  | classification.confidence < threshold = Quarantined
  | classification.autonomyTier == 1 = Processed
  | classification.autonomyTier == 2 = Processed
  | classification.autonomyTier == 3 = PendingReview
  | classification.autonomyTier == 4 = Surfaced
  | otherwise = Quarantined
```

---

## HTTP Handlers

### Pattern

```haskell
-- Http/Handlers/Activities.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Http.Handlers.Activities where

import Web.Scotty.Trans
import Data.Aeson (object, (.=))
import Network.HTTP.Types.Status (status404, status409)
import Control.Monad.IO.Class (liftIO)
import Domain.Activity
import Domain.Id (EntityId(..))
import Infra.Db.Entity
import App.Monad (App)

-- List all activities with optional filters
getActivities :: ActionT App ()
getActivities = do
  statusFilter <- queryParamMaybe "status"
  personaFilter <- queryParamMaybe "persona"

  activities <- lift $ fetchAllEntities "activity"

  let filtered = activities
        & maybe Prelude.id (filterByStatus . parseStatus) statusFilter
        & maybe Prelude.id (filterByPersona . parsePersona) personaFilter

  json $ map (\(_, _, a) -> a) filtered

-- Get single activity
getActivity :: ActionT App ()
getActivity = do
  aid <- EntityId <$> param "id"
  mactivity <- lift $ fetchEntity "activity" aid
  case mactivity of
    Nothing -> status status404 >> json (object ["error" .= ("Not found" :: Text)])
    Just (_, activity) -> json activity

-- Reclassify an activity (manual override)
postReclassify :: ActionT App ()
postReclassify = do
  aid <- EntityId <$> param "id"
  body <- jsonData

  mactivity <- lift $ fetchEntity "activity" aid
  case mactivity of
    Nothing -> status status404 >> json (object ["error" .= ("Not found" :: Text)])
    Just (version, activity) -> do
      let updated = applyReclassification activity body
      success <- lift $ updateEntity "activity" aid version updated
      if success
        then json updated
        else status status409 >> json (object ["error" .= ("Concurrent modification" :: Text)])
```

### Routes

```haskell
-- Http/Routes.hs
{-# LANGUAGE OverloadedStrings #-}

module Http.Routes where

import Web.Scotty.Trans
import Http.Handlers.Health
import Http.Handlers.Auth
import Http.Handlers.Activities
import Http.Handlers.People
import Http.Handlers.Quarantine
import Http.Handlers.Schedule
import Http.Handlers.Chat
import App.Monad (App)

routes :: ScottyT App ()
routes = do
  -- Health
  get "/health" getHealth

  -- Auth
  get "/auth/google" getGoogleAuth
  get "/auth/google/callback" getGoogleCallback

  -- Activities
  get "/activities" getActivities
  get "/activities/:id" getActivity
  post "/activities/:id/reclassify" postReclassify

  -- Quarantine
  get "/quarantine" getQuarantine
  post "/quarantine/:id/approve" postApprove
  post "/quarantine/:id/dismiss" postDismiss

  -- People
  get "/people" getPeople
  get "/people/:id" getPerson

  -- Schedule
  get "/schedule/today" getToday

  -- Chat
  post "/chat" postChat
```

---

## LLM Client

### Client

```haskell
-- Infra/Llm/Client.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}

module Infra.Llm.Client where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import System.Environment (getEnv)
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import App.Monad (App)

data LlmError
  = LlmHttpError HttpException
  | LlmParseError Text
  | LlmApiError Int Text
  deriving (Show)

data Message = Message
  { role :: Text
  , content :: Text
  } deriving (Generic, Show)

instance ToJSON Message
instance FromJSON Message

callClaude :: [Message] -> App (Either LlmError Text)
callClaude messages = liftIO $ do
  apiKey <- getEnv "ANTHROPIC_API_KEY"
  manager <- newManager tlsManagerSettings

  let reqBody = object
        [ "model" .= ("claude-sonnet-4-20250514" :: Text)
        , "max_tokens" .= (1024 :: Int)
        , "messages" .= messages
        ]

  initialReq <- parseRequest "https://api.anthropic.com/v1/messages"
  let req = initialReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("X-API-Key", BS.pack apiKey)
            , ("anthropic-version", "2023-06-01")
            ]
        , requestBody = RequestBodyLBS (encode reqBody)
        }

  response <- try $ httpLbs req manager
  case response of
    Left err -> pure $ Left (LlmHttpError err)
    Right resp -> do
      let body = responseBody resp
      case decode body of
        Nothing -> pure $ Left (LlmParseError "Failed to parse response")
        Just obj -> do
          case obj ^? key "content" . nth 0 . key "text" . _String of
            Just text -> pure $ Right text
            Nothing -> pure $ Left (LlmParseError "No content in response")
```

### Classification Prompt

```haskell
-- Infra/Llm/Prompts.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Infra.Llm.Prompts where

import Data.Aeson (Value, decode, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import NeatInterpolation (trimming)
import Domain.Activity (Classification)
import Infra.Llm.Client
import App.Monad (App)

classifyWithLlm :: Value -> App (Either LlmError (Classification, Text))
classifyWithLlm rawEmail = do
  let prompt = classificationPrompt (T.pack $ show rawEmail)
  result <- callClaude [Message "user" prompt]
  pure $ result >>= parseClassificationResponse

parseClassificationResponse :: Text -> Either LlmError (Classification, Text)
parseClassificationResponse text = do
  obj <- maybe (Left $ LlmParseError "Failed to parse response JSON") Right $
    decode (TL.encodeUtf8 $ TL.fromStrict text)
  (classJson, summary) <- maybe (Left $ LlmParseError "Missing classification or summary") Right $
    (,) <$> obj ^? key "classification" <*> obj ^? key "summary" . _String
  classification <- maybe (Left $ LlmParseError "Failed to parse classification") Right $
    decode (encode classJson)
  Right (classification, summary)

classificationPrompt :: Text -> Text
classificationPrompt emailData = [trimming|
  Classify this email for a personal assistant system.

  Return ONLY valid JSON with these fields:
  {
    "classification": {
      "personas": ["work", "home", and/or "personal"],
      "activityType": "request" | "information" | "action_required" | "fyi" | "event",
      "urgency": "high" | "normal" | "low",
      "autonomyTier": 1-4,
      "confidence": 0.0-1.0
    },
    "summary": "1-2 sentence summary"
  }

  Autonomy tiers:
    1 = Routine, can be handled automatically
    2 = Standard, handle but notify user
    3 = Important, draft response for review
    4 = Significant, surface to user without action

  Email data:
  ${emailData}
|]
```

---

## Receipts (Audit Log)

### Schema

```sql
-- migrations/003_receipts.sql

create table receipts (
  id text primary key,
  activity_id text not null,
  action_taken text not null,
  action_detail text,
  confidence float,
  created_at timestamptz not null default now()
);

create index receipts_by_activity on receipts (activity_id);
create index receipts_by_time on receipts (created_at desc);
```

### Operations

```haskell
-- Infra/Db/Receipts.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Infra.Db.Receipts where

import Database.PostgreSQL.Simple
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Control.Monad.IO.Class (liftIO)
import Domain.Id (EntityId(..), newEntityId)
import Domain.Activity (Status(..), Classification(..))
import App.Monad (App, getConn)

data Receipt = Receipt
  { id :: EntityId
  , activityId :: EntityId
  , actionTaken :: Text
  , actionDetail :: Maybe Text
  , confidence :: Maybe Double
  , createdAt :: UTCTime
  } deriving (Show)

logReceipt :: EntityId -> Status -> Classification -> App ()
logReceipt activityId status classification = do
  conn <- getConn
  rid <- liftIO newEntityId
  let action = statusToAction status
  let detail = Just $ "Classified as tier " <> T.pack (show classification.autonomyTier)
  liftIO $ execute conn
    "insert into receipts (id, activity_id, action_taken, action_detail, confidence) \
    \values (?, ?, ?, ?, ?)"
    (rid.unEntityId, activityId.unEntityId, action, detail, classification.confidence)

statusToAction :: Status -> Text
statusToAction = \case
  Pending -> "pending"
  Quarantined -> "quarantined"
  Processed -> "processed"
  Surfaced -> "surfaced"
  PendingReview -> "pending_review"
  Archived -> "archived"

fetchReceiptsForActivity :: EntityId -> App [Receipt]
fetchReceiptsForActivity activityId = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, activity_id, action_taken, action_detail, confidence, created_at \
    \from receipts where activity_id = ? order by created_at"
    (Only activityId.unEntityId)
  pure [Receipt (EntityId rid) (EntityId aid) action detail conf ts
       | (rid, aid, action, detail, conf, ts) <- results]
```

---

## Other Tables

### Auth Tokens

```sql
-- migrations/004_auth.sql

create table auth_tokens (
  id text primary key,
  provider text not null,
  access_token text not null,
  refresh_token text not null,
  expires_at timestamptz not null,
  scopes text[] not null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
```

### Poll State

```sql
-- migrations/005_poll_state.sql

create table poll_state (
  source text primary key,
  last_poll_at timestamptz not null,
  cursor text
);
```

---

## Configuration Loading

```haskell
-- App/Env.hs (continued)
{-# LANGUAGE OverloadedStrings #-}

import Data.Yaml (decodeFileEither)
import System.Environment (lookupEnv)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import Data.Text.Encoding (encodeUtf8)

loadConfig :: IO Config
loadConfig = do
  fileConfig <- decodeFileEither "wisp.yaml" >>= \case
    Left err -> error $ "Config error: " <> show err
    Right c -> pure c

  -- Override with env vars
  dbUrl <- lookupEnv "DATABASE_URL"
  port <- lookupEnv "PORT"

  pure fileConfig
    { dbUrl = maybe fileConfig.dbUrl T.pack dbUrl
    , serverPort = maybe fileConfig.serverPort read port
    }

buildEnv :: Config -> IO Env
buildEnv config = do
  conn <- connectPostgreSQL (encodeUtf8 config.dbUrl)
  logger <- newStdoutLoggerSet defaultBufSize
  pure Env
    { config = config
    , dbConn = conn
    , logger = logger
    }
```

---

## Main Entry Point

```haskell
-- Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (async)
import App.Env (loadConfig, buildEnv)
import App.Monad (runApp)
import App.Poller (runPoller)
import App.Workers (runWorker)
import App.Workers.Classify (classifyWorker)
import App.Workers.Route (routeWorker)
import Http.Server (startServer)

main :: IO ()
main = do
  config <- loadConfig
  env <- buildEnv config

  -- Start workers in background
  _ <- async $ runApp env $ runWorker "classify" classifyWorker
  _ <- async $ runApp env $ runWorker "route" routeWorker

  -- Start poller in background
  _ <- async $ runApp env runPoller

  -- Start HTTP server (blocks)
  runApp env startServer
```

---

## Error Handling

### Pattern

Use `Either` at module boundaries, not throughout:

```haskell
-- Good: Either at the boundary
fetchPerson :: EntityId -> App (Either DbError Person)

-- Good: Internal functions can use Maybe
lookupPersonByEmail :: Text -> App (Maybe Person)

-- Avoid: Either everywhere internally
-- internalHelper :: X -> Either E Y  -- NO
```

### Error Types

Keep error types simple and specific to the module:

```haskell
-- Infra/Db/Entity.hs
data EntityError
  = EntityNotFound
  | EntityConcurrentModification
  deriving (Show)

-- Infra/Llm/Client.hs
data LlmError
  = LlmHttpError HttpException
  | LlmParseError Text
  | LlmApiError Int Text
  deriving (Show)
```

---

## Testing Pattern

Tests use the same `App` monad with a test database:

```haskell
-- test/TestEnv.hs
{-# LANGUAGE OverloadedStrings #-}

module TestEnv where

import Database.PostgreSQL.Simple
import System.Log.FastLogger
import App.Env (Env(..), Config(..))

testConfig :: Config
testConfig = Config
  { serverHost = "127.0.0.1"
  , serverPort = 8080
  , dbUrl = "postgres://localhost/wisp_test"
  , googleClientId = "test"
  , googleClientSecret = "test"
  , pollingIntervalMinutes = 5
  , confidenceThreshold = 0.5
  }

withTestEnv :: (Env -> IO a) -> IO a
withTestEnv action = do
  conn <- connectPostgreSQL "postgres://localhost/wisp_test"
  execute_ conn "begin"
  logger <- newStdoutLoggerSet defaultBufSize
  let env = Env testConfig conn logger
  result <- action env
  execute_ conn "rollback"
  pure result
```

---

## Summary Checklist

When implementing any backend component:

- [ ] Use required language extensions at top of every module
- [ ] Use `ReaderT Env IO` for all effectful code
- [ ] Use `.field` syntax for record access
- [ ] Use `EntityId` (12-char Nanoid) for all IDs
- [ ] Avoid deep nesting - extract small composable functions
- [ ] Use `neat-interpolation` for multiline text (`[trimming|...|]`)
- [ ] Keep error types simple, `Either` only at boundaries
- [ ] Store domain entities as versioned JSON in `entities` table
- [ ] Include `schema` field in all JSON payloads
- [ ] Let PostgreSQL trigger handle version increment
- [ ] Use `FOR UPDATE SKIP LOCKED` for job claiming
- [ ] Workers return `WorkerResult`, not throw exceptions
- [ ] HTTP handlers are thin, delegate to App functions
- [ ] Prompts are hardcoded in `Infra/Llm/Prompts.hs`
- [ ] Receipts log every significant action
- [ ] Tests use transaction rollback for isolation
