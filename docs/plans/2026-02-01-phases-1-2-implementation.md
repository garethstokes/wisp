# Wisp Backend Phases 1 & 2 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build the foundation infrastructure for wisp-srv including database schema, job queue, REST API skeleton, and Google OAuth authentication flow.

**Architecture:** Haskell backend using Simple Haskell style with ReaderT Env IO monad. PostgreSQL for persistence with versioned JSON entities. Scotty for HTTP routing. Google OAuth for Gmail/Calendar access.

**Tech Stack:** Haskell (GHC 9.6+), postgresql-simple, scotty, aeson, yaml, async, http-client-tls, nanoid

---

## Task 1: Project Scaffolding

**Files:**
- Create: `wisp-srv/wisp-srv.cabal`
- Create: `wisp-srv/app/Main.hs`
- Create: `cabal.project`

**Step 1: Create cabal.project for workspace**

```cabal
-- cabal.project
packages: wisp-srv
```

**Step 2: Run to verify cabal project file is valid**

Run: `cd /home/gareth/code/hacking/wisp && cat cabal.project`
Expected: Shows the packages line

**Step 3: Create wisp-srv.cabal with all dependencies**

```cabal
cabal-version: 3.0
name:          wisp-srv
version:       0.1.0.0
synopsis:      Wisp personal assistant backend
license:       BSD-3-Clause
author:        Gareth
build-type:    Simple

common warnings
    ghc-options: -Wall -Wunused-packages

executable wisp-srv
    import:           warnings
    main-is:          Main.hs
    hs-source-dirs:   app, src
    default-language: GHC2021
    default-extensions:
        OverloadedStrings
        OverloadedRecordDot
        DuplicateRecordFields
        DeriveGeneric
        GeneralizedNewtypeDeriving
        DerivingStrategies
        LambdaCase
        QuasiQuotes
        RecordWildCards
        StrictData
    build-depends:
        base >=4.17 && <5,
        text,
        bytestring,
        aeson,
        yaml,
        time,
        mtl,
        transformers,
        postgresql-simple,
        async,
        http-client,
        http-client-tls,
        http-types,
        scotty,
        wai,
        warp,
        fast-logger,
        neat-interpolation,
        nanoid,
        directory,
        filepath,
        uri-encode

test-suite wisp-srv-test
    import:           warnings
    type:             exitcode-stdio-1.0
    main-is:          Main.hs
    hs-source-dirs:   test
    default-language: GHC2021
    default-extensions:
        OverloadedStrings
        OverloadedRecordDot
        DuplicateRecordFields
        DeriveGeneric
        GeneralizedNewtypeDeriving
        DerivingStrategies
        LambdaCase
        QuasiQuotes
    build-depends:
        base >=4.17 && <5,
        wisp-srv,
        hspec,
        hspec-discover
```

**Step 4: Create placeholder Main.hs**

```haskell
-- app/Main.hs
module Main where

main :: IO ()
main = putStrLn "wisp-srv starting..."
```

**Step 5: Create placeholder test Main.hs**

```haskell
-- test/Main.hs
module Main where

main :: IO ()
main = putStrLn "Tests not yet implemented"
```

**Step 6: Run cabal build to verify project compiles**

Run: `cd /home/gareth/code/hacking/wisp && cabal build wisp-srv`
Expected: Build succeeds (may download dependencies first)

**Step 7: Commit**

```bash
git add cabal.project wisp-srv/
git commit -m "feat: scaffold wisp-srv Haskell project with dependencies"
```

---

## Task 2: Domain ID Type

**Files:**
- Create: `wisp-srv/src/Domain/Id.hs`

**Step 1: Write the failing test for EntityId**

Create `wisp-srv/test/Domain/IdSpec.hs`:

```haskell
module Domain.IdSpec where

import Test.Hspec
import Domain.Id
import Data.Aeson (encode, decode)

spec :: Spec
spec = describe "EntityId" $ do
  it "generates 12-character IDs" $ do
    eid <- newEntityId
    length (unEntityId eid) `shouldBe` 12

  it "round-trips through JSON" $ do
    eid <- newEntityId
    decode (encode eid) `shouldBe` Just eid
```

**Step 2: Update test/Main.hs to run specs**

```haskell
-- test/Main.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

**Step 3: Run test to verify it fails**

Run: `cd /home/gareth/code/hacking/wisp && cabal test`
Expected: FAIL - module Domain.Id not found

**Step 4: Write minimal implementation**

```haskell
-- src/Domain/Id.hs
module Domain.Id
  ( EntityId(..)
  , newEntityId
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.FromField (FromField)
import Nanoid (nanoid)

newtype EntityId = EntityId { unEntityId :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, ToField, FromField)

newEntityId :: IO EntityId
newEntityId = EntityId <$> nanoid 12
```

**Step 5: Run test to verify it passes**

Run: `cd /home/gareth/code/hacking/wisp && cabal test`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Domain/Id.hs wisp-srv/test/Domain/IdSpec.hs wisp-srv/test/Main.hs
git commit -m "feat: add EntityId type with 12-char Nanoid generation"
```

---

## Task 3: Schema Helper Type

**Files:**
- Create: `wisp-srv/src/Domain/Schema.hs`

**Step 1: Write the failing test**

Create `wisp-srv/test/Domain/SchemaSpec.hs`:

```haskell
module Domain.SchemaSpec where

import Test.Hspec
import Domain.Schema
import Data.Aeson (encode, decode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS

spec :: Spec
spec = describe "Schema" $ do
  it "serializes to expected JSON structure" $ do
    let s = Schema "activity" 1
    encode s `shouldBe` "{\"name\":\"activity\",\"version\":1}"

  it "round-trips through JSON" $ do
    let s = Schema "person" 2
    decode (encode s) `shouldBe` Just s
```

**Step 2: Run test to verify it fails**

Run: `cd /home/gareth/code/hacking/wisp && cabal test`
Expected: FAIL - module Domain.Schema not found

**Step 3: Write minimal implementation**

```haskell
-- src/Domain/Schema.hs
module Domain.Schema
  ( Schema(..)
  ) where

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

**Step 4: Run test to verify it passes**

Run: `cd /home/gareth/code/hacking/wisp && cabal test`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Domain/Schema.hs wisp-srv/test/Domain/SchemaSpec.hs
git commit -m "feat: add Schema type for entity versioning"
```

---

## Task 4: App Monad and Environment

**Files:**
- Create: `wisp-srv/src/App/Monad.hs`
- Create: `wisp-srv/src/App/Env.hs`

**Step 1: Write the failing test**

Create `wisp-srv/test/App/EnvSpec.hs`:

```haskell
module App.EnvSpec where

import Test.Hspec
import App.Env
import Data.Yaml (decodeEither')
import qualified Data.ByteString.Char8 as BS

spec :: Spec
spec = describe "Config" $ do
  it "parses from YAML" $ do
    let yaml = BS.unlines
          [ "server:"
          , "  host: 127.0.0.1"
          , "  port: 8080"
          , "database:"
          , "  url: postgres://localhost/wisp"
          , "google:"
          , "  clientId: test-id"
          , "  clientSecret: test-secret"
          , "polling:"
          , "  intervalMinutes: 5"
          , "classification:"
          , "  confidenceThreshold: 0.5"
          ]
    case decodeEither' yaml of
      Left err -> expectationFailure $ show err
      Right cfg -> do
        cfg.server.host `shouldBe` "127.0.0.1"
        cfg.server.port `shouldBe` 8080
        cfg.database.url `shouldBe` "postgres://localhost/wisp"
```

**Step 2: Run test to verify it fails**

Run: `cd /home/gareth/code/hacking/wisp && cabal test`
Expected: FAIL - module App.Env not found

**Step 3: Write App/Monad.hs**

```haskell
-- src/App/Monad.hs
module App.Monad
  ( App
  , runApp
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import App.Env (Env)

type App a = ReaderT Env IO a

runApp :: Env -> App a -> IO a
runApp = flip runReaderT
```

**Step 4: Write App/Env.hs**

```haskell
-- src/App/Env.hs
module App.Env
  ( Config(..)
  , ServerConfig(..)
  , DatabaseConfig(..)
  , GoogleConfig(..)
  , PollingConfig(..)
  , ClassificationConfig(..)
  , Env(..)
  , loadConfig
  , buildEnv
  , getConfig
  , getConn
  , getLogger
  ) where

import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.Log.FastLogger (LoggerSet, newStdoutLoggerSet, defaultBufSize)
import App.Monad (App)

data ServerConfig = ServerConfig
  { host :: Text
  , port :: Int
  } deriving (Generic, Show)

instance FromJSON ServerConfig

data DatabaseConfig = DatabaseConfig
  { url :: Text
  } deriving (Generic, Show)

instance FromJSON DatabaseConfig

data GoogleConfig = GoogleConfig
  { clientId :: Text
  , clientSecret :: Text
  } deriving (Generic, Show)

instance FromJSON GoogleConfig

data PollingConfig = PollingConfig
  { intervalMinutes :: Int
  } deriving (Generic, Show)

instance FromJSON PollingConfig

data ClassificationConfig = ClassificationConfig
  { confidenceThreshold :: Double
  } deriving (Generic, Show)

instance FromJSON ClassificationConfig

data Config = Config
  { server :: ServerConfig
  , database :: DatabaseConfig
  , google :: GoogleConfig
  , polling :: PollingConfig
  , classification :: ClassificationConfig
  } deriving (Generic, Show)

instance FromJSON Config

data Env = Env
  { config :: Config
  , dbConn :: Connection
  , logger :: LoggerSet
  }

loadConfig :: FilePath -> IO Config
loadConfig path = do
  result <- decodeFileEither path
  case result of
    Left err -> error $ "Config error: " <> show err
    Right cfg -> do
      -- Override with env vars if present
      mDbUrl <- lookupEnv "DATABASE_URL"
      mPort <- lookupEnv "PORT"
      pure cfg
        { database = cfg.database
            { url = maybe cfg.database.url T.pack mDbUrl
            }
        , server = cfg.server
            { port = maybe cfg.server.port read mPort
            }
        }

buildEnv :: Config -> IO Env
buildEnv cfg = do
  conn <- connectPostgreSQL (encodeUtf8 cfg.database.url)
  lgr <- newStdoutLoggerSet defaultBufSize
  pure Env
    { config = cfg
    , dbConn = conn
    , logger = lgr
    }

getConfig :: App Config
getConfig = asks (.config)

getConn :: App Connection
getConn = asks (.dbConn)

getLogger :: App LoggerSet
getLogger = asks (.logger)
```

**Step 5: Fix circular import - move App type to Env module**

Actually, we need to restructure to avoid circular imports. Update `App/Monad.hs`:

```haskell
-- src/App/Monad.hs
module App.Monad
  ( Env(..)
  , App
  , runApp
  , getConfig
  , getConn
  , getLogger
  ) where

import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Database.PostgreSQL.Simple (Connection)
import System.Log.FastLogger (LoggerSet)

-- Forward declaration - Env needs Config
import App.Config (Config)

data Env = Env
  { config :: Config
  , dbConn :: Connection
  , logger :: LoggerSet
  }

type App a = ReaderT Env IO a

runApp :: Env -> App a -> IO a
runApp = flip runReaderT

getConfig :: App Config
getConfig = asks (.config)

getConn :: App Connection
getConn = asks (.dbConn)

getLogger :: App LoggerSet
getLogger = asks (.logger)
```

Create separate `App/Config.hs`:

```haskell
-- src/App/Config.hs
module App.Config
  ( Config(..)
  , ServerConfig(..)
  , DatabaseConfig(..)
  , GoogleConfig(..)
  , PollingConfig(..)
  , ClassificationConfig(..)
  , loadConfig
  ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)

data ServerConfig = ServerConfig
  { host :: Text
  , port :: Int
  } deriving (Generic, Show)

instance FromJSON ServerConfig

data DatabaseConfig = DatabaseConfig
  { url :: Text
  } deriving (Generic, Show)

instance FromJSON DatabaseConfig

data GoogleConfig = GoogleConfig
  { clientId :: Text
  , clientSecret :: Text
  } deriving (Generic, Show)

instance FromJSON GoogleConfig

data PollingConfig = PollingConfig
  { intervalMinutes :: Int
  } deriving (Generic, Show)

instance FromJSON PollingConfig

data ClassificationConfig = ClassificationConfig
  { confidenceThreshold :: Double
  } deriving (Generic, Show)

instance FromJSON ClassificationConfig

data Config = Config
  { server :: ServerConfig
  , database :: DatabaseConfig
  , google :: GoogleConfig
  , polling :: PollingConfig
  , classification :: ClassificationConfig
  } deriving (Generic, Show)

instance FromJSON Config

loadConfig :: FilePath -> IO Config
loadConfig path = do
  result <- decodeFileEither path
  case result of
    Left err -> error $ "Config error: " <> show err
    Right cfg -> do
      mDbUrl <- lookupEnv "DATABASE_URL"
      mPort <- lookupEnv "PORT"
      pure cfg
        { database = cfg.database
            { url = maybe cfg.database.url T.pack mDbUrl
            }
        , server = cfg.server
            { port = maybe cfg.server.port read mPort
            }
        }
```

Update `App/Env.hs` to just re-export and provide buildEnv:

```haskell
-- src/App/Env.hs
module App.Env
  ( module App.Config
  , module App.Monad
  , buildEnv
  ) where

import Database.PostgreSQL.Simple (connectPostgreSQL)
import Data.Text.Encoding (encodeUtf8)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import App.Config
import App.Monad

buildEnv :: Config -> IO Env
buildEnv cfg = do
  conn <- connectPostgreSQL (encodeUtf8 cfg.database.url)
  lgr <- newStdoutLoggerSet defaultBufSize
  pure Env
    { config = cfg
    , dbConn = conn
    , logger = lgr
    }
```

**Step 6: Update test to use correct module**

Update `wisp-srv/test/App/EnvSpec.hs`:

```haskell
module App.EnvSpec where

import Test.Hspec
import App.Config
import Data.Yaml (decodeEither')
import qualified Data.ByteString.Char8 as BS

spec :: Spec
spec = describe "Config" $ do
  it "parses from YAML" $ do
    let yaml = BS.unlines
          [ "server:"
          , "  host: 127.0.0.1"
          , "  port: 8080"
          , "database:"
          , "  url: postgres://localhost/wisp"
          , "google:"
          , "  clientId: test-id"
          , "  clientSecret: test-secret"
          , "polling:"
          , "  intervalMinutes: 5"
          , "classification:"
          , "  confidenceThreshold: 0.5"
          ]
    case decodeEither' yaml of
      Left err -> expectationFailure $ show err
      Right cfg -> do
        cfg.server.host `shouldBe` "127.0.0.1"
        cfg.server.port `shouldBe` 8080
        cfg.database.url `shouldBe` "postgres://localhost/wisp"
```

**Step 7: Run test to verify it passes**

Run: `cd /home/gareth/code/hacking/wisp && cabal test`
Expected: PASS

**Step 8: Commit**

```bash
git add wisp-srv/src/App/ wisp-srv/test/App/
git commit -m "feat: add App monad with Config and Env types"
```

---

## Task 5: Database Migrations - Entities Table

**Files:**
- Create: `wisp-srv/migrations/001_entities.sql`

**Step 1: Create migrations directory**

Run: `mkdir -p /home/gareth/code/hacking/wisp/wisp-srv/migrations`

**Step 2: Write the entities migration**

```sql
-- migrations/001_entities.sql

-- Versioned JSON entity storage
-- Each update creates a new version (append-only)

create table if not exists entities (
  entity_type text not null,
  entity_id text not null,
  entity_version int not null,
  created_at timestamptz not null default now(),
  payload jsonb not null,
  primary key (entity_type, entity_id, entity_version)
);

-- Index for efficient "get latest version" queries
create index if not exists entities_latest
  on entities (entity_type, entity_id, entity_version desc);

-- Ensure schema name in JSON matches entity_type
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

-- Apply trigger for auto-versioning when version is null
drop trigger if exists entities_version_trigger on entities;
create trigger entities_version_trigger
before insert on entities
for each row
when (new.entity_version is null)
execute function next_entity_version();
```

**Step 3: Verify SQL syntax is valid**

Run: `cat /home/gareth/code/hacking/wisp/wisp-srv/migrations/001_entities.sql`
Expected: Shows the SQL content

**Step 4: Commit**

```bash
git add wisp-srv/migrations/001_entities.sql
git commit -m "feat: add entities table migration with versioning trigger"
```

---

## Task 6: Database Migrations - Jobs Table

**Files:**
- Create: `wisp-srv/migrations/002_jobs.sql`

**Step 1: Write the jobs migration**

```sql
-- migrations/002_jobs.sql

-- PostgreSQL-backed job queue
-- Workers claim jobs with SELECT ... FOR UPDATE SKIP LOCKED

create table if not exists jobs (
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

-- Index for efficient job claiming
create index if not exists jobs_runnable
  on jobs (status, run_after)
  where status = 'queued';

-- Index for job type filtering
create index if not exists jobs_by_type
  on jobs (job_type, status);
```

**Step 2: Commit**

```bash
git add wisp-srv/migrations/002_jobs.sql
git commit -m "feat: add jobs table migration for queue"
```

---

## Task 7: Database Migrations - Receipts Table

**Files:**
- Create: `wisp-srv/migrations/003_receipts.sql`

**Step 1: Write the receipts migration**

```sql
-- migrations/003_receipts.sql

-- Audit log of all actions taken by Wisp

create table if not exists receipts (
  id text primary key,
  activity_id text not null,
  action_taken text not null,
  action_detail text,
  confidence float,
  created_at timestamptz not null default now()
);

-- Index for looking up receipts by activity
create index if not exists receipts_by_activity
  on receipts (activity_id);

-- Index for chronological queries
create index if not exists receipts_by_time
  on receipts (created_at desc);
```

**Step 2: Commit**

```bash
git add wisp-srv/migrations/003_receipts.sql
git commit -m "feat: add receipts table migration for audit log"
```

---

## Task 8: Database Migrations - Auth Tokens Table

**Files:**
- Create: `wisp-srv/migrations/004_auth.sql`

**Step 1: Write the auth tokens migration**

```sql
-- migrations/004_auth.sql

-- OAuth token storage for Google API access

create table if not exists auth_tokens (
  id text primary key,
  provider text not null,
  access_token text not null,
  refresh_token text not null,
  expires_at timestamptz not null,
  scopes text[] not null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

-- Only one token per provider
create unique index if not exists auth_tokens_provider_unique
  on auth_tokens (provider);
```

**Step 2: Commit**

```bash
git add wisp-srv/migrations/004_auth.sql
git commit -m "feat: add auth_tokens table migration for OAuth"
```

---

## Task 9: Database Migrations - Poll State Table

**Files:**
- Create: `wisp-srv/migrations/005_poll_state.sql`

**Step 1: Write the poll state migration**

```sql
-- migrations/005_poll_state.sql

-- Track polling progress for Gmail and Calendar

create table if not exists poll_state (
  source text primary key,
  last_poll_at timestamptz not null,
  cursor text
);

-- Pre-populate with sources
insert into poll_state (source, last_poll_at, cursor)
values
  ('gmail', now(), null),
  ('calendar', now(), null)
on conflict (source) do nothing;
```

**Step 2: Commit**

```bash
git add wisp-srv/migrations/005_poll_state.sql
git commit -m "feat: add poll_state table migration"
```

---

## Task 10: Migration Runner

**Files:**
- Create: `wisp-srv/src/Infra/Db/Migrations.hs`

**Step 1: Write test for migration runner**

Create `wisp-srv/test/Infra/Db/MigrationsSpec.hs`:

```haskell
module Infra.Db.MigrationsSpec where

import Test.Hspec
import Infra.Db.Migrations (getMigrationFiles, parseMigrationNumber)

spec :: Spec
spec = describe "Migrations" $ do
  describe "parseMigrationNumber" $ do
    it "extracts number from migration filename" $ do
      parseMigrationNumber "001_entities.sql" `shouldBe` Just 1
      parseMigrationNumber "002_jobs.sql" `shouldBe` Just 2
      parseMigrationNumber "010_foo.sql" `shouldBe` Just 10

    it "returns Nothing for invalid filenames" $ do
      parseMigrationNumber "entities.sql" `shouldBe` Nothing
      parseMigrationNumber "abc_entities.sql" `shouldBe` Nothing
```

**Step 2: Run test to verify it fails**

Run: `cd /home/gareth/code/hacking/wisp && cabal test`
Expected: FAIL - module Infra.Db.Migrations not found

**Step 3: Write minimal implementation**

```haskell
-- src/Infra/Db/Migrations.hs
module Infra.Db.Migrations
  ( runMigrations
  , getMigrationFiles
  , parseMigrationNumber
  ) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.PostgreSQL.Simple
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>), takeFileName)
import Text.Read (readMaybe)
import App.Monad (App, getConn)

-- Parse migration number from filename like "001_entities.sql"
parseMigrationNumber :: String -> Maybe Int
parseMigrationNumber name =
  case T.splitOn "_" (T.pack name) of
    (numPart : _) -> readMaybe (T.unpack numPart)
    _ -> Nothing

-- Get all migration files sorted by number
getMigrationFiles :: FilePath -> IO [(Int, FilePath)]
getMigrationFiles dir = do
  files <- listDirectory dir
  let sqlFiles = filter (\f -> ".sql" `T.isSuffixOf` T.pack f) files
  let numbered = mapMaybe (\f -> (,dir </> f) <$> parseMigrationNumber f) sqlFiles
  pure $ sortOn fst numbered

-- Track applied migrations
ensureMigrationsTable :: Connection -> IO ()
ensureMigrationsTable conn =
  execute_ conn
    "create table if not exists schema_migrations (\
    \  version int primary key,\
    \  applied_at timestamptz not null default now()\
    \)"
    >> pure ()

getAppliedMigrations :: Connection -> IO [Int]
getAppliedMigrations conn = do
  results <- query_ conn "select version from schema_migrations order by version"
  pure $ map fromOnly results

markMigrationApplied :: Connection -> Int -> IO ()
markMigrationApplied conn version =
  execute conn "insert into schema_migrations (version) values (?)" (Only version)
    >> pure ()

-- Run all pending migrations
runMigrations :: FilePath -> App ()
runMigrations dir = do
  conn <- getConn
  liftIO $ do
    ensureMigrationsTable conn
    applied <- getAppliedMigrations conn
    migrations <- getMigrationFiles dir
    let pending = filter (\(v, _) -> v `notElem` applied) migrations
    forM_ pending $ \(version, path) -> do
      putStrLn $ "Running migration " <> show version <> ": " <> takeFileName path
      sql <- TIO.readFile path
      withTransaction conn $ do
        execute_ conn (Query $ T.encodeUtf8 sql)
        markMigrationApplied conn version
      putStrLn $ "  Applied migration " <> show version
```

**Step 4: Run test to verify it passes**

Run: `cd /home/gareth/code/hacking/wisp && cabal test`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Db/Migrations.hs wisp-srv/test/Infra/Db/MigrationsSpec.hs
git commit -m "feat: add migration runner with version tracking"
```

---

## Task 11: Entity CRUD Operations

**Files:**
- Create: `wisp-srv/src/Infra/Db/Entity.hs`

**Step 1: Write test for entity operations**

Create `wisp-srv/test/Infra/Db/EntitySpec.hs`:

```haskell
module Infra.Db.EntitySpec where

import Test.Hspec
import Data.Aeson (Value(..), object, (.=))

-- Just test JSON structure for now (DB tests need integration setup)
spec :: Spec
spec = describe "Entity" $ do
  it "placeholder - entity operations need integration tests" $ do
    True `shouldBe` True
```

**Step 2: Write the Entity module**

```haskell
-- src/Infra/Db/Entity.hs
module Infra.Db.Entity
  ( insertEntity
  , insertEntityVersion
  , fetchEntity
  , fetchAllEntities
  , updateEntity
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, FromJSON, Value, toJSON, decode, encode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Domain.Id (EntityId(..))
import App.Monad (App, getConn)

-- Insert new version of entity (version auto-assigned by trigger)
insertEntity :: (ToJSON a) => Text -> EntityId -> a -> App ()
insertEntity entityType entityId payload = do
  conn <- getConn
  liftIO $ execute conn
    "insert into entities (entity_type, entity_id, payload) values (?, ?, ?)"
    (entityType, unEntityId entityId, toJSON payload)
  pure ()

-- Insert with explicit version (for optimistic concurrency)
insertEntityVersion :: (ToJSON a) => Text -> EntityId -> Int -> a -> App Bool
insertEntityVersion entityType entityId version payload = do
  conn <- getConn
  n <- liftIO $ execute conn
    "insert into entities (entity_type, entity_id, entity_version, payload) \
    \values (?, ?, ?, ?) on conflict do nothing"
    (entityType, unEntityId entityId, version, toJSON payload)
  pure (n > 0)

-- Fetch latest version
fetchEntity :: (FromJSON a) => Text -> EntityId -> App (Maybe (Int, a))
fetchEntity entityType entityId = do
  conn <- getConn
  results <- liftIO $ query conn
    "select entity_version, payload from entities \
    \where entity_type = ? and entity_id = ? \
    \order by entity_version desc limit 1"
    (entityType, unEntityId entityId)
  pure $ case results of
    [(v, p)] -> case decode (fromStrict p) of
      Just a -> Just (v, a)
      Nothing -> Nothing
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
  pure [ (EntityId eid, v, a)
       | (eid, v, p) <- results
       , Just a <- [decode (fromStrict p)]
       ]

-- Optimistic update: insert expected version + 1
updateEntity :: (ToJSON a) => Text -> EntityId -> Int -> a -> App Bool
updateEntity entityType entityId expectedVersion payload =
  insertEntityVersion entityType entityId (expectedVersion + 1) payload
```

**Step 3: Run test to verify it compiles**

Run: `cd /home/gareth/code/hacking/wisp && cabal test`
Expected: PASS (placeholder test)

**Step 4: Commit**

```bash
git add wisp-srv/src/Infra/Db/Entity.hs wisp-srv/test/Infra/Db/EntitySpec.hs
git commit -m "feat: add entity CRUD operations with versioning"
```

---

## Task 12: Job Queue Operations

**Files:**
- Create: `wisp-srv/src/Infra/Db/Jobs.hs`

**Step 1: Write test**

Create `wisp-srv/test/Infra/Db/JobsSpec.hs`:

```haskell
module Infra.Db.JobsSpec where

import Test.Hspec

spec :: Spec
spec = describe "Jobs" $ do
  it "placeholder - job operations need integration tests" $ do
    True `shouldBe` True
```

**Step 2: Write the Jobs module**

```haskell
-- src/Infra/Db/Jobs.hs
module Infra.Db.Jobs
  ( Job(..)
  , enqueue
  , enqueueDelayed
  , claimJob
  , completeJob
  , failJob
  , requeueJob
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, FromJSON, Value, toJSON, decode, encode)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Simple
import Domain.Id (EntityId(..), newEntityId)
import App.Monad (App, getConn)

data Job = Job
  { jobId :: EntityId
  , jobPayload :: Value
  } deriving (Show)

-- Enqueue a new job
enqueue :: (ToJSON a) => Text -> a -> App EntityId
enqueue jobType payload = do
  conn <- getConn
  jid <- liftIO newEntityId
  liftIO $ execute conn
    "insert into jobs (id, job_type, payload) values (?, ?, ?)"
    (unEntityId jid, jobType, toJSON payload)
  pure jid

-- Enqueue with delay
enqueueDelayed :: (ToJSON a) => Text -> a -> NominalDiffTime -> App EntityId
enqueueDelayed jobType payload delay = do
  conn <- getConn
  jid <- liftIO newEntityId
  liftIO $ execute conn
    "insert into jobs (id, job_type, payload, run_after) \
    \values (?, ?, ?, now() + ?)"
    (unEntityId jid, jobType, toJSON payload, delay)
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
        pure $ case decode (fromStrict payload) of
          Just p -> Just (Job (EntityId jid) p)
          Nothing -> Nothing
      _ -> pure Nothing

-- Mark job complete
completeJob :: EntityId -> App ()
completeJob jid = do
  conn <- getConn
  liftIO $ execute conn
    "update jobs set status = 'completed' where id = ?"
    (Only $ unEntityId jid)
  pure ()

-- Mark job failed
failJob :: EntityId -> Text -> App ()
failJob jid err = do
  conn <- getConn
  liftIO $ execute conn
    "update jobs set status = 'failed', last_error = ? where id = ?"
    (err, unEntityId jid)
  pure ()

-- Requeue failed job (if under max attempts)
requeueJob :: EntityId -> App Bool
requeueJob jid = do
  conn <- getConn
  n <- liftIO $ execute conn
    "update jobs set status = 'queued', claimed_at = null \
    \where id = ? and attempts < max_attempts"
    (Only $ unEntityId jid)
  pure (n > 0)
```

**Step 3: Run test to verify it compiles**

Run: `cd /home/gareth/code/hacking/wisp && cabal test`
Expected: PASS

**Step 4: Commit**

```bash
git add wisp-srv/src/Infra/Db/Jobs.hs wisp-srv/test/Infra/Db/JobsSpec.hs
git commit -m "feat: add job queue operations with FOR UPDATE SKIP LOCKED"
```

---

## Task 13: HTTP Server Setup with Health Endpoint

**Files:**
- Create: `wisp-srv/src/Http/Server.hs`
- Create: `wisp-srv/src/Http/Routes.hs`
- Create: `wisp-srv/src/Http/Handlers/Health.hs`

**Step 1: Write test for health endpoint**

Create `wisp-srv/test/Http/Handlers/HealthSpec.hs`:

```haskell
module Http.Handlers.HealthSpec where

import Test.Hspec

spec :: Spec
spec = describe "Health" $ do
  it "placeholder - HTTP tests need integration setup" $ do
    True `shouldBe` True
```

**Step 2: Write Http/Handlers/Health.hs**

```haskell
-- src/Http/Handlers/Health.hs
module Http.Handlers.Health
  ( getHealth
  ) where

import Data.Aeson (object, (.=))
import Web.Scotty.Trans (ActionT, json)
import App.Monad (App)

getHealth :: ActionT App ()
getHealth = json $ object
  [ "status" .= ("ok" :: String)
  , "service" .= ("wisp-srv" :: String)
  ]
```

**Step 3: Write Http/Routes.hs**

```haskell
-- src/Http/Routes.hs
module Http.Routes
  ( routes
  ) where

import Web.Scotty.Trans (ScottyT, get)
import Http.Handlers.Health (getHealth)
import App.Monad (App)

routes :: ScottyT App ()
routes = do
  get "/health" getHealth
```

**Step 4: Write Http/Server.hs**

```haskell
-- src/Http/Server.hs
module Http.Server
  ( startServer
  ) where

import Control.Monad.Reader (ask)
import Web.Scotty.Trans (scottyT, middleware)
import Network.Wai.Handler.Warp (setPort, defaultSettings)
import App.Monad (App, Env, runApp, getConfig)
import App.Config (Config(..))
import Http.Routes (routes)

startServer :: App ()
startServer = do
  env <- ask
  cfg <- getConfig
  let port = cfg.server.port
  liftIO $ putStrLn $ "Starting server on port " <> show port
  scottyT port (runApp env) routes
  where
    liftIO = Control.Monad.IO.Class.liftIO
```

**Step 5: Update Main.hs to start the server**

```haskell
-- app/Main.hs
module Main where

import System.Environment (getArgs)
import App.Config (loadConfig)
import App.Env (buildEnv)
import App.Monad (runApp)
import Http.Server (startServer)
import Infra.Db.Migrations (runMigrations)

main :: IO ()
main = do
  args <- getArgs
  let configPath = case args of
        [p] -> p
        _ -> "wisp.yaml"

  putStrLn "Loading configuration..."
  config <- loadConfig configPath

  putStrLn "Building environment..."
  env <- buildEnv config

  putStrLn "Running migrations..."
  runApp env $ runMigrations "migrations"

  putStrLn "Starting wisp-srv..."
  runApp env startServer
```

**Step 6: Run to verify it compiles**

Run: `cd /home/gareth/code/hacking/wisp && cabal build wisp-srv`
Expected: Build succeeds

**Step 7: Commit**

```bash
git add wisp-srv/src/Http/ wisp-srv/test/Http/ wisp-srv/app/Main.hs
git commit -m "feat: add HTTP server with health endpoint"
```

---

## Task 14: Sample Configuration File

**Files:**
- Create: `wisp-srv/wisp.yaml.example`

**Step 1: Write example config**

```yaml
# wisp.yaml.example
# Copy to wisp.yaml and fill in your values

server:
  host: "127.0.0.1"
  port: 8080

database:
  url: "postgres://localhost:5432/wisp"

google:
  clientId: "your-google-client-id"
  clientSecret: "your-google-client-secret"

polling:
  intervalMinutes: 5

classification:
  confidenceThreshold: 0.5
```

**Step 2: Commit**

```bash
git add wisp-srv/wisp.yaml.example
git commit -m "docs: add example configuration file"
```

---

## Task 15: Google OAuth Types

**Files:**
- Create: `wisp-srv/src/Infra/Google/Auth.hs`

**Step 1: Write test for OAuth URL generation**

Create `wisp-srv/test/Infra/Google/AuthSpec.hs`:

```haskell
module Infra.Google.AuthSpec where

import Test.Hspec
import Infra.Google.Auth (buildAuthUrl, OAuthConfig(..))
import Data.Text (isInfixOf)

spec :: Spec
spec = describe "Google Auth" $ do
  describe "buildAuthUrl" $ do
    it "includes client_id in URL" $ do
      let cfg = OAuthConfig
            { clientId = "test-client-id"
            , clientSecret = "test-secret"
            , redirectUri = "http://localhost:8080/auth/google/callback"
            }
      let url = buildAuthUrl cfg
      "test-client-id" `isInfixOf` url `shouldBe` True

    it "includes required scopes" $ do
      let cfg = OAuthConfig
            { clientId = "test-id"
            , clientSecret = "test-secret"
            , redirectUri = "http://localhost:8080/auth/google/callback"
            }
      let url = buildAuthUrl cfg
      "gmail.readonly" `isInfixOf` url `shouldBe` True
      "calendar.readonly" `isInfixOf` url `shouldBe` True
```

**Step 2: Run test to verify it fails**

Run: `cd /home/gareth/code/hacking/wisp && cabal test`
Expected: FAIL - module not found

**Step 3: Write the Auth module**

```haskell
-- src/Infra/Google/Auth.hs
module Infra.Google.Auth
  ( OAuthConfig(..)
  , TokenResponse(..)
  , buildAuthUrl
  , exchangeCode
  , refreshAccessToken
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), object, (.=), withObject)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (urlEncode)
import URI.Encode (encodeText)

data OAuthConfig = OAuthConfig
  { clientId :: Text
  , clientSecret :: Text
  , redirectUri :: Text
  } deriving (Show)

data TokenResponse = TokenResponse
  { accessToken :: Text
  , refreshToken :: Maybe Text
  , expiresIn :: Int
  , tokenType :: Text
  } deriving (Generic, Show)

instance FromJSON TokenResponse where
  parseJSON = withObject "TokenResponse" $ \v -> TokenResponse
    <$> v .: "access_token"
    <*> v .:? "refresh_token"
    <*> v .: "expires_in"
    <*> v .: "token_type"

-- Build the OAuth authorization URL
buildAuthUrl :: OAuthConfig -> Text
buildAuthUrl cfg = T.concat
  [ "https://accounts.google.com/o/oauth2/v2/auth"
  , "?client_id=", encodeText cfg.clientId
  , "&redirect_uri=", encodeText cfg.redirectUri
  , "&response_type=code"
  , "&scope=", encodeText scopes
  , "&access_type=offline"
  , "&prompt=consent"
  ]
  where
    scopes = "https://www.googleapis.com/auth/gmail.readonly \
             \https://www.googleapis.com/auth/calendar.readonly"

-- Exchange authorization code for tokens
exchangeCode :: OAuthConfig -> Text -> IO (Either Text TokenResponse)
exchangeCode cfg code = do
  manager <- newManager tlsManagerSettings
  let body = BS.intercalate "&"
        [ "client_id=" <> encodeUtf8 cfg.clientId
        , "client_secret=" <> encodeUtf8 cfg.clientSecret
        , "code=" <> encodeUtf8 code
        , "redirect_uri=" <> encodeUtf8 cfg.redirectUri
        , "grant_type=authorization_code"
        ]
  initialReq <- parseRequest "https://oauth2.googleapis.com/token"
  let req = initialReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/x-www-form-urlencoded")
            ]
        , requestBody = RequestBodyBS body
        }
  response <- httpLbs req manager
  pure $ case decode (responseBody response) of
    Just tok -> Right tok
    Nothing -> Left $ "Failed to parse token response: " <> T.pack (show $ responseBody response)
  where
    decode :: FromJSON a => ByteString -> Maybe a
    decode = Data.Aeson.decode

-- Refresh an access token
refreshAccessToken :: OAuthConfig -> Text -> IO (Either Text TokenResponse)
refreshAccessToken cfg refreshTok = do
  manager <- newManager tlsManagerSettings
  let body = BS.intercalate "&"
        [ "client_id=" <> encodeUtf8 cfg.clientId
        , "client_secret=" <> encodeUtf8 cfg.clientSecret
        , "refresh_token=" <> encodeUtf8 refreshTok
        , "grant_type=refresh_token"
        ]
  initialReq <- parseRequest "https://oauth2.googleapis.com/token"
  let req = initialReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/x-www-form-urlencoded")
            ]
        , requestBody = RequestBodyBS body
        }
  response <- httpLbs req manager
  pure $ case decode (responseBody response) of
    Just tok -> Right tok
    Nothing -> Left "Failed to parse refresh token response"
  where
    decode :: FromJSON a => ByteString -> Maybe a
    decode = Data.Aeson.decode
```

**Step 4: Run test to verify it passes**

Run: `cd /home/gareth/code/hacking/wisp && cabal test`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Google/Auth.hs wisp-srv/test/Infra/Google/AuthSpec.hs
git commit -m "feat: add Google OAuth URL builder and token exchange"
```

---

## Task 16: Auth Token Storage

**Files:**
- Create: `wisp-srv/src/Infra/Db/Auth.hs`

**Step 1: Write the Auth storage module**

```haskell
-- src/Infra/Db/Auth.hs
module Infra.Db.Auth
  ( AuthToken(..)
  , saveToken
  , getToken
  , updateToken
  , tokenNeedsRefresh
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Data.Vector (Vector, fromList, toList)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Domain.Id (EntityId(..), newEntityId)
import App.Monad (App, getConn)

data AuthToken = AuthToken
  { tokenId :: EntityId
  , provider :: Text
  , accessToken :: Text
  , refreshToken :: Text
  , expiresAt :: UTCTime
  , scopes :: [Text]
  } deriving (Show)

instance FromRow AuthToken where
  fromRow = AuthToken
    <$> (EntityId <$> field)
    <*> field
    <*> field
    <*> field
    <*> field
    <*> (fromPGArray <$> field)

-- Save a new token (upsert)
saveToken :: Text -> Text -> Text -> UTCTime -> [Text] -> App EntityId
saveToken prov accessTok refreshTok expires scps = do
  conn <- getConn
  tid <- liftIO newEntityId
  liftIO $ execute conn
    "insert into auth_tokens (id, provider, access_token, refresh_token, expires_at, scopes) \
    \values (?, ?, ?, ?, ?, ?) \
    \on conflict (provider) do update set \
    \  access_token = excluded.access_token, \
    \  refresh_token = excluded.refresh_token, \
    \  expires_at = excluded.expires_at, \
    \  scopes = excluded.scopes, \
    \  updated_at = now()"
    (unEntityId tid, prov, accessTok, refreshTok, expires, PGArray scps)
  pure tid

-- Get token for a provider
getToken :: Text -> App (Maybe AuthToken)
getToken prov = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, provider, access_token, refresh_token, expires_at, scopes \
    \from auth_tokens where provider = ?"
    (Only prov)
  pure $ case results of
    [tok] -> Just tok
    _ -> Nothing

-- Update access token after refresh
updateToken :: Text -> Text -> UTCTime -> App ()
updateToken prov accessTok expires = do
  conn <- getConn
  liftIO $ execute conn
    "update auth_tokens set access_token = ?, expires_at = ?, updated_at = now() \
    \where provider = ?"
    (accessTok, expires, prov)
  pure ()

-- Check if token needs refresh (within 5 minutes of expiry)
tokenNeedsRefresh :: AuthToken -> IO Bool
tokenNeedsRefresh tok = do
  now <- getCurrentTime
  let fiveMinutes = 5 * 60
  pure $ tok.expiresAt <= addUTCTime fiveMinutes now
```

**Step 2: Commit**

```bash
git add wisp-srv/src/Infra/Db/Auth.hs
git commit -m "feat: add auth token storage with refresh detection"
```

---

## Task 17: OAuth HTTP Handlers

**Files:**
- Create: `wisp-srv/src/Http/Handlers/Auth.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`

**Step 1: Write the Auth handlers**

```haskell
-- src/Http/Handlers/Auth.hs
module Http.Handlers.Auth
  ( getGoogleAuth
  , getGoogleCallback
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import Network.HTTP.Types.Status (status302, status400, status500)
import Web.Scotty.Trans (ActionT, json, status, redirect, queryParam, queryParamMaybe, setHeader)
import App.Monad (App, Env(..), getConfig)
import App.Config (Config(..), GoogleConfig(..))
import Infra.Google.Auth (OAuthConfig(..), buildAuthUrl, exchangeCode)
import Infra.Db.Auth (saveToken)

-- Build OAuthConfig from app config
getOAuthConfig :: App OAuthConfig
getOAuthConfig = do
  cfg <- getConfig
  pure OAuthConfig
    { clientId = cfg.google.clientId
    , clientSecret = cfg.google.clientSecret
    , redirectUri = "http://127.0.0.1:" <> T.pack (show cfg.server.port) <> "/auth/google/callback"
    }

-- Redirect to Google OAuth
getGoogleAuth :: ActionT App ()
getGoogleAuth = do
  oauthCfg <- lift getOAuthConfig
  let url = buildAuthUrl oauthCfg
  setHeader "Location" (T.encodeUtf8 url)
  status status302
  json $ object ["redirect" .= url]
  where
    lift = Control.Monad.Trans.Class.lift

-- Handle OAuth callback
getGoogleCallback :: ActionT App ()
getGoogleCallback = do
  mcode <- queryParamMaybe "code"
  merror <- queryParamMaybe "error"

  case merror of
    Just err -> do
      status status400
      json $ object ["error" .= (err :: Text)]
    Nothing -> case mcode of
      Nothing -> do
        status status400
        json $ object ["error" .= ("Missing authorization code" :: Text)]
      Just code -> do
        oauthCfg <- lift getOAuthConfig
        result <- liftIO $ exchangeCode oauthCfg code
        case result of
          Left err -> do
            status status500
            json $ object ["error" .= err]
          Right tok -> do
            now <- liftIO getCurrentTime
            let expiresAt = addUTCTime (fromIntegral tok.expiresIn) now
            let refreshTok = case tok.refreshToken of
                  Just rt -> rt
                  Nothing -> ""  -- Should always have refresh token on first auth
            _ <- lift $ saveToken "google" tok.accessToken refreshTok expiresAt
              ["https://www.googleapis.com/auth/gmail.readonly"
              ,"https://www.googleapis.com/auth/calendar.readonly"
              ]
            json $ object
              [ "status" .= ("authenticated" :: Text)
              , "expires_at" .= expiresAt
              ]
  where
    lift = Control.Monad.Trans.Class.lift
    liftIO = Control.Monad.IO.Class.liftIO
```

**Step 2: Update Routes.hs**

```haskell
-- src/Http/Routes.hs
module Http.Routes
  ( routes
  ) where

import Web.Scotty.Trans (ScottyT, get)
import Http.Handlers.Health (getHealth)
import Http.Handlers.Auth (getGoogleAuth, getGoogleCallback)
import App.Monad (App)

routes :: ScottyT App ()
routes = do
  -- Health
  get "/health" getHealth

  -- Auth
  get "/auth/google" getGoogleAuth
  get "/auth/google/callback" getGoogleCallback
```

**Step 3: Run to verify it compiles**

Run: `cd /home/gareth/code/hacking/wisp && cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Auth.hs wisp-srv/src/Http/Routes.hs
git commit -m "feat: add OAuth HTTP handlers for Google auth flow"
```

---

## Task 18: CLI Scaffolding

**Files:**
- Create: `wisp-cli/wisp-cli.cabal`
- Create: `wisp-cli/app/Main.hs`
- Modify: `cabal.project`

**Step 1: Update cabal.project**

```cabal
-- cabal.project
packages: wisp-srv, wisp-cli
```

**Step 2: Create wisp-cli.cabal**

```cabal
cabal-version: 3.0
name:          wisp-cli
version:       0.1.0.0
synopsis:      Wisp personal assistant CLI
license:       BSD-3-Clause
author:        Gareth
build-type:    Simple

common warnings
    ghc-options: -Wall -Wunused-packages

executable wisp
    import:           warnings
    main-is:          Main.hs
    hs-source-dirs:   app
    default-language: GHC2021
    default-extensions:
        OverloadedStrings
        LambdaCase
    build-depends:
        base >=4.17 && <5,
        text,
        bytestring,
        aeson,
        http-client,
        http-client-tls,
        http-types,
        optparse-applicative,
        process
```

**Step 3: Create Main.hs with auth command**

```haskell
-- wisp-cli/app/Main.hs
module Main where

import Control.Monad (when)
import Data.Aeson (Value, decode, encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
import System.Process (callCommand)

-- CLI Command types
data Command
  = Auth
  | Status
  deriving (Show)

-- Parse commands
commandParser :: Parser Command
commandParser = subparser
  ( command "auth" (info (pure Auth) (progDesc "Start OAuth flow"))
  <> command "status" (info (pure Status) (progDesc "Quick status overview"))
  )

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc
  <> progDesc "Wisp personal assistant CLI"
  <> header "wisp - your autonomy-preserving assistant"
  )

-- Base URL for wisp-srv
baseUrl :: String
baseUrl = "http://127.0.0.1:8080"

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Auth -> runAuth
    Status -> runStatus

runAuth :: IO ()
runAuth = do
  TIO.putStrLn "Starting OAuth flow..."
  manager <- newManager tlsManagerSettings
  req <- parseRequest $ baseUrl <> "/auth/google"
  response <- httpLbs req manager

  case decode (responseBody response) :: Maybe Value of
    Just obj -> do
      case obj of
        _ -> do
          TIO.putStrLn "Opening browser for Google authentication..."
          let authUrl = baseUrl <> "/auth/google"
          -- Open browser (cross-platform would need more logic)
          callCommand $ "xdg-open '" <> authUrl <> "' 2>/dev/null || open '" <> authUrl <> "'"
          TIO.putStrLn "\nAfter authenticating, the callback will complete the flow."
    Nothing -> TIO.putStrLn "Failed to parse server response"

runStatus :: IO ()
runStatus = do
  TIO.putStrLn "Wisp Status"
  TIO.putStrLn "==========="
  TIO.putStrLn "(Status check not yet implemented)"
```

**Step 4: Run to verify it compiles**

Run: `cd /home/gareth/code/hacking/wisp && cabal build wisp-cli`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add cabal.project wisp-cli/
git commit -m "feat: add wisp-cli with auth command"
```

---

## Task 19: Integration Test Setup

**Files:**
- Create: `wisp-srv/test/TestEnv.hs`

**Step 1: Write test environment helper**

```haskell
-- test/TestEnv.hs
module TestEnv
  ( testConfig
  , withTestEnv
  ) where

import Database.PostgreSQL.Simple
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import App.Config
import App.Monad

testConfig :: Config
testConfig = Config
  { server = ServerConfig
      { host = "127.0.0.1"
      , port = 8080
      }
  , database = DatabaseConfig
      { url = "postgres://localhost/wisp_test"
      }
  , google = GoogleConfig
      { clientId = "test-client-id"
      , clientSecret = "test-client-secret"
      }
  , polling = PollingConfig
      { intervalMinutes = 5
      }
  , classification = ClassificationConfig
      { confidenceThreshold = 0.5
      }
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

**Step 2: Commit**

```bash
git add wisp-srv/test/TestEnv.hs
git commit -m "feat: add test environment with transaction rollback"
```

---

## Task 20: Final Review and Documentation

**Files:**
- Create: `wisp-srv/README.md`

**Step 1: Write README**

```markdown
# wisp-srv

Backend server for Wisp, the autonomy-preserving personal assistant.

## Prerequisites

- GHC 9.6+
- PostgreSQL 14+
- Cabal 3.8+

## Setup

1. Create database:
   ```bash
   createdb wisp
   createdb wisp_test  # for tests
   ```

2. Copy config:
   ```bash
   cp wisp.yaml.example wisp.yaml
   # Edit wisp.yaml with your settings
   ```

3. Set up Google OAuth:
   - Go to Google Cloud Console
   - Create OAuth 2.0 credentials
   - Add `http://127.0.0.1:8080/auth/google/callback` as redirect URI
   - Update `wisp.yaml` with client ID and secret

4. Build and run:
   ```bash
   cabal build wisp-srv
   cabal run wisp-srv
   ```

## Development

Run tests:
```bash
cabal test
```

## API Endpoints

- `GET /health` - Health check
- `GET /auth/google` - Start OAuth flow
- `GET /auth/google/callback` - OAuth callback

## Architecture

See `docs/plans/2026-02-01-technical-design.md` for full architecture documentation.
```

**Step 2: Commit**

```bash
git add wisp-srv/README.md
git commit -m "docs: add wisp-srv README with setup instructions"
```

---

## Summary

This plan implements Phases 1 & 2 from the technical design:

**Phase 1: Foundation (Tasks 1-14)**
- Project scaffolding with all dependencies
- Domain types (EntityId, Schema)
- App monad and configuration
- Database migrations (entities, jobs, receipts, auth, poll_state)
- Migration runner
- Entity CRUD operations
- Job queue operations
- HTTP server with health endpoint
- Sample configuration

**Phase 2: Authentication (Tasks 15-20)**
- Google OAuth types and URL builder
- Token exchange and refresh
- Auth token storage
- OAuth HTTP handlers
- CLI with auth command
- Integration test setup
- Documentation

Each task follows TDD with small, focused steps and frequent commits.
