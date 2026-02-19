# Documents Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a documents table for projects, notes, and preferences with a log connecting activities to documents.

**Architecture:** Single `documents` table with type discriminator and JSONB data. `document_log` table links activities to documents. Domain types use sum types for document type, DB layer handles serialization.

**Tech Stack:** Haskell, PostgreSQL, postgresql-simple, hspec for tests

---

## Task 1: Database Migration

**Files:**
- Create: `wisp-srv/migrations/018_documents.sql`

**Step 1: Write the migration**

```sql
-- 018_documents.sql
-- Documents table for projects, notes, preferences

CREATE TABLE documents (
  id TEXT PRIMARY KEY,
  tenant_id UUID REFERENCES tenants(id),
  type TEXT NOT NULL CHECK (type IN ('project', 'note', 'preference')),
  data JSONB NOT NULL,
  tags TEXT[] NOT NULL DEFAULT '{}',
  confidence FLOAT,
  source TEXT,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  archived_at TIMESTAMPTZ,
  supersedes_id TEXT REFERENCES documents(id),
  last_activity_at TIMESTAMPTZ
);

CREATE INDEX idx_documents_tenant ON documents(tenant_id);
CREATE INDEX idx_documents_type ON documents(type);
CREATE INDEX idx_documents_tags ON documents USING GIN(tags);
CREATE INDEX idx_documents_active ON documents(active) WHERE active = TRUE;
CREATE INDEX idx_documents_supersedes ON documents(supersedes_id);

CREATE TABLE document_log (
  id TEXT PRIMARY KEY,
  document_id TEXT NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
  activity_id TEXT REFERENCES activities(id) ON DELETE SET NULL,
  description TEXT,
  source TEXT NOT NULL CHECK (source IN ('agent', 'user', 'classifier', 'reflection')),
  agent_id TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_document_log_document ON document_log(document_id);
CREATE INDEX idx_document_log_activity ON document_log(activity_id);
CREATE INDEX idx_document_log_created ON document_log(created_at DESC);
```

**Step 2: Verify migration syntax**

Run: `psql -d wisp -f wisp-srv/migrations/018_documents.sql`
Expected: CREATE TABLE, CREATE INDEX messages

**Step 3: Record migration**

Run: `psql -d wisp -c "INSERT INTO schema_migrations (version) VALUES (18);"`

**Step 4: Commit**

```bash
git add wisp-srv/migrations/018_documents.sql
git commit -m "feat(db): add documents and document_log tables"
```

---

## Task 2: Domain Types

**Files:**
- Create: `wisp-srv/src/Domain/Document.hs`
- Modify: `wisp-srv/wisp-srv.cabal` (add to other-modules)

**Step 1: Write the domain types**

```haskell
-- src/Domain/Document.hs
module Domain.Document
  ( Document(..)
  , NewDocument(..)
  , DocumentType(..)
  , ProjectType(..)
  , ProjectData(..)
  , NoteData(..)
  , PreferenceData(..)
  , DocumentLogEntry(..)
  , NewDocumentLogEntry(..)
  , LogSource(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value, object, withObject, withText, (.:), (.:?), (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Id (EntityId(..), unEntityId)
import Domain.Tenant (TenantId)
import GHC.Generics (Generic)

-- Project types
data ProjectType = Work | Personal | Family | Health | Spiritual
  deriving (Eq, Show, Generic)

instance ToJSON ProjectType where
  toJSON Work = "work"
  toJSON Personal = "personal"
  toJSON Family = "family"
  toJSON Health = "health"
  toJSON Spiritual = "spiritual"

instance FromJSON ProjectType where
  parseJSON = withText "ProjectType" $ \case
    "work" -> pure Work
    "personal" -> pure Personal
    "family" -> pure Family
    "health" -> pure Health
    "spiritual" -> pure Spiritual
    other -> fail $ "Unknown project type: " <> show other

-- Document type discriminator
data DocumentType = ProjectDoc | NoteDoc | PreferenceDoc
  deriving (Eq, Show, Generic)

instance ToJSON DocumentType where
  toJSON ProjectDoc = "project"
  toJSON NoteDoc = "note"
  toJSON PreferenceDoc = "preference"

instance FromJSON DocumentType where
  parseJSON = withText "DocumentType" $ \case
    "project" -> pure ProjectDoc
    "note" -> pure NoteDoc
    "preference" -> pure PreferenceDoc
    other -> fail $ "Unknown document type: " <> show other

-- Type-specific data
data ProjectData = ProjectData
  { projectName :: Text
  , projectType :: ProjectType
  } deriving (Eq, Show, Generic)

instance ToJSON ProjectData where
  toJSON p = object ["name" .= projectName p, "type" .= projectType p]

instance FromJSON ProjectData where
  parseJSON = withObject "ProjectData" $ \v ->
    ProjectData <$> v .: "name" <*> v .: "type"

data NoteData = NoteData
  { noteTitle :: Text
  , noteContent :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON NoteData where
  toJSON n = object ["title" .= noteTitle n, "content" .= noteContent n]

instance FromJSON NoteData where
  parseJSON = withObject "NoteData" $ \v ->
    NoteData <$> v .: "title" <*> v .:? "content"

data PreferenceData = PreferenceData
  { prefKey :: Text
  , prefValue :: Text
  , prefContext :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON PreferenceData where
  toJSON p = object ["key" .= prefKey p, "value" .= prefValue p, "context" .= prefContext p]

instance FromJSON PreferenceData where
  parseJSON = withObject "PreferenceData" $ \v ->
    PreferenceData <$> v .: "key" <*> v .: "value" <*> v .:? "context"

-- Main document type
data Document = Document
  { documentId :: EntityId
  , documentTenantId :: Maybe TenantId
  , documentType :: DocumentType
  , documentData :: Value  -- Raw JSONB, parsed per-type as needed
  , documentTags :: [Text]
  , documentConfidence :: Maybe Double
  , documentSource :: Maybe Text
  , documentActive :: Bool
  , documentCreatedAt :: UTCTime
  , documentArchivedAt :: Maybe UTCTime
  , documentSupersedesId :: Maybe EntityId
  , documentLastActivityAt :: Maybe UTCTime
  } deriving (Eq, Show)

instance ToJSON Document where
  toJSON d = object
    [ "id" .= unEntityId (documentId d)
    , "tenant_id" .= documentTenantId d
    , "type" .= documentType d
    , "data" .= documentData d
    , "tags" .= documentTags d
    , "confidence" .= documentConfidence d
    , "source" .= documentSource d
    , "active" .= documentActive d
    , "created_at" .= documentCreatedAt d
    , "archived_at" .= documentArchivedAt d
    , "supersedes_id" .= fmap unEntityId (documentSupersedesId d)
    , "last_activity_at" .= documentLastActivityAt d
    ]

-- For creating new documents
data NewDocument = NewDocument
  { newDocTenantId :: Maybe TenantId
  , newDocType :: DocumentType
  , newDocData :: Value
  , newDocTags :: [Text]
  , newDocConfidence :: Maybe Double
  , newDocSource :: Maybe Text
  , newDocSupersedesId :: Maybe EntityId
  } deriving (Eq, Show)

-- Log entry source
data LogSource = LogAgent | LogUser | LogClassifier | LogReflection
  deriving (Eq, Show, Generic)

instance ToJSON LogSource where
  toJSON LogAgent = "agent"
  toJSON LogUser = "user"
  toJSON LogClassifier = "classifier"
  toJSON LogReflection = "reflection"

instance FromJSON LogSource where
  parseJSON = withText "LogSource" $ \case
    "agent" -> pure LogAgent
    "user" -> pure LogUser
    "classifier" -> pure LogClassifier
    "reflection" -> pure LogReflection
    other -> fail $ "Unknown log source: " <> show other

-- Document log entry
data DocumentLogEntry = DocumentLogEntry
  { logId :: EntityId
  , logDocumentId :: EntityId
  , logActivityId :: Maybe EntityId
  , logDescription :: Maybe Text
  , logSource :: LogSource
  , logAgentId :: Maybe Text
  , logCreatedAt :: UTCTime
  } deriving (Eq, Show)

instance ToJSON DocumentLogEntry where
  toJSON l = object
    [ "id" .= unEntityId (logId l)
    , "document_id" .= unEntityId (logDocumentId l)
    , "activity_id" .= fmap unEntityId (logActivityId l)
    , "description" .= logDescription l
    , "source" .= logSource l
    , "agent_id" .= logAgentId l
    , "created_at" .= logCreatedAt l
    ]

-- For creating new log entries
data NewDocumentLogEntry = NewDocumentLogEntry
  { newLogDocumentId :: EntityId
  , newLogActivityId :: Maybe EntityId
  , newLogDescription :: Maybe Text
  , newLogSource :: LogSource
  , newLogAgentId :: Maybe Text
  } deriving (Eq, Show)
```

**Step 2: Add to cabal file**

Add `Domain.Document` to `other-modules` in `wisp-srv.cabal`.

**Step 3: Verify it compiles**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Domain/Document.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(domain): add Document types"
```

---

## Task 3: Domain Tests

**Files:**
- Create: `wisp-srv/test/Domain/DocumentSpec.hs`
- Modify: `wisp-srv/test/Spec.hs` (import new spec)

**Step 1: Write the tests**

```haskell
-- test/Domain/DocumentSpec.hs
module Domain.DocumentSpec where

import Test.Hspec
import Domain.Document
import Data.Aeson (encode, decode, toJSON)
import qualified Data.ByteString.Lazy.Char8 as LBS

spec :: Spec
spec = describe "Document" $ do
  describe "DocumentType" $ do
    it "serializes ProjectDoc to 'project'" $ do
      LBS.unpack (encode ProjectDoc) `shouldBe` "\"project\""

    it "serializes NoteDoc to 'note'" $ do
      LBS.unpack (encode NoteDoc) `shouldBe` "\"note\""

    it "serializes PreferenceDoc to 'preference'" $ do
      LBS.unpack (encode PreferenceDoc) `shouldBe` "\"preference\""

    it "deserializes 'project' to ProjectDoc" $ do
      decode "\"project\"" `shouldBe` Just ProjectDoc

  describe "ProjectType" $ do
    it "serializes Work to 'work'" $ do
      LBS.unpack (encode Work) `shouldBe` "\"work\""

    it "deserializes 'health' to Health" $ do
      decode "\"health\"" `shouldBe` Just Health

  describe "ProjectData" $ do
    it "serializes with name and type" $ do
      let proj = ProjectData "Gym" Health
          json = LBS.unpack (encode proj)
      json `shouldContain` "\"name\""
      json `shouldContain` "\"Gym\""
      json `shouldContain` "\"type\""
      json `shouldContain` "\"health\""

    it "deserializes from JSON" $ do
      let json = "{\"name\":\"Gym\",\"type\":\"health\"}"
          Just proj = decode json :: Maybe ProjectData
      projectName proj `shouldBe` "Gym"
      projectType proj `shouldBe` Health

  describe "NoteData" $ do
    it "serializes with title and content" $ do
      let note = NoteData "Alice is my sister" (Just "Works at Google")
          json = LBS.unpack (encode note)
      json `shouldContain` "\"title\""
      json `shouldContain` "\"Alice is my sister\""

    it "handles null content" $ do
      let note = NoteData "Simple note" Nothing
          json = encode note
          Just decoded = decode json :: Maybe NoteData
      noteContent decoded `shouldBe` Nothing

  describe "PreferenceData" $ do
    it "serializes with key, value, context" $ do
      let pref = PreferenceData "meeting_time" "10am-12pm" (Just "scheduling")
          json = LBS.unpack (encode pref)
      json `shouldContain` "\"key\""
      json `shouldContain` "\"meeting_time\""

  describe "LogSource" $ do
    it "serializes LogUser to 'user'" $ do
      LBS.unpack (encode LogUser) `shouldBe` "\"user\""

    it "deserializes 'agent' to LogAgent" $ do
      decode "\"agent\"" `shouldBe` Just LogAgent
```

**Step 2: Update test/Spec.hs**

Add import for `Domain.DocumentSpec`.

**Step 3: Run tests**

Run: `cabal test wisp-srv`
Expected: All tests pass

**Step 4: Commit**

```bash
git add wisp-srv/test/Domain/DocumentSpec.hs wisp-srv/test/Spec.hs
git commit -m "test(domain): add Document type tests"
```

---

## Task 4: Database Layer

**Files:**
- Create: `wisp-srv/src/Infra/Db/Document.hs`
- Modify: `wisp-srv/wisp-srv.cabal` (add to other-modules)

**Step 1: Write the database layer**

```haskell
-- src/Infra/Db/Document.hs
module Infra.Db.Document
  ( insertDocument
  , getDocumentById
  , getDocumentsByType
  , getDocumentsByTags
  , getActiveDocuments
  , updateDocumentLastActivity
  , archiveDocument
  , insertDocumentLog
  , getDocumentLog
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField (fromField, returnError, Conversion, FromField(..))
import Database.PostgreSQL.Simple.ToField (toField, ToField(..))
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Domain.Id (EntityId(..), newEntityId)
import Domain.Document
import Domain.Tenant (TenantId(..))
import App.Monad (App, getConn)

-- ToField instances
instance ToField DocumentType where
  toField ProjectDoc = toField ("project" :: Text)
  toField NoteDoc = toField ("note" :: Text)
  toField PreferenceDoc = toField ("preference" :: Text)

instance ToField LogSource where
  toField LogAgent = toField ("agent" :: Text)
  toField LogUser = toField ("user" :: Text)
  toField LogClassifier = toField ("classifier" :: Text)
  toField LogReflection = toField ("reflection" :: Text)

-- FromRow for Document
instance FromRow Document where
  fromRow = Document
    <$> (EntityId <$> field)                    -- id
    <*> (fmap TenantId <$> field)               -- tenant_id
    <*> typeField                               -- type
    <*> field                                   -- data (JSONB -> Value)
    <*> (fromPGArray <$> field)                 -- tags
    <*> field                                   -- confidence
    <*> field                                   -- source
    <*> field                                   -- active
    <*> field                                   -- created_at
    <*> field                                   -- archived_at
    <*> (fmap EntityId <$> field)               -- supersedes_id
    <*> field                                   -- last_activity_at
    where
      typeField = fieldWith $ \f mbs -> do
        txt <- fromField f mbs :: Conversion Text
        case txt of
          "project" -> pure ProjectDoc
          "note" -> pure NoteDoc
          "preference" -> pure PreferenceDoc
          other -> returnError ConversionFailed f $ "Unknown document type: " <> T.unpack other

-- FromRow for DocumentLogEntry
instance FromRow DocumentLogEntry where
  fromRow = DocumentLogEntry
    <$> (EntityId <$> field)                    -- id
    <*> (EntityId <$> field)                    -- document_id
    <*> (fmap EntityId <$> field)               -- activity_id
    <*> field                                   -- description
    <*> sourceField                             -- source
    <*> field                                   -- agent_id
    <*> field                                   -- created_at
    where
      sourceField = fieldWith $ \f mbs -> do
        txt <- fromField f mbs :: Conversion Text
        case txt of
          "agent" -> pure LogAgent
          "user" -> pure LogUser
          "classifier" -> pure LogClassifier
          "reflection" -> pure LogReflection
          other -> returnError ConversionFailed f $ "Unknown log source: " <> T.unpack other

-- Insert a new document
insertDocument :: NewDocument -> App EntityId
insertDocument new = do
  conn <- getConn
  docId <- liftIO newEntityId
  let dataJson = LBS.toStrict $ encode (newDocData new)
  _ <- liftIO $ execute conn
    "INSERT INTO documents (id, tenant_id, type, data, tags, confidence, source, supersedes_id) \
    \VALUES (?, ?, ?, ?::jsonb, ?, ?, ?, ?)"
    ( unEntityId docId
    , fmap (\(TenantId u) -> u) (newDocTenantId new)
    , newDocType new
    , dataJson
    , PGArray (newDocTags new)
    , newDocConfidence new
    , newDocSource new
    , fmap unEntityId (newDocSupersedesId new)
    )
  pure docId

-- Get document by ID
getDocumentById :: EntityId -> App (Maybe Document)
getDocumentById docId = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
    \created_at, archived_at, supersedes_id, last_activity_at \
    \FROM documents WHERE id = ?"
    (Only $ unEntityId docId)
  pure $ case results of
    [doc] -> Just doc
    _ -> Nothing

-- Get documents by type
getDocumentsByType :: DocumentType -> Bool -> Int -> App [Document]
getDocumentsByType docType activeOnly limit = do
  conn <- getConn
  if activeOnly
    then liftIO $ query conn
      "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
      \created_at, archived_at, supersedes_id, last_activity_at \
      \FROM documents WHERE type = ? AND active = TRUE \
      \ORDER BY last_activity_at DESC NULLS LAST, created_at DESC \
      \LIMIT ?"
      (docType, limit)
    else liftIO $ query conn
      "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
      \created_at, archived_at, supersedes_id, last_activity_at \
      \FROM documents WHERE type = ? \
      \ORDER BY created_at DESC \
      \LIMIT ?"
      (docType, limit)

-- Get documents by tags (any match)
getDocumentsByTags :: [Text] -> Bool -> Int -> App [Document]
getDocumentsByTags tags activeOnly limit = do
  conn <- getConn
  if activeOnly
    then liftIO $ query conn
      "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
      \created_at, archived_at, supersedes_id, last_activity_at \
      \FROM documents WHERE tags && ? AND active = TRUE \
      \ORDER BY last_activity_at DESC NULLS LAST, created_at DESC \
      \LIMIT ?"
      (PGArray tags, limit)
    else liftIO $ query conn
      "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
      \created_at, archived_at, supersedes_id, last_activity_at \
      \FROM documents WHERE tags && ? \
      \ORDER BY created_at DESC \
      \LIMIT ?"
      (PGArray tags, limit)

-- Get all active documents
getActiveDocuments :: Int -> App [Document]
getActiveDocuments limit = do
  conn <- getConn
  liftIO $ query conn
    "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
    \created_at, archived_at, supersedes_id, last_activity_at \
    \FROM documents WHERE active = TRUE AND supersedes_id IS NULL \
    \ORDER BY last_activity_at DESC NULLS LAST, created_at DESC \
    \LIMIT ?"
    (Only limit)

-- Update last_activity_at timestamp
updateDocumentLastActivity :: EntityId -> App ()
updateDocumentLastActivity docId = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "UPDATE documents SET last_activity_at = now() WHERE id = ?"
    (Only $ unEntityId docId)
  pure ()

-- Archive a document
archiveDocument :: EntityId -> App ()
archiveDocument docId = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "UPDATE documents SET active = FALSE, archived_at = now() WHERE id = ?"
    (Only $ unEntityId docId)
  pure ()

-- Insert a log entry
insertDocumentLog :: NewDocumentLogEntry -> App EntityId
insertDocumentLog entry = do
  conn <- getConn
  logId <- liftIO newEntityId
  _ <- liftIO $ execute conn
    "INSERT INTO document_log (id, document_id, activity_id, description, source, agent_id) \
    \VALUES (?, ?, ?, ?, ?, ?)"
    ( unEntityId logId
    , unEntityId (newLogDocumentId entry)
    , fmap unEntityId (newLogActivityId entry)
    , newLogDescription entry
    , newLogSource entry
    , newLogAgentId entry
    )
  -- Update last_activity_at on the document
  updateDocumentLastActivity (newLogDocumentId entry)
  pure logId

-- Get log entries for a document
getDocumentLog :: EntityId -> Int -> App [DocumentLogEntry]
getDocumentLog docId limit = do
  conn <- getConn
  liftIO $ query conn
    "SELECT id, document_id, activity_id, description, source, agent_id, created_at \
    \FROM document_log WHERE document_id = ? \
    \ORDER BY created_at DESC \
    \LIMIT ?"
    (unEntityId docId, limit)
```

**Step 2: Add to cabal file**

Add `Infra.Db.Document` to `other-modules`.

**Step 3: Verify it compiles**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Infra/Db/Document.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(db): add Document database layer"
```

---

## Task 5: CLI Commands

**Files:**
- Modify: `wisp-cli/app/Main.hs`

**Step 1: Add command types**

Add to the `Command` data type:

```haskell
  | Project ProjectCommand
  | Note NoteCommand
  | Pref PrefCommand
```

Add new command types:

```haskell
data ProjectCommand
  = ProjectList
  | ProjectCreate Text Text  -- name, type
  | ProjectArchive Text      -- id
  deriving (Show)

data NoteCommand
  = NoteList (Maybe Text)    -- optional tag filter
  | NoteCreate Text (Maybe Text) [Text]  -- title, content, tags
  deriving (Show)

data PrefCommand
  = PrefList
  | PrefSet Text Text (Maybe Text)  -- key, value, context
  deriving (Show)
```

**Step 2: Add parsers**

```haskell
projectParser :: Parser Command
projectParser = Project <$> subparser
  ( command "list" (info (pure ProjectList) (progDesc "List active projects"))
  <> command "create" (info projectCreateParser (progDesc "Create a project"))
  <> command "archive" (info projectArchiveParser (progDesc "Archive a project"))
  )

projectCreateParser :: Parser ProjectCommand
projectCreateParser = ProjectCreate
  <$> strArgument (metavar "NAME" <> help "Project name")
  <*> strArgument (metavar "TYPE" <> help "Type: work, personal, family, health, spiritual")

projectArchiveParser :: Parser ProjectCommand
projectArchiveParser = ProjectArchive
  <$> strArgument (metavar "ID" <> help "Project ID")

noteParser :: Parser Command
noteParser = Note <$> subparser
  ( command "list" (info noteListParser (progDesc "List notes"))
  <> command "create" (info noteCreateParser (progDesc "Create a note"))
  )

noteListParser :: Parser NoteCommand
noteListParser = NoteList <$> optional (strOption (long "tag" <> short 't' <> metavar "TAG"))

noteCreateParser :: Parser NoteCommand
noteCreateParser = NoteCreate
  <$> strArgument (metavar "TITLE" <> help "Note title")
  <*> optional (strOption (long "content" <> short 'c' <> metavar "CONTENT"))
  <*> (words <$> strOption (long "tags" <> value "" <> metavar "TAGS" <> help "Comma-separated tags"))
  where
    words s = filter (not . null) $ map T.strip $ T.splitOn "," (T.pack s)

prefParser :: Parser Command
prefParser = Pref <$> subparser
  ( command "list" (info (pure PrefList) (progDesc "List preferences"))
  <> command "set" (info prefSetParser (progDesc "Set a preference"))
  )

prefSetParser :: Parser PrefCommand
prefSetParser = PrefSet
  <$> strArgument (metavar "KEY" <> help "Preference key")
  <*> strArgument (metavar "VALUE" <> help "Preference value")
  <*> optional (strOption (long "context" <> metavar "CONTEXT"))
```

**Step 3: Add to command subparser**

```haskell
<> command "project" (info projectParser (progDesc "Manage projects"))
<> command "note" (info noteParser (progDesc "Manage notes"))
<> command "pref" (info prefParser (progDesc "Manage preferences"))
```

**Step 4: Add handlers in main**

```haskell
Project cmd -> runProject cmd
Note cmd -> runNote cmd
Pref cmd -> runPref cmd
```

**Step 5: Implement handlers**

These will call HTTP endpoints (to be added in Task 6).

**Step 6: Verify it compiles**

Run: `cabal build wisp-cli`
Expected: Build succeeds (handlers can be stubs initially)

**Step 7: Commit**

```bash
git add wisp-cli/app/Main.hs
git commit -m "feat(cli): add project, note, pref commands"
```

---

## Task 6: HTTP Endpoints

**Files:**
- Create: `wisp-srv/src/Http/Handlers/Documents.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`

**Step 1: Write the handlers**

```haskell
-- src/Http/Handlers/Documents.hs
module Http.Handlers.Documents
  ( getProjectsList
  , postProject
  , postProjectArchive
  , getNotesList
  , postNote
  , getPrefsList
  , postPref
  , getDocumentById
  , getDocumentLog
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Web.Scotty.Trans (ActionT, json, jsonData, pathParam, status)
import Network.HTTP.Types.Status (status201, status404)
import App.Monad (Env)
import Domain.Id (EntityId(..))
import Domain.Document
import qualified Infra.Db.Document as Db

-- List active projects
getProjectsList :: ActionT (ReaderT Env IO) ()
getProjectsList = do
  projects <- lift $ Db.getDocumentsByType ProjectDoc True 100
  json $ object ["projects" .= projects, "count" .= length projects]

-- Create a project
postProject :: ActionT (ReaderT Env IO) ()
postProject = do
  body <- jsonData
  let name = body Aeson..: "name"
      projType = body Aeson..: "type"
  case (name, projType) of
    (Just n, Just t) -> do
      let projData = Aeson.object ["name" .= (n :: Text), "type" .= (t :: Text)]
      let newDoc = NewDocument
            { newDocTenantId = Nothing  -- TODO: get from auth
            , newDocType = ProjectDoc
            , newDocData = projData
            , newDocTags = []
            , newDocConfidence = Nothing
            , newDocSource = Just "user"
            , newDocSupersedesId = Nothing
            }
      docId <- lift $ Db.insertDocument newDoc
      -- Create log entry
      let logEntry = NewDocumentLogEntry
            { newLogDocumentId = docId
            , newLogActivityId = Nothing
            , newLogDescription = Just "Created project"
            , newLogSource = LogUser
            , newLogAgentId = Nothing
            }
      _ <- lift $ Db.insertDocumentLog logEntry
      status status201
      json $ object ["id" .= docId, "status" .= ("created" :: Text)]
    _ -> json $ object ["error" .= ("Missing name or type" :: Text)]

-- Archive a project
postProjectArchive :: ActionT (ReaderT Env IO) ()
postProjectArchive = do
  docId <- pathParam "id"
  lift $ Db.archiveDocument (EntityId docId)
  json $ object ["status" .= ("archived" :: Text)]

-- List notes (optionally filtered by tag)
getNotesList :: ActionT (ReaderT Env IO) ()
getNotesList = do
  -- TODO: get tag from query param
  notes <- lift $ Db.getDocumentsByType NoteDoc True 100
  json $ object ["notes" .= notes, "count" .= length notes]

-- Create a note
postNote :: ActionT (ReaderT Env IO) ()
postNote = do
  body <- jsonData
  let title = body Aeson..: "title"
      content = body Aeson..:? "content"
      tags = maybe [] id (body Aeson..:? "tags")
  case title of
    Just t -> do
      let noteData = Aeson.object ["title" .= (t :: Text), "content" .= (content :: Maybe Text)]
      let newDoc = NewDocument
            { newDocTenantId = Nothing
            , newDocType = NoteDoc
            , newDocData = noteData
            , newDocTags = tags
            , newDocConfidence = Nothing
            , newDocSource = Just "user"
            , newDocSupersedesId = Nothing
            }
      docId <- lift $ Db.insertDocument newDoc
      let logEntry = NewDocumentLogEntry
            { newLogDocumentId = docId
            , newLogActivityId = Nothing
            , newLogDescription = Just "Created note"
            , newLogSource = LogUser
            , newLogAgentId = Nothing
            }
      _ <- lift $ Db.insertDocumentLog logEntry
      status status201
      json $ object ["id" .= docId, "status" .= ("created" :: Text)]
    Nothing -> json $ object ["error" .= ("Missing title" :: Text)]

-- List preferences
getPrefsList :: ActionT (ReaderT Env IO) ()
getPrefsList = do
  prefs <- lift $ Db.getDocumentsByType PreferenceDoc True 100
  json $ object ["preferences" .= prefs, "count" .= length prefs]

-- Set a preference
postPref :: ActionT (ReaderT Env IO) ()
postPref = do
  body <- jsonData
  let key = body Aeson..: "key"
      value = body Aeson..: "value"
      context = body Aeson..:? "context"
  case (key, value) of
    (Just k, Just v) -> do
      let prefData = Aeson.object
            [ "key" .= (k :: Text)
            , "value" .= (v :: Text)
            , "context" .= (context :: Maybe Text)
            ]
      let newDoc = NewDocument
            { newDocTenantId = Nothing
            , newDocType = PreferenceDoc
            , newDocData = prefData
            , newDocTags = []
            , newDocConfidence = Nothing
            , newDocSource = Just "user"
            , newDocSupersedesId = Nothing
            }
      docId <- lift $ Db.insertDocument newDoc
      let logEntry = NewDocumentLogEntry
            { newLogDocumentId = docId
            , newLogActivityId = Nothing
            , newLogDescription = Just "Set preference"
            , newLogSource = LogUser
            , newLogAgentId = Nothing
            }
      _ <- lift $ Db.insertDocumentLog logEntry
      status status201
      json $ object ["id" .= docId, "status" .= ("created" :: Text)]
    _ -> json $ object ["error" .= ("Missing key or value" :: Text)]

-- Get document by ID
getDocumentById :: ActionT (ReaderT Env IO) ()
getDocumentById = do
  docId <- pathParam "id"
  mDoc <- lift $ Db.getDocumentById (EntityId docId)
  case mDoc of
    Just doc -> json doc
    Nothing -> do
      status status404
      json $ object ["error" .= ("Document not found" :: Text)]

-- Get document log
getDocumentLog :: ActionT (ReaderT Env IO) ()
getDocumentLog = do
  docId <- pathParam "id"
  entries <- lift $ Db.getDocumentLog (EntityId docId) 50
  json $ object ["log" .= entries, "count" .= length entries]
```

**Step 2: Add routes**

In `Http/Routes.hs`:

```haskell
import Http.Handlers.Documents (getProjectsList, postProject, postProjectArchive, getNotesList, postNote, getPrefsList, postPref, getDocumentById, getDocumentLog)

-- Documents
get "/api/projects" getProjectsList
post "/api/projects" postProject
post "/api/projects/:id/archive" postProjectArchive
get "/api/notes" getNotesList
post "/api/notes" postNote
get "/api/preferences" getPrefsList
post "/api/preferences" postPref
get "/api/documents/:id" getDocumentById
get "/api/documents/:id/log" getDocumentLog
```

**Step 3: Build and test**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Documents.hs wisp-srv/src/Http/Routes.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(http): add document API endpoints"
```

---

## Task 7: Complete CLI Handlers

**Files:**
- Modify: `wisp-cli/app/Main.hs`

**Step 1: Implement project handlers**

```haskell
runProject :: ProjectCommand -> IO ()
runProject ProjectList = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/api/projects"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "projects" obj of
      Just (Array projects) -> do
        TIO.putStrLn "Active Projects"
        TIO.putStrLn "==============="
        if null projects
          then TIO.putStrLn "No projects found."
          else mapM_ showProject (toList projects)
      _ -> TIO.putStrLn "No projects found"
    _ -> TIO.putStrLn "Failed to fetch projects"

runProject (ProjectCreate name projType) = do
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/api/projects"
  let reqBody = object ["name" .= name, "type" .= projType]
  let req = initialReq
        { method = "POST"
        , requestHeaders = [("Content-Type", "application/json")]
        , requestBody = RequestBodyLBS (encode reqBody)
        }
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "id" obj of
      Just (String pid) -> TIO.putStrLn $ "Created project: " <> pid
      _ -> TIO.putStrLn "Project created"
    _ -> TIO.putStrLn "Failed to create project"

runProject (ProjectArchive pid) = do
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/api/projects/" <> unpack pid <> "/archive"
  let req = initialReq { method = "POST" }
  _ <- httpLbs req manager
  TIO.putStrLn $ "Archived project: " <> pid

showProject :: Value -> IO ()
showProject (Object p) = do
  let getId = case KM.lookup "id" p of
        Just (String s) -> s
        _ -> "?"
  let getData = KM.lookup "data" p
  let name = case getData of
        Just (Object d) -> case KM.lookup "name" d of
          Just (String n) -> n
          _ -> "?"
        _ -> "?"
  let ptype = case getData of
        Just (Object d) -> case KM.lookup "type" d of
          Just (String t) -> t
          _ -> "?"
        _ -> "?"
  TIO.putStrLn $ "  [" <> getId <> "] " <> name <> " (" <> ptype <> ")"
showProject _ = pure ()
```

**Step 2: Implement note handlers** (similar pattern)

**Step 3: Implement pref handlers** (similar pattern)

**Step 4: Build and test**

Run: `cabal build wisp-cli`
Run: `cabal run wisp-cli -- project list`
Expected: Shows empty projects list or any created projects

**Step 5: Commit**

```bash
git add wisp-cli/app/Main.hs
git commit -m "feat(cli): implement project, note, pref handlers"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Database migration | `migrations/018_documents.sql` |
| 2 | Domain types | `Domain/Document.hs` |
| 3 | Domain tests | `test/Domain/DocumentSpec.hs` |
| 4 | Database layer | `Infra/Db/Document.hs` |
| 5 | CLI commands (structure) | `wisp-cli/app/Main.hs` |
| 6 | HTTP endpoints | `Http/Handlers/Documents.hs`, `Http/Routes.hs` |
| 7 | CLI handlers (implementation) | `wisp-cli/app/Main.hs` |
