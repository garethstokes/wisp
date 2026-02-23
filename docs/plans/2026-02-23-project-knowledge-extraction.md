# Project Knowledge Extraction Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Automatically assign activities to projects during classification and synthesize project state through batch reflection.

**Architecture:** Extend the classifier to output project assignments, store links in a many-to-many join table, update project documents immediately for simple signals, and run batch reflection for synthesis. Surface new project suggestions through the existing Approvals view.

**Tech Stack:** Haskell, PostgreSQL, Aeson for JSON, postgresql-simple for DB access, HSpec for tests.

---

## Task 1: Database Migration - Activity Documents Join Table

**Files:**
- Create: `wisp-srv/migrations/020_activity_documents.sql`
- Test: Run migration via `psql` or app startup

**Step 1: Write the migration file**

```sql
-- 020_activity_documents.sql
-- Many-to-many relationship between activities and documents

CREATE TABLE activity_documents (
  activity_id TEXT NOT NULL REFERENCES activities(id) ON DELETE CASCADE,
  document_id TEXT NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
  relationship TEXT NOT NULL,  -- 'project', 'mentions', etc.
  confidence FLOAT,
  source TEXT NOT NULL CHECK (source IN ('classifier', 'user', 'reflection')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (activity_id, document_id, relationship)
);

CREATE INDEX idx_activity_documents_document ON activity_documents(document_id);
CREATE INDEX idx_activity_documents_activity ON activity_documents(activity_id);

-- Track dismissed project suggestions to avoid re-surfacing
CREATE TABLE dismissed_suggestions (
  id TEXT PRIMARY KEY,
  cluster_key TEXT NOT NULL UNIQUE,  -- e.g., "subject:phoenix"
  tenant_id UUID REFERENCES tenants(id),
  dismissed_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_dismissed_suggestions_tenant ON dismissed_suggestions(tenant_id);
```

**Step 2: Verify migration syntax**

Run: `psql postgres://localhost/wisp_test -f wisp-srv/migrations/020_activity_documents.sql`
Expected: Tables created without errors

**Step 3: Commit**

```bash
git add wisp-srv/migrations/020_activity_documents.sql
git commit -m "feat: add activity_documents join table and dismissed_suggestions"
```

---

## Task 2: Database Migration - Bootstrap Projects

**Files:**
- Create: `wisp-srv/migrations/021_seed_projects.sql`

**Step 1: Write the seed migration**

```sql
-- 021_seed_projects.sql
-- Bootstrap initial projects for the default tenant

-- Insert projects (idempotent using ON CONFLICT)
INSERT INTO documents (id, tenant_id, type, data, tags, confidence, source, active)
VALUES
  ('project-wisp', NULL, 'project',
   '{"name": "Wisp", "type": "work", "summary": "", "status": "active", "participants": [], "activity_count": 0}'::jsonb,
   ARRAY['wisp'], 1.0, 'user', TRUE),
  ('project-lune', NULL, 'project',
   '{"name": "Lune", "type": "work", "summary": "", "status": "active", "participants": [], "activity_count": 0}'::jsonb,
   ARRAY['lune'], 1.0, 'user', TRUE),
  ('project-superit', NULL, 'project',
   '{"name": "Super IT", "type": "work", "summary": "", "status": "active", "participants": [], "activity_count": 0}'::jsonb,
   ARRAY['superit', 'super-it'], 1.0, 'user', TRUE),
  ('project-paidright', NULL, 'project',
   '{"name": "PaidRight", "type": "work", "summary": "", "status": "active", "participants": [], "activity_count": 0}'::jsonb,
   ARRAY['paidright'], 1.0, 'user', TRUE)
ON CONFLICT (id) DO NOTHING;
```

**Step 2: Verify migration**

Run: `psql postgres://localhost/wisp_test -f wisp-srv/migrations/021_seed_projects.sql`
Expected: 4 rows inserted (or 0 if already exist)

**Step 3: Commit**

```bash
git add wisp-srv/migrations/021_seed_projects.sql
git commit -m "feat: seed initial projects (Wisp, Lune, Super IT, PaidRight)"
```

---

## Task 3: Domain Types - Extended ProjectData

**Files:**
- Modify: `wisp-srv/src/Domain/Document.hs`
- Test: `wisp-srv/test/Domain/DocumentSpec.hs`

**Step 1: Write the failing test**

Add to `wisp-srv/test/Domain/DocumentSpec.hs`:

```haskell
  describe "ExtendedProjectData" $ do
    it "deserializes full project with accumulated state" $ do
      let json = "{\"name\":\"Wisp\",\"type\":\"work\",\"summary\":\"Building an AI assistant\",\"status\":\"active\",\"participants\":[\"alice@example.com\"],\"activity_count\":5,\"last_activity_at\":\"2026-02-20T14:30:00Z\"}"
          Just proj = decode json :: Maybe ExtendedProjectData
      extProjectName proj `shouldBe` "Wisp"
      extProjectStatus proj `shouldBe` "active"
      extProjectActivityCount proj `shouldBe` 5
      length (extProjectParticipants proj) `shouldBe` 1

    it "serializes with all fields" $ do
      let proj = ExtendedProjectData
            { extProjectName = "Wisp"
            , extProjectType = Work
            , extProjectSummary = "AI assistant"
            , extProjectStatus = "active"
            , extProjectParticipants = ["alice@example.com"]
            , extProjectActivityCount = 10
            , extProjectLastActivityAt = Nothing
            }
          json = LBS.unpack (encode proj)
      json `shouldContain` "\"activity_count\":10"
      json `shouldContain` "\"status\":\"active\""
```

**Step 2: Run test to verify it fails**

Run: `cabal test --test-option=--match="/Document/ExtendedProjectData/"`
Expected: FAIL - ExtendedProjectData not in scope

**Step 3: Add ExtendedProjectData type**

Add to `wisp-srv/src/Domain/Document.hs` exports:

```haskell
  , ExtendedProjectData(..)
```

Add type definition after `ProjectData`:

```haskell
-- Extended project data with accumulated state
data ExtendedProjectData = ExtendedProjectData
  { extProjectName :: Text
  , extProjectType :: ProjectType
  , extProjectSummary :: Text
  , extProjectStatus :: Text  -- "active", "stalled", "completed"
  , extProjectParticipants :: [Text]
  , extProjectActivityCount :: Int
  , extProjectLastActivityAt :: Maybe UTCTime
  } deriving (Eq, Show, Generic)

instance ToJSON ExtendedProjectData where
  toJSON p = object
    [ "name" .= extProjectName p
    , "type" .= extProjectType p
    , "summary" .= extProjectSummary p
    , "status" .= extProjectStatus p
    , "participants" .= extProjectParticipants p
    , "activity_count" .= extProjectActivityCount p
    , "last_activity_at" .= extProjectLastActivityAt p
    ]

instance FromJSON ExtendedProjectData where
  parseJSON = withObject "ExtendedProjectData" $ \v -> ExtendedProjectData
    <$> v .: "name"
    <*> v .: "type"
    <*> v .:? "summary" .!= ""
    <*> v .:? "status" .!= "active"
    <*> v .:? "participants" .!= []
    <*> v .:? "activity_count" .!= 0
    <*> v .:? "last_activity_at"
```

Add import at top:

```haskell
import Data.Aeson (... , (.!=))
```

**Step 4: Run test to verify it passes**

Run: `cabal test --test-option=--match="/Document/ExtendedProjectData/"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Domain/Document.hs wisp-srv/test/Domain/DocumentSpec.hs
git commit -m "feat: add ExtendedProjectData with accumulated state fields"
```

---

## Task 4: Domain Types - ActivityDocument

**Files:**
- Create: `wisp-srv/src/Domain/ActivityDocument.hs`
- Test: `wisp-srv/test/Domain/ActivityDocumentSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal` (add to other-modules)

**Step 1: Write the failing test**

Create `wisp-srv/test/Domain/ActivityDocumentSpec.hs`:

```haskell
module Domain.ActivityDocumentSpec where

import Test.Hspec
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Domain.ActivityDocument

spec :: Spec
spec = describe "ActivityDocument" $ do
  describe "LinkSource" $ do
    it "serializes Classifier to 'classifier'" $ do
      LBS.unpack (encode Classifier) `shouldBe` "\"classifier\""

    it "deserializes 'user' to User" $ do
      decode "\"user\"" `shouldBe` Just User

  describe "Relationship" $ do
    it "serializes Project to 'project'" $ do
      LBS.unpack (encode Project) `shouldBe` "\"project\""
```

**Step 2: Run test to verify it fails**

Run: `cabal test --test-option=--match="/ActivityDocument/"`
Expected: FAIL - module not found

**Step 3: Create the domain module**

Create `wisp-srv/src/Domain/ActivityDocument.hs`:

```haskell
module Domain.ActivityDocument
  ( ActivityDocument(..)
  , NewActivityDocument(..)
  , LinkSource(..)
  , Relationship(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Id (EntityId)
import GHC.Generics (Generic)

-- | Source of the activity-document link
data LinkSource = Classifier | User | Reflection
  deriving (Eq, Show, Generic)

instance ToJSON LinkSource where
  toJSON Classifier = "classifier"
  toJSON User = "user"
  toJSON Reflection = "reflection"

instance FromJSON LinkSource where
  parseJSON = withText "LinkSource" $ \case
    "classifier" -> pure Classifier
    "user" -> pure User
    "reflection" -> pure Reflection
    other -> fail $ "Unknown link source: " <> show other

-- | Type of relationship between activity and document
data Relationship = Project | Mentions
  deriving (Eq, Show, Generic)

instance ToJSON Relationship where
  toJSON Project = "project"
  toJSON Mentions = "mentions"

instance FromJSON Relationship where
  parseJSON = withText "Relationship" $ \case
    "project" -> pure Project
    "mentions" -> pure Mentions
    other -> fail $ "Unknown relationship: " <> show other

-- | Full activity-document link from database
data ActivityDocument = ActivityDocument
  { adActivityId :: EntityId
  , adDocumentId :: EntityId
  , adRelationship :: Relationship
  , adConfidence :: Maybe Double
  , adSource :: LinkSource
  , adCreatedAt :: UTCTime
  } deriving (Eq, Show)

-- | For creating new links
data NewActivityDocument = NewActivityDocument
  { newAdActivityId :: EntityId
  , newAdDocumentId :: EntityId
  , newAdRelationship :: Relationship
  , newAdConfidence :: Maybe Double
  , newAdSource :: LinkSource
  } deriving (Eq, Show)
```

**Step 4: Add to cabal file**

Add `Domain.ActivityDocument` and `Domain.ActivityDocumentSpec` to `wisp-srv.cabal` other-modules in both library and test-suite sections.

**Step 5: Run test to verify it passes**

Run: `cabal test --test-option=--match="/ActivityDocument/"`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Domain/ActivityDocument.hs wisp-srv/test/Domain/ActivityDocumentSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add ActivityDocument domain types"
```

---

## Task 5: Domain Types - ProjectClassification

**Files:**
- Create: `wisp-srv/src/Domain/ProjectClassification.hs`
- Test: `wisp-srv/test/Domain/ProjectClassificationSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Domain/ProjectClassificationSpec.hs`:

```haskell
module Domain.ProjectClassificationSpec where

import Test.Hspec
import Data.Aeson (decode)
import Domain.ProjectClassification

spec :: Spec
spec = describe "ProjectClassification" $ do
  describe "JSON parsing" $ do
    it "parses project assignment with confidence" $ do
      let json = "{\"name\":\"wisp\",\"confidence\":0.9}"
          Just pc = decode json :: Maybe ProjectAssignment
      paName pc `shouldBe` "wisp"
      paConfidence pc `shouldBe` 0.9

    it "parses list of project assignments" $ do
      let json = "[{\"name\":\"wisp\",\"confidence\":0.9},{\"name\":\"superit\",\"confidence\":0.6}]"
          Just pcs = decode json :: Maybe [ProjectAssignment]
      length pcs `shouldBe` 2
```

**Step 2: Run test to verify it fails**

Run: `cabal test --test-option=--match="/ProjectClassification/"`
Expected: FAIL - module not found

**Step 3: Create the domain module**

Create `wisp-srv/src/Domain/ProjectClassification.hs`:

```haskell
module Domain.ProjectClassification
  ( ProjectAssignment(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, object, (.:), (.=))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A project assignment from the classifier
data ProjectAssignment = ProjectAssignment
  { paName :: Text       -- Lowercase project identifier (e.g., "wisp", "superit")
  , paConfidence :: Double
  } deriving (Eq, Show, Generic)

instance FromJSON ProjectAssignment where
  parseJSON = withObject "ProjectAssignment" $ \v -> ProjectAssignment
    <$> v .: "name"
    <*> v .: "confidence"

instance ToJSON ProjectAssignment where
  toJSON pa = object
    [ "name" .= paName pa
    , "confidence" .= paConfidence pa
    ]
```

**Step 4: Add to cabal file**

Add `Domain.ProjectClassification` and `Domain.ProjectClassificationSpec` to `wisp-srv.cabal`.

**Step 5: Run test to verify it passes**

Run: `cabal test --test-option=--match="/ProjectClassification/"`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Domain/ProjectClassification.hs wisp-srv/test/Domain/ProjectClassificationSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add ProjectAssignment type for classifier output"
```

---

## Task 6: Database Layer - ActivityDocument CRUD

**Files:**
- Create: `wisp-srv/src/Infra/Db/ActivityDocument.hs`
- Test: `wisp-srv/test/Infra/Db/ActivityDocumentSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Infra/Db/ActivityDocumentSpec.hs`:

```haskell
module Infra.Db.ActivityDocumentSpec where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Domain.Id (EntityId(..))
import Domain.ActivityDocument
import Infra.Db.ActivityDocument
import Infra.Db.Activity (insertActivity)
import Infra.Db.Document (insertDocument)
import Domain.Activity (NewActivity(..), ActivitySource(..))
import Domain.Document (NewDocument(..), DocumentType(..))
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = around withTestEnv $ do

  describe "linkActivityToDocument" $ do
    it "creates a link between activity and document" $ \env -> runTestApp env $ do
      -- Create test activity
      let newActivity = NewActivity
            { newActivityAccountId = EntityId "test-account"
            , newActivitySource = Email
            , newActivitySourceId = "test-email-1"
            , newActivityRaw = object ["test" .= True]
            , newActivityTitle = Just "Test email"
            , newActivitySenderEmail = Just "alice@example.com"
            , newActivityStartsAt = Nothing
            , newActivityEndsAt = Nothing
            }
      actId <- insertActivity newActivity

      -- Create test document
      let newDoc = NewDocument
            { newDocTenantId = Nothing
            , newDocType = ProjectDoc
            , newDocData = object ["name" .= ("Wisp" :: String), "type" .= ("work" :: String)]
            , newDocTags = ["wisp"]
            , newDocConfidence = Just 1.0
            , newDocSource = Just "user"
            , newDocSupersedesId = Nothing
            }
      docId <- insertDocument newDoc

      -- Link them
      let link = NewActivityDocument
            { newAdActivityId = actId
            , newAdDocumentId = docId
            , newAdRelationship = Project
            , newAdConfidence = Just 0.9
            , newAdSource = Classifier
            }
      linkActivityToDocument link

      -- Verify
      links <- getDocumentActivities docId 10
      liftIO $ length links `shouldBe` 1

  describe "getActivityProjects" $ do
    it "returns all projects linked to an activity" $ \env -> runTestApp env $ do
      -- Setup: create activity and two projects, link both
      let newActivity = NewActivity
            { newActivityAccountId = EntityId "test-account"
            , newActivitySource = Email
            , newActivitySourceId = "test-email-2"
            , newActivityRaw = object []
            , newActivityTitle = Just "Cross-project email"
            , newActivitySenderEmail = Nothing
            , newActivityStartsAt = Nothing
            , newActivityEndsAt = Nothing
            }
      actId <- insertActivity newActivity

      let doc1 = NewDocument
            { newDocTenantId = Nothing
            , newDocType = ProjectDoc
            , newDocData = object ["name" .= ("Wisp" :: String), "type" .= ("work" :: String)]
            , newDocTags = []
            , newDocConfidence = Nothing
            , newDocSource = Nothing
            , newDocSupersedesId = Nothing
            }
      docId1 <- insertDocument doc1

      let doc2 = NewDocument
            { newDocTenantId = Nothing
            , newDocType = ProjectDoc
            , newDocData = object ["name" .= ("Lune" :: String), "type" .= ("work" :: String)]
            , newDocTags = []
            , newDocConfidence = Nothing
            , newDocSource = Nothing
            , newDocSupersedesId = Nothing
            }
      docId2 <- insertDocument doc2

      linkActivityToDocument $ NewActivityDocument actId docId1 Project (Just 0.9) Classifier
      linkActivityToDocument $ NewActivityDocument actId docId2 Project (Just 0.7) Classifier

      projects <- getActivityProjects actId
      liftIO $ length projects `shouldBe` 2
```

**Step 2: Run test to verify it fails**

Run: `cabal test --test-option=--match="/Infra.Db.ActivityDocument/"`
Expected: FAIL - module not found

**Step 3: Create the database module**

Create `wisp-srv/src/Infra/Db/ActivityDocument.hs`:

```haskell
module Infra.Db.ActivityDocument
  ( linkActivityToDocument
  , getActivityProjects
  , getDocumentActivities
  , unlinkActivityFromDocument
  ) where

import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField (fromField, returnError, Conversion, FromField(..))
import Database.PostgreSQL.Simple.ToField (toField, ToField(..))
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Id (EntityId(..), unEntityId)
import Domain.ActivityDocument
import Domain.Document (Document)
import App.Monad (App, getConn)
import Infra.Db.Document ()  -- Import FromRow Document instance

-- ToField instances
instance ToField LinkSource where
  toField Classifier = toField ("classifier" :: Text)
  toField User = toField ("user" :: Text)
  toField Reflection = toField ("reflection" :: Text)

instance ToField Relationship where
  toField Project = toField ("project" :: Text)
  toField Mentions = toField ("mentions" :: Text)

-- FromRow instance for ActivityDocument
instance FromRow ActivityDocument where
  fromRow = ActivityDocument
    <$> (EntityId <$> field)  -- activity_id
    <*> (EntityId <$> field)  -- document_id
    <*> relField              -- relationship
    <*> field                 -- confidence
    <*> sourceField           -- source
    <*> field                 -- created_at
    where
      relField = fieldWith $ \f mbs -> do
        txt <- fromField f mbs :: Conversion Text
        case txt of
          "project" -> pure Project
          "mentions" -> pure Mentions
          other -> returnError ConversionFailed f $ "Unknown relationship: " <> T.unpack other

      sourceField = fieldWith $ \f mbs -> do
        txt <- fromField f mbs :: Conversion Text
        case txt of
          "classifier" -> pure Classifier
          "user" -> pure User
          "reflection" -> pure Reflection
          other -> returnError ConversionFailed f $ "Unknown link source: " <> T.unpack other

-- | Link an activity to a document
linkActivityToDocument :: NewActivityDocument -> App ()
linkActivityToDocument link = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "INSERT INTO activity_documents (activity_id, document_id, relationship, confidence, source) \
    \VALUES (?, ?, ?, ?, ?) \
    \ON CONFLICT (activity_id, document_id, relationship) DO UPDATE SET \
    \confidence = EXCLUDED.confidence, source = EXCLUDED.source"
    ( unEntityId (newAdActivityId link)
    , unEntityId (newAdDocumentId link)
    , newAdRelationship link
    , newAdConfidence link
    , newAdSource link
    )
  pure ()

-- | Get all project documents linked to an activity
getActivityProjects :: EntityId -> App [Document]
getActivityProjects actId = do
  conn <- getConn
  liftIO $ query conn
    "SELECT d.id, d.tenant_id, d.type, d.data, d.tags, d.confidence, d.source, d.active, \
    \d.created_at, d.archived_at, d.supersedes_id, d.last_activity_at \
    \FROM documents d \
    \JOIN activity_documents ad ON d.id = ad.document_id \
    \WHERE ad.activity_id = ? AND ad.relationship = 'project' \
    \ORDER BY ad.confidence DESC NULLS LAST"
    (Only $ unEntityId actId)

-- | Get all activities linked to a document
getDocumentActivities :: EntityId -> Int -> App [ActivityDocument]
getDocumentActivities docId limit = do
  conn <- getConn
  liftIO $ query conn
    "SELECT activity_id, document_id, relationship, confidence, source, created_at \
    \FROM activity_documents \
    \WHERE document_id = ? \
    \ORDER BY created_at DESC \
    \LIMIT ?"
    (unEntityId docId, limit)

-- | Remove a link
unlinkActivityFromDocument :: EntityId -> EntityId -> Relationship -> App ()
unlinkActivityFromDocument actId docId rel = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "DELETE FROM activity_documents WHERE activity_id = ? AND document_id = ? AND relationship = ?"
    (unEntityId actId, unEntityId docId, rel)
  pure ()
```

**Step 4: Add to cabal file**

Add `Infra.Db.ActivityDocument` and `Infra.Db.ActivityDocumentSpec` to `wisp-srv.cabal`.

**Step 5: Run test to verify it passes**

Run: `cabal test --test-option=--match="/Infra.Db.ActivityDocument/"`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Infra/Db/ActivityDocument.hs wisp-srv/test/Infra/Db/ActivityDocumentSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add ActivityDocument database layer"
```

---

## Task 7: Database Layer - Project Queries

**Files:**
- Modify: `wisp-srv/src/Infra/Db/Document.hs`
- Test: Add to existing tests or create new

**Step 1: Write the failing test**

Add to an appropriate spec file or create `wisp-srv/test/Infra/Db/DocumentProjectSpec.hs`:

```haskell
  describe "getProjectByTag" $ do
    it "finds project by tag" $ \env -> runTestApp env $ do
      let newDoc = NewDocument
            { newDocTenantId = Nothing
            , newDocType = ProjectDoc
            , newDocData = object ["name" .= ("Wisp" :: String), "type" .= ("work" :: String)]
            , newDocTags = ["wisp"]
            , newDocConfidence = Just 1.0
            , newDocSource = Just "user"
            , newDocSupersedesId = Nothing
            }
      docId <- insertDocument newDoc

      mDoc <- getProjectByTag "wisp"
      liftIO $ case mDoc of
        Just doc -> documentId doc `shouldBe` docId
        Nothing -> expectationFailure "Project not found"

  describe "getAllProjects" $ do
    it "returns all active projects" $ \env -> runTestApp env $ do
      projects <- getAllProjects
      liftIO $ projects `shouldSatisfy` (not . null)  -- Seed data exists
```

**Step 2: Run test to verify it fails**

Run: `cabal test --test-option=--match="/getProjectByTag/"`
Expected: FAIL - function not in scope

**Step 3: Add functions to Document.hs**

Add to exports in `wisp-srv/src/Infra/Db/Document.hs`:

```haskell
  , getProjectByTag
  , getAllProjects
  , updateProjectData
```

Add implementations:

```haskell
-- | Get a project document by its tag (for classifier lookup)
getProjectByTag :: Text -> App (Maybe Document)
getProjectByTag tag = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
    \created_at, archived_at, supersedes_id, last_activity_at \
    \FROM documents \
    \WHERE type = 'project' AND active = TRUE AND ? = ANY(tags) \
    \LIMIT 1"
    (Only tag)
  pure $ case results of
    [doc] -> Just doc
    _ -> Nothing

-- | Get all active projects
getAllProjects :: App [Document]
getAllProjects = do
  conn <- getConn
  liftIO $ query_ conn
    "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
    \created_at, archived_at, supersedes_id, last_activity_at \
    \FROM documents \
    \WHERE type = 'project' AND active = TRUE \
    \ORDER BY last_activity_at DESC NULLS LAST, created_at DESC"

-- | Update project document data (for accumulated state)
updateProjectData :: EntityId -> Value -> App ()
updateProjectData docId newData = do
  conn <- getConn
  let dataJson = LBS.toStrict $ encode newData
  _ <- liftIO $ execute conn
    "UPDATE documents SET data = ?::jsonb, last_activity_at = now() WHERE id = ?"
    (dataJson, unEntityId docId)
  pure ()
```

**Step 4: Run test to verify it passes**

Run: `cabal test --test-option=--match="/getProjectByTag/"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Db/Document.hs wisp-srv/test/Infra/Db/DocumentSpec.hs
git commit -m "feat: add project-specific document queries"
```

---

## Task 8: Extend Classification Domain Type

**Files:**
- Modify: `wisp-srv/src/Domain/Classification.hs`
- Test: `wisp-srv/test/Domain/ClassificationSpec.hs`

**Step 1: Write the failing test**

Add to `wisp-srv/test/Domain/ClassificationSpec.hs`:

```haskell
    it "parses classification with projects array" $ do
      let json = "{\"personas\":[\"work\"],\"activity_type\":\"request\",\
                 \\"urgency\":\"normal\",\"autonomy_tier\":2,\
                 \\"confidence\":0.85,\"summary\":\"Meeting request\",\
                 \\"reasoning\":\"Work email\",\"suggested_actions\":[],\
                 \\"projects\":[{\"name\":\"wisp\",\"confidence\":0.9}]}"
          Just c = decode json :: Maybe Classification
      length (classificationProjects c) `shouldBe` 1
      paName (head (classificationProjects c)) `shouldBe` "wisp"

    it "defaults to empty projects if not present" $ do
      let json = "{\"personas\":[\"work\"],\"activity_type\":\"request\",\
                 \\"urgency\":\"normal\",\"autonomy_tier\":2,\
                 \\"confidence\":0.85,\"summary\":\"Meeting request\",\
                 \\"reasoning\":\"Work email\",\"suggested_actions\":[]}"
          Just c = decode json :: Maybe Classification
      classificationProjects c `shouldBe` []
```

**Step 2: Run test to verify it fails**

Run: `cabal test --test-option=--match="/Classification/parses classification with projects/"`
Expected: FAIL - classificationProjects not a field

**Step 3: Extend Classification type**

Modify `wisp-srv/src/Domain/Classification.hs`:

Add import:
```haskell
import Domain.ProjectClassification (ProjectAssignment(..))
```

Add field to `Classification`:
```haskell
data Classification = Classification
  { classificationPersonas :: [Text]
  , classificationActivityType :: ActivityType
  , classificationUrgency :: Urgency
  , classificationAutonomyTier :: Int
  , classificationConfidence :: Double
  , classificationSummary :: Text
  , classificationReasoning :: Text
  , classificationSuggestedActions :: [Text]
  , classificationOptionFraming :: Maybe Text
  , classificationProjects :: [ProjectAssignment]  -- NEW
  } deriving (Eq, Show, Generic)
```

Update `FromJSON` instance:
```haskell
instance FromJSON Classification where
  parseJSON = withObject "Classification" $ \v -> Classification
    <$> v .: "personas"
    <*> v .: "activity_type"
    <*> v .: "urgency"
    <*> v .: "autonomy_tier"
    <*> v .: "confidence"
    <*> v .: "summary"
    <*> v .: "reasoning"
    <*> v .: "suggested_actions"
    <*> v .:? "option_framing"
    <*> v .:? "projects" .!= []
```

Update `ToJSON` instance to include projects.

**Step 4: Run test to verify it passes**

Run: `cabal test --test-option=--match="/Classification/"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Domain/Classification.hs wisp-srv/test/Domain/ClassificationSpec.hs
git commit -m "feat: add projects field to Classification type"
```

---

## Task 9: Extend Classifier Prompt

**Files:**
- Modify: `wisp-srv/src/Skills/Concierge/Classifier.hs`
- Test: `wisp-srv/test/Skills/Concierge/ClassifierSpec.hs`

**Step 1: Write the failing test**

Add to `wisp-srv/test/Skills/Concierge/ClassifierSpec.hs`:

```haskell
  describe "buildClassificationPromptWithProjects" $ do
    it "includes project list in prompt" $ do
      let projects = ["Wisp", "Lune", "Super IT", "PaidRight"]
          prompt = buildClassificationPromptWithProjects "email" (Just "Test") (object []) projects
      prompt `shouldSatisfy` T.isInfixOf "Wisp"
      prompt `shouldSatisfy` T.isInfixOf "projects"
```

**Step 2: Run test to verify it fails**

Run: `cabal test --test-option=--match="/buildClassificationPromptWithProjects/"`
Expected: FAIL - function not in scope

**Step 3: Add the function**

Add to exports in `wisp-srv/src/Skills/Concierge/Classifier.hs`:

```haskell
  , buildClassificationPromptWithProjects
```

Add implementation:

```haskell
-- | Build classification prompt with project context
buildClassificationPromptWithProjects :: Text -> Maybe Text -> Value -> [Text] -> Text
buildClassificationPromptWithProjects source mTitle raw projectNames = T.unlines
  [ "You are classifying an incoming " <> source <> " for a personal assistant."
  , ""
  , "## Known Projects"
  , if null projectNames
      then "(no projects defined)"
      else T.unlines $ map ("- " <>) projectNames
  , ""
  , "Analyze the following and respond with ONLY a JSON object (no markdown, no explanation):"
  , ""
  , "Title: " <> maybe "(none)" id mTitle
  , ""
  , "Raw data:"
  , TL.toStrict $ TLE.decodeUtf8 $ encode raw
  , ""
  , "Respond with this exact JSON structure:"
  , "{"
  , "  \"personas\": [\"work\"|\"home\"|\"personal\"],"
  , "  \"activity_type\": \"request\"|\"information\"|\"action_required\"|\"fyi\"|\"event\","
  , "  \"urgency\": \"high\"|\"normal\"|\"low\","
  , "  \"autonomy_tier\": 1-4,"
  , "  \"confidence\": 0.0-1.0,"
  , "  \"summary\": \"Brief 1-sentence summary\","
  , "  \"reasoning\": \"Why you classified it this way\","
  , "  \"suggested_actions\": [\"action1\", \"action2\"],"
  , "  \"option_framing\": \"How to present if surfaced\" | null,"
  , "  \"projects\": [{\"name\": \"project-tag\", \"confidence\": 0.0-1.0}]"
  , "}"
  , ""
  , "## Project Assignment Guidelines"
  , "- Assign to known projects based on content, sender, subject patterns"
  , "- Use lowercase tag form: 'wisp', 'superit', 'lune', 'paidright'"
  , "- Can assign multiple projects if activity relates to several"
  , "- Set confidence based on how clearly it matches the project"
  , "- If no clear project match, return empty projects array"
  , ""
  , "## Autonomy Tier Guidelines"
  , "- Tier 1: Automated notifications, newsletters, receipts"
  , "- Tier 2: FYI items, updates, non-urgent info"
  , "- Tier 3: Requests needing response, calendar invites"
  , "- Tier 4: Urgent items, important people, time-sensitive"
  ]
```

**Step 4: Run test to verify it passes**

Run: `cabal test --test-option=--match="/buildClassificationPromptWithProjects/"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Skills/Concierge/Classifier.hs wisp-srv/test/Skills/Concierge/ClassifierSpec.hs
git commit -m "feat: add project-aware classification prompt"
```

---

## Task 10: Service Layer - ProjectUpdater

**Files:**
- Create: `wisp-srv/src/Services/ProjectUpdater.hs`
- Test: `wisp-srv/test/Services/ProjectUpdaterSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Services/ProjectUpdaterSpec.hs`:

```haskell
module Services.ProjectUpdaterSpec where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=), decode)
import qualified Data.ByteString.Lazy as LBS
import Domain.Id (EntityId(..))
import Domain.Activity (NewActivity(..), ActivitySource(..))
import Domain.Document (NewDocument(..), DocumentType(..), ExtendedProjectData(..))
import Domain.ProjectClassification (ProjectAssignment(..))
import Infra.Db.Activity (insertActivity)
import Infra.Db.Document (insertDocument, getDocumentById)
import Services.ProjectUpdater
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = around withTestEnv $ do

  describe "updateProjectFromActivity" $ do
    it "increments activity count and updates last_activity_at" $ \env -> runTestApp env $ do
      -- Create project with initial state
      let projData = object
            [ "name" .= ("Wisp" :: String)
            , "type" .= ("work" :: String)
            , "summary" .= ("" :: String)
            , "status" .= ("active" :: String)
            , "participants" .= ([] :: [String])
            , "activity_count" .= (0 :: Int)
            ]
      let newDoc = NewDocument Nothing ProjectDoc projData ["wisp"] (Just 1.0) (Just "user") Nothing
      docId <- insertDocument newDoc

      -- Create activity
      let newAct = NewActivity
            { newActivityAccountId = EntityId "test-account"
            , newActivitySource = Email
            , newActivitySourceId = "email-1"
            , newActivityRaw = object []
            , newActivityTitle = Just "Wisp feature request"
            , newActivitySenderEmail = Just "alice@example.com"
            , newActivityStartsAt = Nothing
            , newActivityEndsAt = Nothing
            }
      actId <- insertActivity newAct

      -- Update project
      let assignment = ProjectAssignment "wisp" 0.9
      updateProjectFromActivity actId docId assignment (Just "alice@example.com")

      -- Verify
      mDoc <- getDocumentById docId
      case mDoc of
        Just doc -> do
          let Just projState = decode (LBS.fromStrict $ documentData doc) :: Maybe ExtendedProjectData
          liftIO $ extProjectActivityCount projState `shouldBe` 1
          liftIO $ extProjectParticipants projState `shouldContain` ["alice@example.com"]
        Nothing -> liftIO $ expectationFailure "Document not found"
```

**Step 2: Run test to verify it fails**

Run: `cabal test --test-option=--match="/ProjectUpdater/"`
Expected: FAIL - module not found

**Step 3: Create the service module**

Create `wisp-srv/src/Services/ProjectUpdater.hs`:

```haskell
module Services.ProjectUpdater
  ( updateProjectFromActivity
  , linkAndUpdateProject
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Result(..), Value, encode, decode, fromJSON, toJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub)
import Domain.Id (EntityId)
import Domain.Document (Document(..), ExtendedProjectData(..), NewDocumentLogEntry(..), LogSource(..))
import Domain.ActivityDocument (NewActivityDocument(..), LinkSource(..), Relationship(..))
import Domain.ProjectClassification (ProjectAssignment(..))
import Infra.Db.Document (getDocumentById, updateProjectData, insertDocumentLog)
import Infra.Db.ActivityDocument (linkActivityToDocument)
import App.Monad (App)

-- | Update a project document based on a newly classified activity
-- Performs immediate updates: increment count, update participants, link activity
updateProjectFromActivity :: EntityId -> EntityId -> ProjectAssignment -> Maybe Text -> App ()
updateProjectFromActivity activityId documentId assignment mSenderEmail = do
  -- Link activity to document
  let link = NewActivityDocument
        { newAdActivityId = activityId
        , newAdDocumentId = documentId
        , newAdRelationship = Project
        , newAdConfidence = Just (paConfidence assignment)
        , newAdSource = Classifier
        }
  linkActivityToDocument link

  -- Get current project state
  mDoc <- getDocumentById documentId
  case mDoc of
    Nothing -> pure ()  -- Document doesn't exist
    Just doc -> do
      -- Parse current state
      case fromJSON (documentData doc) :: Result ExtendedProjectData of
        Error _ -> pure ()  -- Invalid data, skip update
        Success projData -> do
          -- Update state
          let newParticipants = case mSenderEmail of
                Nothing -> extProjectParticipants projData
                Just email -> nub $ extProjectParticipants projData ++ [T.toLower email]
              newCount = extProjectActivityCount projData + 1
              updatedData = projData
                { extProjectParticipants = newParticipants
                , extProjectActivityCount = newCount
                }

          -- Save updated state
          updateProjectData documentId (toJSON updatedData)

          -- Log the change
          let logEntry = NewDocumentLogEntry
                { newLogDocumentId = documentId
                , newLogActivityId = Just activityId
                , newLogDescription = Just $ "Activity linked (confidence: " <> T.pack (show $ paConfidence assignment) <> ")"
                , newLogSource = LogClassifier
                , newLogAgentId = Nothing
                }
          _ <- insertDocumentLog logEntry
          pure ()

-- | Convenience function to look up project by tag and update
linkAndUpdateProject :: EntityId -> ProjectAssignment -> Maybe Text -> App ()
linkAndUpdateProject activityId assignment mSenderEmail = do
  -- Look up project by tag
  mDoc <- getProjectByTag (paName assignment)
  case mDoc of
    Nothing -> pure ()  -- Project not found, skip
    Just doc -> updateProjectFromActivity activityId (documentId doc) assignment mSenderEmail
  where
    getProjectByTag tag = do
      docs <- Infra.Db.Document.getDocumentsByTags [tag] True 1
      pure $ case docs of
        [d] -> Just d
        _ -> Nothing
```

**Step 4: Add to cabal file**

Add `Services.ProjectUpdater` and `Services.ProjectUpdaterSpec` to `wisp-srv.cabal`.

**Step 5: Run test to verify it passes**

Run: `cabal test --test-option=--match="/ProjectUpdater/"`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Services/ProjectUpdater.hs wisp-srv/test/Services/ProjectUpdaterSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add ProjectUpdater service for immediate updates"
```

---

## Task 11: Wire Classifier to ProjectUpdater

**Files:**
- Modify: `wisp-srv/src/Skills/Concierge.hs`
- Test: Integration test or modify existing

**Step 1: Understand current flow**

Read `Skills/Concierge.hs` to understand where classification results are processed.

**Step 2: Add project update call after classification**

After classification succeeds, for each project in `classificationProjects`:
1. Look up project document by tag
2. Call `updateProjectFromActivity`

The integration point is in the classification result handler. Add:

```haskell
import Services.ProjectUpdater (linkAndUpdateProject)
import Domain.ProjectClassification (ProjectAssignment(..))

-- After successful classification, in the handler:
forM_ (classificationProjects classification) $ \assignment ->
  linkAndUpdateProject activityId assignment (activitySenderEmail activity)
```

**Step 3: Update classifier to fetch and include projects**

Modify `classifyActivity` to:
1. Fetch all active projects
2. Extract project names
3. Use `buildClassificationPromptWithProjects` instead of `buildClassificationPrompt`

```haskell
import Infra.Db.Document (getAllProjects)
import Domain.Document (Document(..), ExtendedProjectData(..))

classifyActivityWithProjects :: Activity -> App (Either Text Classification)
classifyActivityWithProjects activity = do
  -- Fetch known projects
  projects <- getAllProjects
  let projectNames = mapMaybe extractProjectName projects

  cfg <- getConfig
  let apiKey = cfg.claude.apiKey
  let model = cfg.claude.model
  let source = sourceText (activitySource activity)
  let prompt = buildClassificationPromptWithProjects source (activityTitle activity) (activityRaw activity) projectNames
  result <- liftIO $ callClaude apiKey model prompt
  pure $ case result of
    Left err -> Left err
    Right respText -> parseClassificationResponse respText
  where
    extractProjectName doc = case fromJSON (documentData doc) of
      Success (pd :: ExtendedProjectData) -> Just (extProjectName pd)
      Error _ -> Nothing
```

**Step 4: Test the integration**

Run full test suite to ensure nothing broke:
Run: `cabal test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add wisp-srv/src/Skills/Concierge.hs wisp-srv/src/Skills/Concierge/Classifier.hs
git commit -m "feat: wire classifier to ProjectUpdater for automatic project assignment"
```

---

## Task 12: Batch Reflection - Basic Structure

**Files:**
- Create: `wisp-srv/src/Skills/Reflection.hs`
- Test: `wisp-srv/test/Skills/ReflectionSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Skills/ReflectionSpec.hs`:

```haskell
module Skills.ReflectionSpec where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Skills.Reflection
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = around withTestEnv $ do

  describe "getProjectsNeedingReflection" $ do
    it "returns projects with recent activity" $ \env -> runTestApp env $ do
      -- Seed projects exist from migration
      projects <- getProjectsNeedingReflection
      -- Initially no activities linked, so should return empty
      liftIO $ projects `shouldSatisfy` const True  -- Just verify it runs

  describe "buildReflectionPrompt" $ do
    it "formats project state and activities for LLM" $ do
      let prompt = buildReflectionPrompt "Wisp" "AI assistant project" ["email about feature X", "meeting notes"]
      prompt `shouldSatisfy` T.isInfixOf "Wisp"
      prompt `shouldSatisfy` T.isInfixOf "feature X"
```

**Step 2: Run test to verify it fails**

Run: `cabal test --test-option=--match="/Reflection/"`
Expected: FAIL - module not found

**Step 3: Create the reflection module**

Create `wisp-srv/src/Skills/Reflection.hs`:

```haskell
module Skills.Reflection
  ( runProjectReflection
  , getProjectsNeedingReflection
  , buildReflectionPrompt
  , ReflectionResult(..)
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Result(..), Value, encode, decode, fromJSON, toJSON, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (UTCTime)
import Domain.Id (EntityId)
import Domain.Document (Document(..), DocumentType(..), ExtendedProjectData(..), NewDocumentLogEntry(..), LogSource(..))
import Domain.Activity (Activity(..))
import Infra.Db.Document (getAllProjects, getDocumentById, updateProjectData, insertDocumentLog)
import Infra.Db.ActivityDocument (getDocumentActivities)
import Infra.Db.Activity (getActivity)
import App.Monad (App, getConfig)
import App.Config (Config(..), ClaudeConfig(..))
import Infra.Claude.Client (callClaude)

data ReflectionResult = ReflectionResult
  { rrProjectId :: EntityId
  , rrUpdatedSummary :: Text
  , rrUpdatedStatus :: Text
  , rrActivitiesProcessed :: Int
  } deriving (Show)

-- | Get projects that have new activities since last reflection
getProjectsNeedingReflection :: App [Document]
getProjectsNeedingReflection = do
  projects <- getAllProjects
  -- Filter to those with recent activity links
  -- For now, return all active projects
  pure projects

-- | Build the reflection prompt for a project
buildReflectionPrompt :: Text -> Text -> [Text] -> Text
buildReflectionPrompt projectName currentSummary activitySummaries = T.unlines
  [ "You are updating a project's knowledge state based on recent activities."
  , ""
  , "## Current Project State"
  , "Name: " <> projectName
  , "Summary: " <> if T.null currentSummary then "(no summary yet)" else currentSummary
  , ""
  , "## Recent Activities"
  , if null activitySummaries
      then "(no new activities)"
      else T.unlines $ map ("- " <>) activitySummaries
  , ""
  , "## Instructions"
  , "Based on these activities, provide an updated project state."
  , "Synthesize the activities into a coherent summary."
  , "Infer the project status: 'active', 'stalled', or 'completed'."
  , ""
  , "Respond with ONLY a JSON object:"
  , "{"
  , "  \"summary\": \"Updated project summary...\","
  , "  \"status\": \"active|stalled|completed\""
  , "}"
  ]

-- | Run reflection for all projects
runProjectReflection :: App [ReflectionResult]
runProjectReflection = do
  projects <- getProjectsNeedingReflection
  results <- forM projects reflectOnProject
  pure $ concat results

-- | Reflect on a single project
reflectOnProject :: Document -> App [ReflectionResult]
reflectOnProject doc = do
  -- Get linked activities
  links <- getDocumentActivities (documentId doc) 50
  if null links
    then pure []
    else do
      -- Fetch activity details
      activities <- forM links $ \link -> getActivity (adActivityId link)
      let actSummaries = mapMaybe (>>= activitySummary) activities

      -- Get current project state
      case fromJSON (documentData doc) :: Result ExtendedProjectData of
        Error _ -> pure []
        Success projData -> do
          let prompt = buildReflectionPrompt
                (extProjectName projData)
                (extProjectSummary projData)
                actSummaries

          -- Call LLM
          cfg <- getConfig
          result <- liftIO $ callClaude (cfg.claude.apiKey) (cfg.claude.model) prompt

          case result of
            Left _ -> pure []
            Right respText -> do
              -- Parse response and update
              case decode (TLE.encodeUtf8 $ TL.fromStrict respText) of
                Nothing -> pure []
                Just updates -> do
                  let newSummary = updates .: "summary" .!= extProjectSummary projData
                      newStatus = updates .: "status" .!= extProjectStatus projData
                      updatedData = projData
                        { extProjectSummary = newSummary
                        , extProjectStatus = newStatus
                        }

                  updateProjectData (documentId doc) (toJSON updatedData)

                  -- Log the reflection
                  let logEntry = NewDocumentLogEntry
                        { newLogDocumentId = documentId doc
                        , newLogActivityId = Nothing
                        , newLogDescription = Just $ "Reflection: processed " <> T.pack (show $ length links) <> " activities"
                        , newLogSource = LogReflection
                        , newLogAgentId = Nothing
                        }
                  _ <- insertDocumentLog logEntry

                  pure [ReflectionResult
                    { rrProjectId = documentId doc
                    , rrUpdatedSummary = newSummary
                    , rrUpdatedStatus = newStatus
                    , rrActivitiesProcessed = length links
                    }]
  where
    adActivityId = Domain.ActivityDocument.adActivityId
```

**Step 4: Add to cabal file**

Add `Skills.Reflection` and `Skills.ReflectionSpec` to `wisp-srv.cabal`.

**Step 5: Run test to verify it passes**

Run: `cabal test --test-option=--match="/Reflection/"`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Skills/Reflection.hs wisp-srv/test/Skills/ReflectionSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Reflection skill for batch project synthesis"
```

---

## Task 13: CLI Command for Reflection

**Files:**
- Modify: `wisp-cli/app/Main.hs` or appropriate CLI module

**Step 1: Add reflect command**

Add a new CLI command `reflect` that triggers `runProjectReflection`:

```haskell
reflectCmd :: Command
reflectCmd = command "reflect" "Run project reflection" $ do
  pure $ runReflection

runReflection :: App ()
runReflection = do
  liftIO $ putStrLn "Running project reflection..."
  results <- runProjectReflection
  forM_ results $ \r -> do
    liftIO $ putStrLn $ "Updated: " <> T.unpack (rrProjectId r)
    liftIO $ putStrLn $ "  Summary: " <> T.unpack (rrUpdatedSummary r)
    liftIO $ putStrLn $ "  Status: " <> T.unpack (rrUpdatedStatus r)
    liftIO $ putStrLn $ "  Activities: " <> show (rrActivitiesProcessed r)
  liftIO $ putStrLn $ "Reflection complete. " <> show (length results) <> " projects updated."
```

**Step 2: Test manually**

Run: `cabal run wisp-cli -- reflect`
Expected: Runs without error, outputs reflection results

**Step 3: Commit**

```bash
git add wisp-cli/app/Main.hs
git commit -m "feat: add CLI reflect command for batch project reflection"
```

---

## Task 14: New Project Suggestion Detection

**Files:**
- Modify: `wisp-srv/src/Skills/Reflection.hs`
- Create: `wisp-srv/src/Domain/ProjectSuggestion.hs`

**Step 1: Add ProjectSuggestion type**

Create `wisp-srv/src/Domain/ProjectSuggestion.hs`:

```haskell
module Domain.ProjectSuggestion
  ( ProjectSuggestion(..)
  , ClusterKey(..)
  ) where

import Data.Text (Text)
import Domain.Id (EntityId)

newtype ClusterKey = ClusterKey Text
  deriving (Eq, Show)

data ProjectSuggestion = ProjectSuggestion
  { psSuggestedName :: Text
  , psClusterKey :: ClusterKey
  , psReason :: Text
  , psSampleActivityIds :: [EntityId]
  , psActivityCount :: Int
  } deriving (Eq, Show)
```

**Step 2: Add cluster detection to Reflection**

Add to `Skills/Reflection.hs`:

```haskell
-- | Detect clusters of unassigned activities that might be new projects
detectProjectClusters :: App [ProjectSuggestion]
detectProjectClusters = do
  -- Get activities without project links from last 7 days
  unassigned <- getUnassignedActivities 7

  -- Group by sender domain
  let byDomain = groupBy senderDomain unassigned

  -- Filter to clusters with 5+ activities
  let significantClusters = filter ((>= 5) . length . snd) byDomain

  -- Convert to suggestions
  pure $ map toSuggestion significantClusters
  where
    senderDomain act = case activitySenderEmail act of
      Just email -> T.takeWhileEnd (/= '@') email
      Nothing -> "unknown"

    toSuggestion (domain, acts) = ProjectSuggestion
      { psSuggestedName = domain
      , psClusterKey = ClusterKey $ "domain:" <> domain
      , psReason = T.pack (show $ length acts) <> " activities from " <> domain
      , psSampleActivityIds = take 3 $ map activityId acts
      , psActivityCount = length acts
      }
```

**Step 3: Add dismissed check**

Query `dismissed_suggestions` table before surfacing.

**Step 4: Commit**

```bash
git add wisp-srv/src/Domain/ProjectSuggestion.hs wisp-srv/src/Skills/Reflection.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add project cluster detection for new project suggestions"
```

---

## Task 15: Surface Suggestions in Approvals

**Files:**
- Modify: `wisp-srv/src/Skills/Reflection.hs`
- Possibly modify Approvals-related code

**Step 1: Create synthetic activity for suggestion**

When a cluster is detected and not dismissed:

```haskell
surfaceProjectSuggestion :: ProjectSuggestion -> App ()
surfaceProjectSuggestion suggestion = do
  -- Create a synthetic activity
  let newAct = NewActivity
        { newActivityAccountId = systemAccountId  -- Need a system account
        , newActivitySource = Note  -- Or a new "Reflection" source
        , newActivitySourceId = "suggestion-" <> unClusterKey (psClusterKey suggestion)
        , newActivityRaw = toJSON suggestion
        , newActivityTitle = Just $ "New project suggestion: " <> psSuggestedName suggestion
        , newActivitySenderEmail = Nothing
        , newActivityStartsAt = Nothing
        , newActivityEndsAt = Nothing
        }
  actId <- insertActivity newAct

  -- Set status to needs_review
  updateActivityStatus actId NeedsReview
```

**Step 2: Handle approval/dismissal**

When user approves (y):
1. Create new project document
2. Link sample activities to it

When user dismisses (x):
1. Insert into `dismissed_suggestions`

**Step 3: Commit**

```bash
git add wisp-srv/src/Skills/Reflection.hs
git commit -m "feat: surface project suggestions through Approvals view"
```

---

## Summary

This plan covers:

1. **Tasks 1-2**: Database migrations for join table and seed projects
2. **Tasks 3-5**: Domain types for extended projects, activity-document links, and project assignments
3. **Tasks 6-7**: Database layer for CRUD operations
4. **Tasks 8-9**: Extend classifier to output and prompt for projects
5. **Tasks 10-11**: ProjectUpdater service and wiring to classifier
6. **Tasks 12-13**: Batch reflection skill and CLI command
7. **Tasks 14-15**: New project suggestion detection and surfacing

Each task is designed to be completable in 2-5 minutes with clear test-first steps.
