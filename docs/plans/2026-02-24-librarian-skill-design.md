# Librarian Skill Design

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Create a scheduled skill that maintains project knowledge by reviewing linked activities and enriching with external context. Acts as a "librarian" - collecting and collating knowledge rather than directing work.

**Trigger:** Scheduled (nightly by default), runs for all active projects with recent activity.

**Tech Stack:** Haskell, PostgreSQL, existing Skills (GitHub, Research) for enrichment.

---

## Overview

The Librarian skill creates and maintains four child document types for each project:

1. **Product Research** - Vision, unique value proposition, key contacts, open questions
2. **Roadmap** - Timeline, milestones, key dates
3. **Architecture** - Users/personas, specs links, testing, code structure, data structure, infrastructure
4. **Activity Log** - Summary of recent activity (day/week/month/year)

Documents are versioned using `supersedes_id` chains. The agent decides whether each document needs updating based on new activities - no code heuristics.

---

## Task 1: Database Migration - Add parent_id to Documents

**Files:**
- Create: `wisp-srv/migrations/025_document_parent_id.sql`

**Step 1: Write the migration**

```sql
-- 025_document_parent_id.sql
-- Add parent_id for hierarchical document relationships (e.g., project -> knowledge docs)

ALTER TABLE documents ADD COLUMN parent_id TEXT REFERENCES documents(id);
CREATE INDEX idx_documents_parent ON documents(parent_id);
```

**Step 2: Run migration**

```bash
psql postgres://localhost/wisp -f wisp-srv/migrations/025_document_parent_id.sql
```

**Step 3: Commit**

```bash
git add wisp-srv/migrations/025_document_parent_id.sql
git commit -m "feat: add parent_id to documents for hierarchical relationships"
```

---

## Task 2: Domain Types - ProjectKnowledge

**Files:**
- Create: `wisp-srv/src/Domain/ProjectKnowledge.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create the domain module**

```haskell
module Domain.ProjectKnowledge
  ( ProjectKnowledgeKind(..)
  , ProductResearchData(..)
  , RoadmapData(..)
  , ArchitectureData(..)
  , ActivityLogData(..)
  , KeyContact(..)
  , OpenQuestion(..)
  , Milestone(..)
  , ActivityHighlight(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | The four kinds of project knowledge documents
data ProjectKnowledgeKind
  = ProductResearch
  | Roadmap
  | Architecture
  | ActivityLog
  deriving (Eq, Show, Generic)

instance ToJSON ProjectKnowledgeKind where
  toJSON ProductResearch = "product_research"
  toJSON Roadmap = "roadmap"
  toJSON Architecture = "architecture"
  toJSON ActivityLog = "activity_log"

instance FromJSON ProjectKnowledgeKind where
  parseJSON = withText "ProjectKnowledgeKind" $ \case
    "product_research" -> pure ProductResearch
    "roadmap" -> pure Roadmap
    "architecture" -> pure Architecture
    "activity_log" -> pure ActivityLog
    other -> fail $ "Unknown kind: " <> show other

-- | Product Research document data
data KeyContact = KeyContact
  { kcName :: Text
  , kcEmail :: Maybe Text
  , kcRole :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON KeyContact where
  toJSON kc = object
    [ "name" .= kcName kc
    , "email" .= kcEmail kc
    , "role" .= kcRole kc
    ]

instance FromJSON KeyContact where
  parseJSON = withObject "KeyContact" $ \v -> KeyContact
    <$> v .: "name"
    <*> v .:? "email"
    <*> v .:? "role"

data OpenQuestion = OpenQuestion
  { oqQuestion :: Text
  , oqRaisedAt :: Maybe Text
  , oqContext :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON OpenQuestion where
  toJSON oq = object
    [ "question" .= oqQuestion oq
    , "raised_at" .= oqRaisedAt oq
    , "context" .= oqContext oq
    ]

instance FromJSON OpenQuestion where
  parseJSON = withObject "OpenQuestion" $ \v -> OpenQuestion
    <$> v .: "question"
    <*> v .:? "raised_at"
    <*> v .:? "context"

data ProductResearchData = ProductResearchData
  { prVision :: Maybe Text
  , prValueProposition :: Maybe Text
  , prKeyContacts :: [KeyContact]
  , prOpenQuestions :: [OpenQuestion]
  } deriving (Eq, Show, Generic)

instance ToJSON ProductResearchData where
  toJSON pr = object
    [ "kind" .= ("product_research" :: Text)
    , "vision" .= prVision pr
    , "value_proposition" .= prValueProposition pr
    , "key_contacts" .= prKeyContacts pr
    , "open_questions" .= prOpenQuestions pr
    ]

instance FromJSON ProductResearchData where
  parseJSON = withObject "ProductResearchData" $ \v -> ProductResearchData
    <$> v .:? "vision"
    <*> v .:? "value_proposition"
    <*> v .:? "key_contacts" .!= []
    <*> v .:? "open_questions" .!= []

-- | Roadmap document data
data Milestone = Milestone
  { msTitle :: Text
  , msDate :: Maybe Text
  , msStatus :: Maybe Text  -- "planned", "in_progress", "completed"
  , msSource :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON Milestone where
  toJSON ms = object
    [ "title" .= msTitle ms
    , "date" .= msDate ms
    , "status" .= msStatus ms
    , "source" .= msSource ms
    ]

instance FromJSON Milestone where
  parseJSON = withObject "Milestone" $ \v -> Milestone
    <$> v .: "title"
    <*> v .:? "date"
    <*> v .:? "status"
    <*> v .:? "source"

data RoadmapData = RoadmapData
  { rmMilestones :: [Milestone]
  , rmTimelineNotes :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON RoadmapData where
  toJSON rm = object
    [ "kind" .= ("roadmap" :: Text)
    , "milestones" .= rmMilestones rm
    , "timeline_notes" .= rmTimelineNotes rm
    ]

instance FromJSON RoadmapData where
  parseJSON = withObject "RoadmapData" $ \v -> RoadmapData
    <$> v .:? "milestones" .!= []
    <*> v .:? "timeline_notes"

-- | Architecture document data
data ArchitectureData = ArchitectureData
  { archUsersPersonas :: [Text]
  , archSpecsLinks :: [Text]
  , archTesting :: Maybe Text
  , archCodeStructure :: Maybe Text
  , archDataStructure :: Maybe Text
  , archInfrastructure :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON ArchitectureData where
  toJSON arch = object
    [ "kind" .= ("architecture" :: Text)
    , "users_personas" .= archUsersPersonas arch
    , "specs_links" .= archSpecsLinks arch
    , "testing" .= archTesting arch
    , "code_structure" .= archCodeStructure arch
    , "data_structure" .= archDataStructure arch
    , "infrastructure" .= archInfrastructure arch
    ]

instance FromJSON ArchitectureData where
  parseJSON = withObject "ArchitectureData" $ \v -> ArchitectureData
    <$> v .:? "users_personas" .!= []
    <*> v .:? "specs_links" .!= []
    <*> v .:? "testing"
    <*> v .:? "code_structure"
    <*> v .:? "data_structure"
    <*> v .:? "infrastructure"

-- | Activity Log document data
data ActivityHighlight = ActivityHighlight
  { ahDate :: Text
  , ahDescription :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON ActivityHighlight where
  toJSON ah = object
    [ "date" .= ahDate ah
    , "description" .= ahDescription ah
    ]

instance FromJSON ActivityHighlight where
  parseJSON = withObject "ActivityHighlight" $ \v -> ActivityHighlight
    <$> v .: "date"
    <*> v .: "description"

data ActivityLogData = ActivityLogData
  { alGeneratedAt :: UTCTime
  , alPeriod :: Text  -- "day", "week", "month", "year"
  , alSummary :: Text
  , alHighlights :: [ActivityHighlight]
  } deriving (Eq, Show, Generic)

instance ToJSON ActivityLogData where
  toJSON al = object
    [ "kind" .= ("activity_log" :: Text)
    , "generated_at" .= alGeneratedAt al
    , "period" .= alPeriod al
    , "summary" .= alSummary al
    , "highlights" .= alHighlights al
    ]

instance FromJSON ActivityLogData where
  parseJSON = withObject "ActivityLogData" $ \v -> ActivityLogData
    <$> v .: "generated_at"
    <*> v .: "period"
    <*> v .: "summary"
    <*> v .:? "highlights" .!= []
```

**Step 2: Add to cabal file**

Add `Domain.ProjectKnowledge` to the library's `other-modules` in `wisp-srv.cabal`.

**Step 3: Commit**

```bash
git add wisp-srv/src/Domain/ProjectKnowledge.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add ProjectKnowledge domain types for librarian skill"
```

---

## Task 3: Extend Document Domain and Database Layer

**Files:**
- Modify: `wisp-srv/src/Domain/Document.hs`
- Modify: `wisp-srv/src/Infra/Db/Document.hs`

**Step 1: Add ProjectKnowledgeDoc to DocumentType**

In `Domain/Document.hs`:

```haskell
data DocumentType = ProjectDoc | NoteDoc | PreferenceDoc | ProjectKnowledgeDoc
  deriving (Eq, Show, Generic)
```

Update ToJSON/FromJSON instances to handle `"project_knowledge"`.

**Step 2: Add parent_id to Document record**

```haskell
data Document = Document
  { documentId :: EntityId
  , documentTenantId :: Maybe TenantId
  , documentType :: DocumentType
  , documentData :: Value
  , documentTags :: [Text]
  , documentConfidence :: Maybe Double
  , documentSource :: Maybe Text
  , documentActive :: Bool
  , documentCreatedAt :: UTCTime
  , documentArchivedAt :: Maybe UTCTime
  , documentSupersedesId :: Maybe EntityId
  , documentParentId :: Maybe EntityId  -- NEW
  , documentLastActivityAt :: Maybe UTCTime
  }
```

**Step 3: Add NewDocument parent field**

```haskell
data NewDocument = NewDocument
  { -- existing fields...
  , newDocParentId :: Maybe EntityId  -- NEW
  }
```

**Step 4: Update FromRow instance**

Update the `FromRow Document` instance in `Infra/Db/Document.hs` to read `parent_id`.

**Step 5: Add new queries**

```haskell
-- | Get all child documents for a project
getProjectChildren :: EntityId -> App [Document]
getProjectChildren projectId = do
  conn <- getConn
  liftIO $ query conn
    "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
    \created_at, archived_at, supersedes_id, parent_id, last_activity_at \
    \FROM documents \
    \WHERE parent_id = ? AND active = TRUE \
    \ORDER BY created_at DESC"
    (Only $ unEntityId projectId)

-- | Get latest version of a specific knowledge kind for a project
getLatestKnowledgeByKind :: EntityId -> Text -> App (Maybe Document)
getLatestKnowledgeByKind projectId kind = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
    \created_at, archived_at, supersedes_id, parent_id, last_activity_at \
    \FROM documents \
    \WHERE parent_id = ? AND type = 'project_knowledge' \
    \AND data->>'kind' = ? AND active = TRUE \
    \ORDER BY created_at DESC LIMIT 1"
    (unEntityId projectId, kind)
  pure $ case results of
    [doc] -> Just doc
    _ -> Nothing

-- | Insert document that supersedes another (for versioning)
insertWithSupersedes :: NewDocument -> EntityId -> App EntityId
insertWithSupersedes newDoc oldDocId = do
  -- Deactivate old version
  conn <- getConn
  _ <- liftIO $ execute conn
    "UPDATE documents SET active = FALSE WHERE id = ?"
    (Only $ unEntityId oldDocId)
  -- Insert new version with supersedes_id
  insertDocument newDoc { newDocSupersedesId = Just oldDocId }
```

**Step 6: Commit**

```bash
git add wisp-srv/src/Domain/Document.hs wisp-srv/src/Infra/Db/Document.hs
git commit -m "feat: add parent_id to documents and knowledge queries"
```

---

## Task 4: Librarian Skill - Core Structure

**Files:**
- Create: `wisp-srv/src/Skills/Librarian.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create the skill module**

```haskell
module Skills.Librarian
  ( runLibrarian
  , runLibrarianForProject
  , LibrarianResult(..)
  ) where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode, decode, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (getCurrentTime)
import Domain.Id (EntityId)
import Domain.Document (Document(..), DocumentType(..), NewDocument(..))
import Domain.Activity (Activity(..))
import Domain.ProjectKnowledge
import Infra.Db.Document (getAllProjects, getProjectChildren, getLatestKnowledgeByKind, insertDocument, insertWithSupersedes)
import Infra.Db.ActivityDocument (getDocumentActivities)
import Infra.Db.Activity (getActivity)
import App.Monad (App, getConfig)
import App.Config (Config(..), ClaudeConfig(..))
import Infra.Claude.Client (callClaude)
import qualified Skills.GitHub as GitHub
import qualified Skills.Research as Research

data LibrarianResult = LibrarianResult
  { lrProjectId :: EntityId
  , lrProjectName :: Text
  , lrUpdatedDocs :: [ProjectKnowledgeKind]
  , lrSkippedDocs :: [ProjectKnowledgeKind]
  } deriving (Show)

-- | Run librarian for all active projects with recent activity
runLibrarian :: App [LibrarianResult]
runLibrarian = do
  projects <- getAllProjects
  results <- forM projects runLibrarianForProject
  pure $ catMaybes results

-- | Run librarian for a single project
runLibrarianForProject :: Document -> App (Maybe LibrarianResult)
runLibrarianForProject project = do
  -- 1. Gather context
  let projectId = documentId project

  -- Get linked activities
  activityLinks <- getDocumentActivities projectId 100
  activities <- fmap catMaybes $ forM activityLinks $ \link ->
    getActivity (adActivityId link)

  -- Skip if no activities
  when (null activities) $ pure Nothing

  -- Get existing child documents
  children <- getProjectChildren projectId
  let currentResearch = findByKind "product_research" children
      currentRoadmap = findByKind "roadmap" children
      currentArch = findByKind "architecture" children
      currentLog = findByKind "activity_log" children

  -- 2. Enrich (conditionally)
  enrichment <- gatherEnrichment project activities

  -- 3. Synthesize via LLM
  let prompt = buildLibrarianPrompt project activities
        currentResearch currentRoadmap currentArch currentLog enrichment

  cfg <- getConfig
  result <- liftIO $ callClaude (cfg.claude.apiKey) (cfg.claude.model) prompt

  case result of
    Left _ -> pure Nothing
    Right respText -> do
      -- 4. Persist updates
      updates <- parseAndPersist projectId respText
        currentResearch currentRoadmap currentArch currentLog
      pure $ Just updates

-- | Find child document by kind
findByKind :: Text -> [Document] -> Maybe Document
findByKind kind docs =
  listToMaybe [d | d <- docs, matchesKind kind d]
  where
    matchesKind k doc = case Aeson.decode (Aeson.encode $ documentData doc) of
      Just obj -> case Aeson.lookup "kind" obj of
        Just (Aeson.String k') -> k == k'
        _ -> False
      _ -> False

-- | Gather enrichment data from GitHub and Research skills
gatherEnrichment :: Document -> [Activity] -> App Value
gatherEnrichment project activities = do
  -- Check if project has GitHub repo info
  let tags = documentTags project
      hasGitHub = any (T.isPrefixOf "repo:") tags

  githubData <- if hasGitHub
    then do
      -- Extract repo from tags like "repo:owner/name"
      let repoTag = head [t | t <- tags, T.isPrefixOf "repo:" t]
          repo = T.drop 5 repoTag
      result <- GitHub.executeToolCall $ GitHub.ListCommits $ GitHub.ListCommitsQuery repo Nothing Nothing (Just 10)
      pure $ case result of
        GitHub.ToolSuccess v -> Just v
        GitHub.ToolError _ -> Nothing
    else pure Nothing

  pure $ object
    [ "github" .= githubData
    ]

-- | Build the LLM prompt
buildLibrarianPrompt :: Document -> [Activity] -> Maybe Document -> Maybe Document -> Maybe Document -> Maybe Document -> Value -> Text
buildLibrarianPrompt project activities mResearch mRoadmap mArch mLog enrichment = T.unlines
  [ "You are a project librarian maintaining knowledge documents."
  , ""
  , "## Project"
  , "Name: " <> extractProjectName project
  , "Summary: " <> extractProjectSummary project
  , ""
  , "## Current Knowledge Documents"
  , ""
  , "### Product Research"
  , maybe "Not yet created" (formatDoc) mResearch
  , ""
  , "### Roadmap"
  , maybe "Not yet created" (formatDoc) mRoadmap
  , ""
  , "### Architecture"
  , maybe "Not yet created" (formatDoc) mArch
  , ""
  , "### Activity Log"
  , maybe "Not yet created" (formatDoc) mLog
  , ""
  , "## New Activities Since Last Run"
  , T.unlines $ map formatActivity activities
  , ""
  , "## Enrichment Data"
  , TL.toStrict $ TLE.decodeUtf8 $ encode enrichment
  , ""
  , "## Instructions"
  , "Review the new activities and enrichment data. For each document type, decide:"
  , "- \"no_change\" if nothing meaningful to add"
  , "- Provide updated content if there's valuable information to incorporate"
  , ""
  , "Respond with ONLY this JSON structure:"
  , "{"
  , "  \"product_research\": \"no_change\" | { full updated content },"
  , "  \"roadmap\": \"no_change\" | { full updated content },"
  , "  \"architecture\": \"no_change\" | { full updated content },"
  , "  \"activity_log\": \"no_change\" | { full updated content }"
  , "}"
  ]
  where
    formatDoc doc = TL.toStrict $ TLE.decodeUtf8 $ encode (documentData doc)
    formatActivity act = "- " <> fromMaybe "(no summary)" (activitySummary act)

extractProjectName :: Document -> Text
extractProjectName doc = case Aeson.decode (Aeson.encode $ documentData doc) of
  Just obj -> case Aeson.lookup "name" obj of
    Just (Aeson.String n) -> n
    _ -> "Unknown"
  _ -> "Unknown"

extractProjectSummary :: Document -> Text
extractProjectSummary doc = case Aeson.decode (Aeson.encode $ documentData doc) of
  Just obj -> case Aeson.lookup "summary" obj of
    Just (Aeson.String s) -> s
    _ -> ""
  _ -> ""

-- | Parse LLM response and persist updates
parseAndPersist :: EntityId -> Text -> Maybe Document -> Maybe Document -> Maybe Document -> Maybe Document -> App LibrarianResult
parseAndPersist projectId respText mResearch mRoadmap mArch mLog = do
  now <- liftIO getCurrentTime

  -- Parse response
  let mResp = decode (TLE.encodeUtf8 $ TL.fromStrict respText)

  case mResp of
    Nothing -> pure $ LibrarianResult projectId "" [] [ProductResearch, Roadmap, Architecture, ActivityLog]
    Just resp -> do
      -- Process each document type
      updatedResearch <- processUpdate projectId "product_research" (resp Aeson..: "product_research") mResearch
      updatedRoadmap <- processUpdate projectId "roadmap" (resp Aeson..: "roadmap") mRoadmap
      updatedArch <- processUpdate projectId "architecture" (resp Aeson..: "architecture") mArch
      updatedLog <- processUpdate projectId "activity_log" (resp Aeson..: "activity_log") mLog

      let updated = catMaybes [updatedResearch, updatedRoadmap, updatedArch, updatedLog]
          skipped = [ProductResearch, Roadmap, Architecture, ActivityLog] \\ updated

      pure $ LibrarianResult projectId "" updated skipped

-- | Process a single document update
processUpdate :: EntityId -> Text -> Maybe Value -> Maybe Document -> App (Maybe ProjectKnowledgeKind)
processUpdate projectId kind mValue mExisting = case mValue of
  Nothing -> pure Nothing
  Just (Aeson.String "no_change") -> pure Nothing
  Just content -> do
    let newDoc = NewDocument
          { newDocTenantId = Nothing
          , newDocType = ProjectKnowledgeDoc
          , newDocData = content
          , newDocTags = ["project-knowledge", kind]
          , newDocConfidence = Just 1.0
          , newDocSource = Just "librarian"
          , newDocSupersedesId = documentId <$> mExisting
          , newDocParentId = Just projectId
          }
    case mExisting of
      Nothing -> do
        _ <- insertDocument newDoc
        pure ()
      Just existing -> do
        _ <- insertWithSupersedes newDoc (documentId existing)
        pure ()
    pure $ textToKind kind
  where
    textToKind "product_research" = Just ProductResearch
    textToKind "roadmap" = Just Roadmap
    textToKind "architecture" = Just Architecture
    textToKind "activity_log" = Just ActivityLog
    textToKind _ = Nothing
```

**Step 2: Add to cabal file**

Add `Skills.Librarian` to `wisp-srv.cabal`.

**Step 3: Commit**

```bash
git add wisp-srv/src/Skills/Librarian.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Librarian skill for project knowledge maintenance"
```

---

## Task 5: Scheduler Integration

**Files:**
- Modify: `wisp-srv/src/Skills/Scheduler.hs`

**Step 1: Import Librarian**

```haskell
import qualified Skills.Librarian as Librarian
```

**Step 2: Add to scheduled tasks**

In the nightly task runner (or create one if it doesn't exist):

```haskell
runNightlyTasks :: App ()
runNightlyTasks = do
  -- Run librarian for all projects
  results <- Librarian.runLibrarian
  forM_ results $ \r -> do
    logInfo $ "Librarian processed " <> lrProjectName r
      <> ": updated " <> T.pack (show $ length $ lrUpdatedDocs r) <> " docs"
```

**Step 3: Commit**

```bash
git add wisp-srv/src/Skills/Scheduler.hs
git commit -m "feat: integrate Librarian skill into nightly scheduler"
```

---

## Task 6: CLI Command for Manual Run

**Files:**
- Modify: `wisp-cli/app/Main.hs` (or appropriate CLI module)

**Step 1: Add librarian command**

```haskell
librarianCmd :: Command
librarianCmd = command "librarian" "Run project librarian to update knowledge documents" $ do
  projectArg <- optional $ argument str (metavar "PROJECT")
  pure $ runLibrarianCli projectArg

runLibrarianCli :: Maybe Text -> App ()
runLibrarianCli mProject = do
  case mProject of
    Nothing -> do
      liftIO $ putStrLn "Running librarian for all projects..."
      results <- Librarian.runLibrarian
      forM_ results printResult
      liftIO $ putStrLn $ "Done. Processed " <> show (length results) <> " projects."
    Just projectName -> do
      liftIO $ putStrLn $ "Running librarian for project: " <> T.unpack projectName
      mDoc <- getProjectByTag projectName
      case mDoc of
        Nothing -> liftIO $ putStrLn "Project not found."
        Just doc -> do
          mResult <- Librarian.runLibrarianForProject doc
          case mResult of
            Nothing -> liftIO $ putStrLn "No updates needed."
            Just result -> printResult result
  where
    printResult r = liftIO $ do
      putStrLn $ "Project: " <> T.unpack (lrProjectName r)
      putStrLn $ "  Updated: " <> show (lrUpdatedDocs r)
      putStrLn $ "  Skipped: " <> show (lrSkippedDocs r)
```

**Step 2: Commit**

```bash
git add wisp-cli/app/Main.hs
git commit -m "feat: add CLI command for manual librarian runs"
```

---

## Task 7: TUI - Display Project Knowledge Documents

**Files:**
- Modify: `wisp-tui/src/Tui/Views/Knowledge.hs`

**Step 1: When viewing a project, show child documents**

Add a sub-view that appears when expanding a project, showing its child knowledge documents (Product Research, Roadmap, Architecture, Activity Log).

**Step 2: Commit**

```bash
git add wisp-tui/src/Tui/Views/Knowledge.hs
git commit -m "feat: display project knowledge documents in TUI"
```

---

## Summary

| Task | Description |
|------|-------------|
| 1 | Database migration - add `parent_id` to documents |
| 2 | Domain types - `ProjectKnowledge.hs` with four document kinds |
| 3 | Extend Document domain and database layer with parent queries |
| 4 | Librarian skill - core structure with gather/enrich/synthesize/persist flow |
| 5 | Scheduler integration - run librarian nightly |
| 6 | CLI command - manual librarian runs |
| 7 | TUI - display project knowledge documents |

The Librarian skill:
- Runs on schedule (nightly) for all projects with recent activity
- Gathers linked activities and enriches with GitHub/Research data
- Uses LLM to decide which of 4 document types need updating
- Creates versioned snapshots via `supersedes_id` chain
- Child documents linked to project via `parent_id`
