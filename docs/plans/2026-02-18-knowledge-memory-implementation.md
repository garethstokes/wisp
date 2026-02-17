# Knowledge + Memory System Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement shared knowledge (tagged notes) and per-agent memory (sessions, summaries, souls) for Wisp.

**Architecture:** Notes are Activities with `source: Note`, classified through existing pipeline. Tags are normalized and stored in a many-to-many table. Sessions store raw conversation messages per-agent, summaries compress old sessions, souls hold personality and insights.

**Tech Stack:** Haskell, PostgreSQL, existing App monad, Aeson for JSON

---

## Phase 1: Foundation (Tags + Notes)

### Task 1: Database Migration for Tags

**Files:**
- Create: `wisp-srv/migrations/012_tags.sql`

**Step 1: Write the migration**

```sql
-- 012_tags.sql
-- Tags for knowledge categorization

CREATE TABLE tags (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name TEXT UNIQUE NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE activity_tags (
  activity_id UUID REFERENCES activities(id) ON DELETE CASCADE,
  tag_id UUID REFERENCES tags(id) ON DELETE CASCADE,
  PRIMARY KEY (activity_id, tag_id)
);

CREATE INDEX idx_activity_tags_activity ON activity_tags(activity_id);
CREATE INDEX idx_activity_tags_tag ON activity_tags(tag_id);

-- Add parent_id for note supersession
ALTER TABLE activities ADD COLUMN parent_id UUID REFERENCES activities(id);
```

**Step 2: Verify migration file exists**

Run: `ls -la wisp-srv/migrations/012_tags.sql`
Expected: File exists

**Step 3: Commit**

```bash
git add wisp-srv/migrations/012_tags.sql
git commit -m "feat(db): add tags and activity_tags tables"
```

---

### Task 2: Tag Domain Model

**Files:**
- Create: `wisp-srv/src/Domain/Tag.hs`
- Modify: `wisp-srv/wisp-srv.cabal` (add Domain.Tag to other-modules)

**Step 1: Write the failing test**

Create: `wisp-srv/test/Domain/TagSpec.hs`

```haskell
module Domain.TagSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode)
import Domain.Tag

spec :: Spec
spec = describe "Tag" $ do
  describe "JSON serialization" $ do
    it "serializes Tag to JSON" $ do
      let tag = Tag (TagId "abc123") "superit"
      encode tag `shouldContain` "\"name\":\"superit\""

    it "normalizes tag names to lowercase" $ do
      normalizeTagName "SuperIT" `shouldBe` "superit"
      normalizeTagName "ALICE" `shouldBe` "alice"
      normalizeTagName "my-project" `shouldBe` "my-project"
```

**Step 2: Run test to verify it fails**

Run: `cd wisp-srv && cabal test --test-option=--match="Tag"`
Expected: FAIL - module Domain.Tag not found

**Step 3: Write Domain/Tag.hs**

```haskell
module Domain.Tag
  ( Tag(..)
  , TagId(..)
  , NewTag(..)
  , normalizeTagName
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)

newtype TagId = TagId { unTagId :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

data Tag = Tag
  { tagId :: TagId
  , tagName :: Text
  } deriving (Eq, Show)

instance ToJSON Tag where
  toJSON t = object
    [ "id" .= tagId t
    , "name" .= tagName t
    ]

instance FromJSON Tag where
  parseJSON = withObject "Tag" $ \v -> Tag
    <$> v .: "id"
    <*> v .: "name"

data NewTag = NewTag
  { newTagName :: Text
  } deriving (Eq, Show)

-- Normalize tag names: lowercase, trimmed
normalizeTagName :: Text -> Text
normalizeTagName = T.toLower . T.strip
```

**Step 4: Add to cabal file**

Add `Domain.Tag` and `Domain.TagSpec` to the appropriate other-modules sections in `wisp-srv.cabal`.

**Step 5: Run test to verify it passes**

Run: `cd wisp-srv && cabal test --test-option=--match="Tag"`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Domain/Tag.hs wisp-srv/test/Domain/TagSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(domain): add Tag model with normalization"
```

---

### Task 3: Tag Database Layer

**Files:**
- Create: `wisp-srv/src/Infra/Db/Tag.hs`
- Create: `wisp-srv/test/Infra/Db/TagSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

```haskell
module Infra.Db.TagSpec (spec) where

import Test.Hspec
import Domain.Tag (Tag(..), TagId(..), normalizeTagName)
import Infra.Db.Tag
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = describe "Infra.Db.Tag" $ around withTestEnv $ do
  describe "insertTag" $ do
    it "creates a new tag with normalized name" $ \env -> runTestApp env $ do
      mTag <- insertTag "SuperIT"
      case mTag of
        Just tag -> do
          liftIO $ tagName tag `shouldBe` "superit"
        Nothing -> liftIO $ expectationFailure "Expected tag to be created"

  describe "getOrCreateTag" $ do
    it "returns existing tag if name exists" $ \env -> runTestApp env $ do
      tag1 <- getOrCreateTag "alice"
      tag2 <- getOrCreateTag "ALICE"  -- different case
      liftIO $ tagId tag1 `shouldBe` tagId tag2

  describe "getTagsByActivity" $ do
    it "returns empty list for activity with no tags" $ \env -> runTestApp env $ do
      tags <- getTagsByActivity (EntityId "nonexistent")
      liftIO $ tags `shouldBe` []
```

**Step 2: Run test to verify it fails**

Run: `cd wisp-srv && cabal test --test-option=--match="Infra.Db.Tag"`
Expected: FAIL - module not found

**Step 3: Write Infra/Db/Tag.hs**

```haskell
module Infra.Db.Tag
  ( insertTag
  , getOrCreateTag
  , getTagByName
  , getAllTags
  , getTagsByActivity
  , addTagToActivity
  , removeTagFromActivity
  , setActivityTags
  , searchTags
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Domain.Id (EntityId(..), newEntityId)
import Domain.Tag
import App.Monad (App, getConn)

instance FromRow Tag where
  fromRow = Tag <$> (TagId <$> field) <*> field

-- Insert a new tag (returns Nothing if already exists)
insertTag :: Text -> App (Maybe Tag)
insertTag name = do
  conn <- getConn
  let normalized = normalizeTagName name
  tagId <- liftIO $ unEntityId <$> newEntityId
  results <- liftIO $ query conn
    "INSERT INTO tags (id, name) VALUES (?, ?) \
    \ON CONFLICT (name) DO NOTHING \
    \RETURNING id, name"
    (tagId, normalized)
  pure $ case results of
    [tag] -> Just tag
    _ -> Nothing

-- Get or create a tag by name
getOrCreateTag :: Text -> App Tag
getOrCreateTag name = do
  conn <- getConn
  let normalized = normalizeTagName name
  -- Try to get existing
  existing <- getTagByName normalized
  case existing of
    Just tag -> pure tag
    Nothing -> do
      -- Create new
      tagId <- liftIO $ unEntityId <$> newEntityId
      _ <- liftIO $ execute conn
        "INSERT INTO tags (id, name) VALUES (?, ?) ON CONFLICT DO NOTHING"
        (tagId, normalized)
      -- Fetch it (handles race condition)
      result <- getTagByName normalized
      case result of
        Just tag -> pure tag
        Nothing -> error "Failed to create tag"  -- Should never happen

-- Get tag by name
getTagByName :: Text -> App (Maybe Tag)
getTagByName name = do
  conn <- getConn
  let normalized = normalizeTagName name
  results <- liftIO $ query conn
    "SELECT id, name FROM tags WHERE name = ?"
    (Only normalized)
  pure $ case results of
    [tag] -> Just tag
    _ -> Nothing

-- Get all tags (for suggestion)
getAllTags :: App [Tag]
getAllTags = do
  conn <- getConn
  liftIO $ query_ conn "SELECT id, name FROM tags ORDER BY name"

-- Get tags for an activity
getTagsByActivity :: EntityId -> App [Tag]
getTagsByActivity activityId = do
  conn <- getConn
  liftIO $ query conn
    "SELECT t.id, t.name FROM tags t \
    \JOIN activity_tags at ON t.id = at.tag_id \
    \WHERE at.activity_id = ? \
    \ORDER BY t.name"
    (Only $ unEntityId activityId)

-- Add a tag to an activity
addTagToActivity :: EntityId -> TagId -> App ()
addTagToActivity activityId tagId = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "INSERT INTO activity_tags (activity_id, tag_id) VALUES (?, ?) \
    \ON CONFLICT DO NOTHING"
    (unEntityId activityId, unTagId tagId)
  pure ()

-- Remove a tag from an activity
removeTagFromActivity :: EntityId -> TagId -> App ()
removeTagFromActivity activityId tagId = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "DELETE FROM activity_tags WHERE activity_id = ? AND tag_id = ?"
    (unEntityId activityId, unTagId tagId)
  pure ()

-- Set all tags for an activity (replaces existing)
setActivityTags :: EntityId -> [Text] -> App [Tag]
setActivityTags activityId tagNames = do
  conn <- getConn
  -- Remove existing tags
  _ <- liftIO $ execute conn
    "DELETE FROM activity_tags WHERE activity_id = ?"
    (Only $ unEntityId activityId)
  -- Add new tags
  tags <- mapM getOrCreateTag tagNames
  mapM_ (\t -> addTagToActivity activityId (tagId t)) tags
  pure tags

-- Search tags by prefix (for autocomplete)
searchTags :: Text -> Int -> App [Tag]
searchTags prefix limit = do
  conn <- getConn
  let pattern = normalizeTagName prefix <> "%"
  liftIO $ query conn
    "SELECT id, name FROM tags WHERE name LIKE ? ORDER BY name LIMIT ?"
    (pattern, limit)
```

**Step 4: Add to cabal file**

Add `Infra.Db.Tag` and `Infra.Db.TagSpec` to `wisp-srv.cabal`.

**Step 5: Run test to verify it passes**

Run: `cd wisp-srv && cabal test --test-option=--match="Infra.Db.Tag"`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Infra/Db/Tag.hs wisp-srv/test/Infra/Db/TagSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(db): add Tag database operations"
```

---

### Task 4: Add Note Source to Activity

**Files:**
- Modify: `wisp-srv/src/Domain/Activity.hs`
- Modify: `wisp-srv/src/Infra/Db/Activity.hs`
- Modify: `wisp-srv/test/Domain/ActivitySpec.hs`

**Step 1: Write the failing test**

Add to `Domain/ActivitySpec.hs`:

```haskell
  describe "ActivitySource" $ do
    it "includes Note as a valid source" $ do
      toJSON Note `shouldBe` "note"

    it "parses note from JSON" $ do
      decode "\"note\"" `shouldBe` Just Note
```

**Step 2: Run test to verify it fails**

Run: `cd wisp-srv && cabal test --test-option=--match="ActivitySource"`
Expected: FAIL - Note not in scope

**Step 3: Add Note to ActivitySource**

In `Domain/Activity.hs`, update:

```haskell
data ActivitySource = Email | Calendar | Conversation | Note
  deriving (Eq, Show, Generic)

instance ToJSON ActivitySource where
  toJSON Email = "email"
  toJSON Calendar = "calendar"
  toJSON Conversation = "conversation"
  toJSON Note = "note"

instance FromJSON ActivitySource where
  parseJSON = withText "ActivitySource" $ \case
    "email" -> pure Email
    "calendar" -> pure Calendar
    "conversation" -> pure Conversation
    "note" -> pure Note
    _ -> fail "Invalid activity source"
```

**Step 4: Update Infra/Db/Activity.hs**

Update `parseSource` and `srcText` mappings to include "note" -> Note.

**Step 5: Run test to verify it passes**

Run: `cd wisp-srv && cabal test --test-option=--match="ActivitySource"`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Domain/Activity.hs wisp-srv/src/Infra/Db/Activity.hs wisp-srv/test/Domain/ActivitySpec.hs
git commit -m "feat(domain): add Note as ActivitySource"
```

---

### Task 5: Note Insertion Function

**Files:**
- Modify: `wisp-srv/src/Infra/Db/Activity.hs`
- Modify: `wisp-srv/test/Infra/Db/ActivitySpec.hs`

**Step 1: Write the failing test**

Add to `Infra/Db/ActivitySpec.hs`:

```haskell
  describe "insertNote" $ do
    it "creates a note activity with tags" $ \env -> runTestApp env $ do
      -- Need an account first
      let accountId = EntityId "test-account"
      mActivityId <- insertNote accountId "Alice works at Google" ["alice", "work"] noteOrigin
      case mActivityId of
        Just aid -> do
          mActivity <- getActivity aid
          case mActivity of
            Just activity -> do
              liftIO $ activitySource activity `shouldBe` Note
              liftIO $ activityTitle activity `shouldBe` Just "Alice works at Google"
            Nothing -> liftIO $ expectationFailure "Activity not found"
        Nothing -> liftIO $ expectationFailure "Note not created"
      where
        noteOrigin = object ["origin" .= ("test" :: Text)]
```

**Step 2: Run test to verify it fails**

Run: `cd wisp-srv && cabal test --test-option=--match="insertNote"`
Expected: FAIL - insertNote not in scope

**Step 3: Add insertNote function**

In `Infra/Db/Activity.hs`, add:

```haskell
import Data.Aeson (Value, object, (.=))
import Infra.Db.Tag (setActivityTags)

-- Insert a note (returns activity ID)
insertNote :: EntityId -> Text -> [Text] -> Value -> App (Maybe EntityId)
insertNote accountId content tagNames rawMeta = do
  conn <- getConn
  aid <- liftIO newEntityId
  n <- liftIO $ execute conn
    "INSERT INTO activities \
    \(id, account_id, source, source_id, raw, title, status) \
    \VALUES (?, ?, 'note', ?, ?, ?, 'pending')"
    ( unEntityId aid
    , unEntityId accountId
    , "note-" <> unEntityId aid
    , rawMeta
    , content
    )
  if n > 0
    then do
      _ <- setActivityTags aid tagNames
      pure $ Just aid
    else pure Nothing
```

Also export `insertNote` from the module.

**Step 4: Run test to verify it passes**

Run: `cd wisp-srv && cabal test --test-option=--match="insertNote"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Db/Activity.hs wisp-srv/test/Infra/Db/ActivitySpec.hs
git commit -m "feat(db): add insertNote for creating note activities"
```

---

## Phase 2: Memory (Sessions, Summaries, Souls)

### Task 6: Database Migration for Memory Tables

**Files:**
- Create: `wisp-srv/migrations/013_memory.sql`

**Step 1: Write the migration**

```sql
-- 013_memory.sql
-- Agent memory: sessions, summaries, souls

CREATE TABLE sessions (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  agent_id TEXT NOT NULL,
  messages JSONB NOT NULL DEFAULT '[]',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  ended_at TIMESTAMPTZ,
  summarized BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE TABLE summaries (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  agent_id TEXT NOT NULL,
  session_ids UUID[] NOT NULL,
  content TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE souls (
  agent_id TEXT PRIMARY KEY,
  personality TEXT NOT NULL DEFAULT '',
  insights JSONB NOT NULL DEFAULT '[]',
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_sessions_agent ON sessions(agent_id);
CREATE INDEX idx_sessions_created ON sessions(created_at DESC);
CREATE INDEX idx_sessions_not_summarized ON sessions(agent_id) WHERE NOT summarized;
CREATE INDEX idx_summaries_agent ON summaries(agent_id);
```

**Step 2: Verify migration file exists**

Run: `ls -la wisp-srv/migrations/013_memory.sql`
Expected: File exists

**Step 3: Commit**

```bash
git add wisp-srv/migrations/013_memory.sql
git commit -m "feat(db): add sessions, summaries, souls tables"
```

---

### Task 7: Session Domain Model

**Files:**
- Create: `wisp-srv/src/Domain/Session.hs`
- Create: `wisp-srv/test/Domain/SessionSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

```haskell
module Domain.SessionSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode)
import Domain.Session
import Domain.Chat (ChatMessage(..))

spec :: Spec
spec = describe "Session" $ do
  describe "JSON serialization" $ do
    it "serializes Session to JSON" $ do
      let session = Session
            { sessionId = SessionId "sess-123"
            , sessionAgentId = "wisp/concierge"
            , sessionMessages = []
            , sessionCreatedAt = read "2026-02-18 10:00:00 UTC"
            , sessionEndedAt = Nothing
            , sessionSummarized = False
            }
      encode session `shouldContain` "\"agent_id\":\"wisp/concierge\""
```

**Step 2: Run test to verify it fails**

Run: `cd wisp-srv && cabal test --test-option=--match="Session"`
Expected: FAIL - module not found

**Step 3: Write Domain/Session.hs**

```haskell
module Domain.Session
  ( Session(..)
  , SessionId(..)
  , NewSession(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), (.:?))
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Chat (ChatMessage)
import GHC.Generics (Generic)

newtype SessionId = SessionId { unSessionId :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

data Session = Session
  { sessionId :: SessionId
  , sessionAgentId :: Text
  , sessionMessages :: [ChatMessage]
  , sessionCreatedAt :: UTCTime
  , sessionEndedAt :: Maybe UTCTime
  , sessionSummarized :: Bool
  } deriving (Eq, Show)

instance ToJSON Session where
  toJSON s = object
    [ "id" .= sessionId s
    , "agent_id" .= sessionAgentId s
    , "messages" .= sessionMessages s
    , "created_at" .= sessionCreatedAt s
    , "ended_at" .= sessionEndedAt s
    , "summarized" .= sessionSummarized s
    ]

instance FromJSON Session where
  parseJSON = withObject "Session" $ \v -> Session
    <$> v .: "id"
    <*> v .: "agent_id"
    <*> v .: "messages"
    <*> v .: "created_at"
    <*> v .:? "ended_at"
    <*> v .: "summarized"

data NewSession = NewSession
  { newSessionAgentId :: Text
  } deriving (Eq, Show)
```

**Step 4: Add to cabal and run tests**

Run: `cd wisp-srv && cabal test --test-option=--match="Session"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Domain/Session.hs wisp-srv/test/Domain/SessionSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(domain): add Session model"
```

---

### Task 8: Summary Domain Model

**Files:**
- Create: `wisp-srv/src/Domain/Summary.hs`
- Create: `wisp-srv/test/Domain/SummarySpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

```haskell
module Domain.SummarySpec (spec) where

import Test.Hspec
import Data.Aeson (encode)
import Domain.Summary
import Domain.Session (SessionId(..))

spec :: Spec
spec = describe "Summary" $ do
  it "serializes Summary to JSON" $ do
    let summary = Summary
          { summaryId = SummaryId "sum-123"
          , summaryAgentId = "wisp/scheduler"
          , summarySessionIds = [SessionId "sess-1", SessionId "sess-2"]
          , summaryContent = "Discussed scheduling preferences"
          , summaryCreatedAt = read "2026-02-18 12:00:00 UTC"
          }
    encode summary `shouldContain` "\"content\":\"Discussed scheduling preferences\""
```

**Step 2: Run test to verify it fails**

Run: `cd wisp-srv && cabal test --test-option=--match="Summary"`
Expected: FAIL

**Step 3: Write Domain/Summary.hs**

```haskell
module Domain.Summary
  ( Summary(..)
  , SummaryId(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:))
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Session (SessionId)
import GHC.Generics (Generic)

newtype SummaryId = SummaryId { unSummaryId :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

data Summary = Summary
  { summaryId :: SummaryId
  , summaryAgentId :: Text
  , summarySessionIds :: [SessionId]
  , summaryContent :: Text
  , summaryCreatedAt :: UTCTime
  } deriving (Eq, Show)

instance ToJSON Summary where
  toJSON s = object
    [ "id" .= summaryId s
    , "agent_id" .= summaryAgentId s
    , "session_ids" .= summarySessionIds s
    , "content" .= summaryContent s
    , "created_at" .= summaryCreatedAt s
    ]

instance FromJSON Summary where
  parseJSON = withObject "Summary" $ \v -> Summary
    <$> v .: "id"
    <*> v .: "agent_id"
    <*> v .: "session_ids"
    <*> v .: "content"
    <*> v .: "created_at"
```

**Step 4: Add to cabal and run tests**

Run: `cd wisp-srv && cabal test --test-option=--match="Summary"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Domain/Summary.hs wisp-srv/test/Domain/SummarySpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(domain): add Summary model"
```

---

### Task 9: Soul Domain Model

**Files:**
- Create: `wisp-srv/src/Domain/Soul.hs`
- Create: `wisp-srv/test/Domain/SoulSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

```haskell
module Domain.SoulSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode)
import Domain.Soul

spec :: Spec
spec = describe "Soul" $ do
  it "serializes Soul to JSON" $ do
    let soul = Soul
          { soulAgentId = "wisp/concierge"
          , soulPersonality = "Concise, uses bullet points"
          , soulInsights = ["Prefers morning meetings", "Dislikes preambles"]
          , soulUpdatedAt = read "2026-02-18 10:00:00 UTC"
          }
    encode soul `shouldContain` "\"personality\":\"Concise, uses bullet points\""
    encode soul `shouldContain` "\"Prefers morning meetings\""

  it "starts with empty soul" $ do
    let empty = emptySoul "wisp/scheduler"
    soulPersonality empty `shouldBe` ""
    soulInsights empty `shouldBe` []
```

**Step 2: Run test to verify it fails**

Run: `cd wisp-srv && cabal test --test-option=--match="Soul"`
Expected: FAIL

**Step 3: Write Domain/Soul.hs**

```haskell
module Domain.Soul
  ( Soul(..)
  , emptySoul
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:))
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)

data Soul = Soul
  { soulAgentId :: Text
  , soulPersonality :: Text
  , soulInsights :: [Text]
  , soulUpdatedAt :: UTCTime
  } deriving (Eq, Show)

instance ToJSON Soul where
  toJSON s = object
    [ "agent_id" .= soulAgentId s
    , "personality" .= soulPersonality s
    , "insights" .= soulInsights s
    , "updated_at" .= soulUpdatedAt s
    ]

instance FromJSON Soul where
  parseJSON = withObject "Soul" $ \v -> Soul
    <$> v .: "agent_id"
    <*> v .: "personality"
    <*> v .: "insights"
    <*> v .: "updated_at"

-- Create an empty soul for an agent
emptySoul :: Text -> Soul
emptySoul agentId = Soul
  { soulAgentId = agentId
  , soulPersonality = ""
  , soulInsights = []
  , soulUpdatedAt = read "1970-01-01 00:00:00 UTC"  -- Will be set properly on insert
  }
```

**Step 4: Add to cabal and run tests**

Run: `cd wisp-srv && cabal test --test-option=--match="Soul"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Domain/Soul.hs wisp-srv/test/Domain/SoulSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(domain): add Soul model"
```

---

### Task 10: Session Database Layer

**Files:**
- Create: `wisp-srv/src/Infra/Db/Session.hs`
- Create: `wisp-srv/test/Infra/Db/SessionSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

```haskell
module Infra.Db.SessionSpec (spec) where

import Test.Hspec
import Domain.Session (Session(..), SessionId(..))
import Domain.Chat (ChatMessage(..))
import Infra.Db.Session
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = describe "Infra.Db.Session" $ around withTestEnv $ do
  describe "createSession" $ do
    it "creates a new session for an agent" $ \env -> runTestApp env $ do
      session <- createSession "wisp/concierge"
      liftIO $ sessionAgentId session `shouldBe` "wisp/concierge"
      liftIO $ sessionMessages session `shouldBe` []
      liftIO $ sessionSummarized session `shouldBe` False

  describe "appendMessage" $ do
    it "adds a message to a session" $ \env -> runTestApp env $ do
      session <- createSession "wisp/concierge"
      let msg = ChatMessage "user" "Hello" Nothing Nothing
      updated <- appendMessage (sessionId session) msg
      liftIO $ length (sessionMessages updated) `shouldBe` 1

  describe "getActiveSession" $ do
    it "returns active session for agent" $ \env -> runTestApp env $ do
      _ <- createSession "wisp/concierge"
      mSession <- getActiveSession "wisp/concierge"
      liftIO $ mSession `shouldSatisfy` isJust

  describe "endSession" $ do
    it "marks session as ended" $ \env -> runTestApp env $ do
      session <- createSession "wisp/concierge"
      endSession (sessionId session)
      mSession <- getSession (sessionId session)
      liftIO $ case mSession of
        Just s -> sessionEndedAt s `shouldSatisfy` isJust
        Nothing -> expectationFailure "Session not found"
```

**Step 2: Run test to verify it fails**

Run: `cd wisp-srv && cabal test --test-option=--match="Infra.Db.Session"`
Expected: FAIL

**Step 3: Write Infra/Db/Session.hs**

```haskell
module Infra.Db.Session
  ( createSession
  , getSession
  , getActiveSession
  , getRecentSessions
  , appendMessage
  , endSession
  , markSummarized
  , getUnsummarizedSessions
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, decode, Value)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Domain.Id (newEntityId, unEntityId)
import Domain.Session
import Domain.Chat (ChatMessage)
import App.Monad (App, getConn)

instance FromRow Session where
  fromRow = Session
    <$> (SessionId <$> field)
    <*> field
    <*> (parseMessages <$> field)
    <*> field
    <*> field
    <*> field
    where
      parseMessages :: Value -> [ChatMessage]
      parseMessages v = case decode (encode v) of
        Just msgs -> msgs
        Nothing -> []

-- Create a new session
createSession :: Text -> App Session
createSession agentId = do
  conn <- getConn
  sid <- liftIO $ unEntityId <$> newEntityId
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "INSERT INTO sessions (id, agent_id, messages, created_at) VALUES (?, ?, '[]', ?)"
    (sid, agentId, now)
  pure Session
    { sessionId = SessionId sid
    , sessionAgentId = agentId
    , sessionMessages = []
    , sessionCreatedAt = now
    , sessionEndedAt = Nothing
    , sessionSummarized = False
    }

-- Get session by ID
getSession :: SessionId -> App (Maybe Session)
getSession sid = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT id, agent_id, messages, created_at, ended_at, summarized \
    \FROM sessions WHERE id = ?"
    (Only $ unSessionId sid)
  pure $ case results of
    [s] -> Just s
    _ -> Nothing

-- Get active (not ended) session for agent
getActiveSession :: Text -> App (Maybe Session)
getActiveSession agentId = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT id, agent_id, messages, created_at, ended_at, summarized \
    \FROM sessions WHERE agent_id = ? AND ended_at IS NULL \
    \ORDER BY created_at DESC LIMIT 1"
    (Only agentId)
  pure $ case results of
    [s] -> Just s
    _ -> Nothing

-- Get recent sessions for agent
getRecentSessions :: Text -> Int -> App [Session]
getRecentSessions agentId limit = do
  conn <- getConn
  liftIO $ query conn
    "SELECT id, agent_id, messages, created_at, ended_at, summarized \
    \FROM sessions WHERE agent_id = ? \
    \ORDER BY created_at DESC LIMIT ?"
    (agentId, limit)

-- Append a message to session
appendMessage :: SessionId -> ChatMessage -> App Session
appendMessage sid msg = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "UPDATE sessions SET messages = messages || ?::jsonb WHERE id = ?"
    (encode [msg], unSessionId sid)
  mSession <- getSession sid
  case mSession of
    Just s -> pure s
    Nothing -> error "Session not found after append"

-- End a session
endSession :: SessionId -> App ()
endSession sid = do
  conn <- getConn
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "UPDATE sessions SET ended_at = ? WHERE id = ?"
    (now, unSessionId sid)
  pure ()

-- Mark session as summarized
markSummarized :: SessionId -> App ()
markSummarized sid = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "UPDATE sessions SET summarized = TRUE WHERE id = ?"
    (Only $ unSessionId sid)
  pure ()

-- Get sessions that need summarization
getUnsummarizedSessions :: Text -> App [Session]
getUnsummarizedSessions agentId = do
  conn <- getConn
  liftIO $ query conn
    "SELECT id, agent_id, messages, created_at, ended_at, summarized \
    \FROM sessions \
    \WHERE agent_id = ? AND ended_at IS NOT NULL AND NOT summarized \
    \ORDER BY created_at"
    (Only agentId)
```

**Step 4: Add to cabal and run tests**

Run: `cd wisp-srv && cabal test --test-option=--match="Infra.Db.Session"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Db/Session.hs wisp-srv/test/Infra/Db/SessionSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(db): add Session database operations"
```

---

### Task 11: Soul Database Layer

**Files:**
- Create: `wisp-srv/src/Infra/Db/Soul.hs`
- Create: `wisp-srv/test/Infra/Db/SoulSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

```haskell
module Infra.Db.SoulSpec (spec) where

import Test.Hspec
import Domain.Soul (Soul(..))
import Infra.Db.Soul
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = describe "Infra.Db.Soul" $ around withTestEnv $ do
  describe "getOrCreateSoul" $ do
    it "creates empty soul if none exists" $ \env -> runTestApp env $ do
      soul <- getOrCreateSoul "wisp/concierge"
      liftIO $ soulAgentId soul `shouldBe` "wisp/concierge"
      liftIO $ soulPersonality soul `shouldBe` ""
      liftIO $ soulInsights soul `shouldBe` []

    it "returns existing soul" $ \env -> runTestApp env $ do
      _ <- getOrCreateSoul "wisp/concierge"
      _ <- updateSoulPersonality "wisp/concierge" "Formal tone"
      soul <- getOrCreateSoul "wisp/concierge"
      liftIO $ soulPersonality soul `shouldBe` "Formal tone"

  describe "addInsight" $ do
    it "appends insight to soul" $ \env -> runTestApp env $ do
      _ <- getOrCreateSoul "wisp/scheduler"
      addInsight "wisp/scheduler" "Prefers mornings"
      soul <- getOrCreateSoul "wisp/scheduler"
      liftIO $ soulInsights soul `shouldContain` ["Prefers mornings"]
```

**Step 2: Run test to verify it fails**

Run: `cd wisp-srv && cabal test --test-option=--match="Infra.Db.Soul"`
Expected: FAIL

**Step 3: Write Infra/Db/Soul.hs**

```haskell
module Infra.Db.Soul
  ( getSoul
  , getOrCreateSoul
  , updateSoulPersonality
  , addInsight
  , removeInsight
  , updateSoul
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, decode, Value)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Domain.Soul
import App.Monad (App, getConn)

instance FromRow Soul where
  fromRow = Soul
    <$> field
    <*> field
    <*> (parseInsights <$> field)
    <*> field
    where
      parseInsights :: Value -> [Text]
      parseInsights v = case decode (encode v) of
        Just insights -> insights
        Nothing -> []

-- Get soul for agent
getSoul :: Text -> App (Maybe Soul)
getSoul agentId = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT agent_id, personality, insights, updated_at FROM souls WHERE agent_id = ?"
    (Only agentId)
  pure $ case results of
    [s] -> Just s
    _ -> Nothing

-- Get or create soul for agent
getOrCreateSoul :: Text -> App Soul
getOrCreateSoul agentId = do
  existing <- getSoul agentId
  case existing of
    Just soul -> pure soul
    Nothing -> do
      conn <- getConn
      now <- liftIO getCurrentTime
      _ <- liftIO $ execute conn
        "INSERT INTO souls (agent_id, personality, insights, updated_at) \
        \VALUES (?, '', '[]', ?) ON CONFLICT DO NOTHING"
        (agentId, now)
      mSoul <- getSoul agentId
      case mSoul of
        Just soul -> pure soul
        Nothing -> pure $ emptySoul agentId

-- Update soul personality
updateSoulPersonality :: Text -> Text -> App ()
updateSoulPersonality agentId personality = do
  conn <- getConn
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "UPDATE souls SET personality = ?, updated_at = ? WHERE agent_id = ?"
    (personality, now, agentId)
  pure ()

-- Add an insight
addInsight :: Text -> Text -> App ()
addInsight agentId insight = do
  conn <- getConn
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "UPDATE souls SET insights = insights || ?::jsonb, updated_at = ? WHERE agent_id = ?"
    (encode [insight], now, agentId)
  pure ()

-- Remove an insight
removeInsight :: Text -> Text -> App ()
removeInsight agentId insight = do
  conn <- getConn
  now <- liftIO getCurrentTime
  -- Remove by filtering the array
  _ <- liftIO $ execute conn
    "UPDATE souls SET \
    \  insights = (SELECT jsonb_agg(elem) FROM jsonb_array_elements(insights) elem WHERE elem::text != ?::text), \
    \  updated_at = ? \
    \WHERE agent_id = ?"
    (encode insight, now, agentId)
  pure ()

-- Full soul update
updateSoul :: Soul -> App ()
updateSoul soul = do
  conn <- getConn
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "INSERT INTO souls (agent_id, personality, insights, updated_at) \
    \VALUES (?, ?, ?, ?) \
    \ON CONFLICT (agent_id) DO UPDATE SET \
    \  personality = EXCLUDED.personality, \
    \  insights = EXCLUDED.insights, \
    \  updated_at = EXCLUDED.updated_at"
    (soulAgentId soul, soulPersonality soul, encode (soulInsights soul), now)
  pure ()
```

**Step 4: Add to cabal and run tests**

Run: `cd wisp-srv && cabal test --test-option=--match="Infra.Db.Soul"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Db/Soul.hs wisp-srv/test/Infra/Db/SoulSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(db): add Soul database operations"
```

---

### Task 12: Summary Database Layer

**Files:**
- Create: `wisp-srv/src/Infra/Db/Summary.hs`
- Create: `wisp-srv/test/Infra/Db/SummarySpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

```haskell
module Infra.Db.SummarySpec (spec) where

import Test.Hspec
import Domain.Summary (Summary(..), SummaryId(..))
import Domain.Session (SessionId(..))
import Infra.Db.Summary
import Infra.Db.Session (createSession, endSession)
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = describe "Infra.Db.Summary" $ around withTestEnv $ do
  describe "insertSummary" $ do
    it "creates a summary for sessions" $ \env -> runTestApp env $ do
      session <- createSession "wisp/concierge"
      endSession (sessionId session)
      summary <- insertSummary "wisp/concierge" [sessionId session] "Discussed inbox management"
      liftIO $ summaryAgentId summary `shouldBe` "wisp/concierge"
      liftIO $ summaryContent summary `shouldBe` "Discussed inbox management"

  describe "getRecentSummaries" $ do
    it "returns summaries for agent" $ \env -> runTestApp env $ do
      session <- createSession "wisp/scheduler"
      endSession (sessionId session)
      _ <- insertSummary "wisp/scheduler" [sessionId session] "Calendar discussion"
      summaries <- getRecentSummaries "wisp/scheduler" 10
      liftIO $ length summaries `shouldBe` 1
```

**Step 2: Run test to verify it fails**

Run: `cd wisp-srv && cabal test --test-option=--match="Infra.Db.Summary"`
Expected: FAIL

**Step 3: Write Infra/Db/Summary.hs**

```haskell
module Infra.Db.Summary
  ( insertSummary
  , getSummary
  , getRecentSummaries
  , getSummariesForSessions
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Domain.Id (newEntityId, unEntityId)
import Domain.Summary
import Domain.Session (SessionId(..))
import App.Monad (App, getConn)

instance FromRow Summary where
  fromRow = Summary
    <$> (SummaryId <$> field)
    <*> field
    <*> (map SessionId . fromPGArray <$> field)
    <*> field
    <*> field

-- Insert a summary
insertSummary :: Text -> [SessionId] -> Text -> App Summary
insertSummary agentId sessionIds content = do
  conn <- getConn
  sid <- liftIO $ unEntityId <$> newEntityId
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "INSERT INTO summaries (id, agent_id, session_ids, content, created_at) \
    \VALUES (?, ?, ?, ?, ?)"
    (sid, agentId, PGArray (map unSessionId sessionIds), content, now)
  pure Summary
    { summaryId = SummaryId sid
    , summaryAgentId = agentId
    , summarySessionIds = sessionIds
    , summaryContent = content
    , summaryCreatedAt = now
    }

-- Get summary by ID
getSummary :: SummaryId -> App (Maybe Summary)
getSummary sid = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT id, agent_id, session_ids, content, created_at \
    \FROM summaries WHERE id = ?"
    (Only $ unSummaryId sid)
  pure $ case results of
    [s] -> Just s
    _ -> Nothing

-- Get recent summaries for agent
getRecentSummaries :: Text -> Int -> App [Summary]
getRecentSummaries agentId limit = do
  conn <- getConn
  liftIO $ query conn
    "SELECT id, agent_id, session_ids, content, created_at \
    \FROM summaries WHERE agent_id = ? \
    \ORDER BY created_at DESC LIMIT ?"
    (agentId, limit)

-- Get summaries that cover specific sessions
getSummariesForSessions :: [SessionId] -> App [Summary]
getSummariesForSessions sessionIds = do
  conn <- getConn
  liftIO $ query conn
    "SELECT id, agent_id, session_ids, content, created_at \
    \FROM summaries WHERE session_ids && ? \
    \ORDER BY created_at DESC"
    (Only $ PGArray (map unSessionId sessionIds))
```

**Step 4: Add to cabal and run tests**

Run: `cd wisp-srv && cabal test --test-option=--match="Infra.Db.Summary"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Db/Summary.hs wisp-srv/test/Infra/Db/SummarySpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(db): add Summary database operations"
```

---

## Phase 3: Integration

### Task 13: Update ChatContext to Include Soul

**Files:**
- Modify: `wisp-srv/src/Domain/Chat.hs`
- Modify: `wisp-srv/test/Domain/ChatSpec.hs`

**Step 1: Update ChatContext**

Add soul and summaries to ChatContext:

```haskell
import Domain.Soul (Soul)
import Domain.Summary (Summary)

data ChatContext = ChatContext
  { contextCalendarEvents :: [Activity]
  , contextRecentActivities :: [Activity]
  , contextPendingEmails :: [Activity]
  , contextQuarantined :: [Activity]
  , contextSurfaced :: [Activity]
  , contextNeedsReview :: [Activity]
  , contextMentionedPeople :: [Person]
  , contextSoul :: Maybe Soul           -- NEW
  , contextSummaries :: [Summary]       -- NEW
  , contextRelevantNotes :: [Activity]  -- NEW: tag-matched knowledge
  } deriving (Show)
```

**Step 2: Update agents to use soul**

This will be done in subsequent tasks when updating agent context assembly.

**Step 3: Run tests**

Run: `cd wisp-srv && cabal test`
Expected: PASS (may need to update tests that construct ChatContext)

**Step 4: Commit**

```bash
git add wisp-srv/src/Domain/Chat.hs wisp-srv/test/Domain/ChatSpec.hs
git commit -m "feat(domain): add soul, summaries, and notes to ChatContext"
```

---

### Task 14: Knowledge Retrieval by Tags

**Files:**
- Modify: `wisp-srv/src/Infra/Db/Activity.hs`
- Add test to: `wisp-srv/test/Infra/Db/ActivitySpec.hs`

**Step 1: Write the failing test**

```haskell
  describe "getActivitiesByTags" $ do
    it "returns notes matching any of the given tags" $ \env -> runTestApp env $ do
      let accountId = EntityId "test-account"
      -- Create notes with different tags
      _ <- insertNote accountId "Alice info" ["alice", "family"] (object [])
      _ <- insertNote accountId "SuperIT info" ["superit", "work"] (object [])
      _ <- insertNote accountId "Alice at SuperIT" ["alice", "superit"] (object [])

      -- Query by alice tag
      aliceNotes <- getActivitiesByTags ["alice"] 10
      liftIO $ length aliceNotes `shouldBe` 2

      -- Query by superit tag
      superitNotes <- getActivitiesByTags ["superit"] 10
      liftIO $ length superitNotes `shouldBe` 2
```

**Step 2: Run test to verify it fails**

Run: `cd wisp-srv && cabal test --test-option=--match="getActivitiesByTags"`
Expected: FAIL

**Step 3: Add getActivitiesByTags function**

```haskell
-- Get activities (notes) by tags
getActivitiesByTags :: [Text] -> Int -> App [Activity]
getActivitiesByTags tagNames limit = do
  conn <- getConn
  let normalizedTags = map normalizeTagName tagNames
  results <- liftIO $ query conn
    "SELECT DISTINCT a.id, a.account_id, a.source, a.source_id, a.raw, a.status, \
    \a.title, a.summary, a.sender_email, a.starts_at, a.ends_at, a.created_at, \
    \a.personas, a.activity_type, a.urgency, a.autonomy_tier, a.confidence, a.person_id \
    \FROM activities a \
    \JOIN activity_tags at ON a.id = at.activity_id \
    \JOIN tags t ON at.tag_id = t.id \
    \WHERE t.name = ANY(?) AND a.source = 'note' \
    \ORDER BY a.created_at DESC \
    \LIMIT ?"
    (PGArray normalizedTags, limit)
  pure $ map unDbActivity results
```

**Step 4: Run test to verify it passes**

Run: `cd wisp-srv && cabal test --test-option=--match="getActivitiesByTags"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Db/Activity.hs wisp-srv/test/Infra/Db/ActivitySpec.hs
git commit -m "feat(db): add getActivitiesByTags for knowledge retrieval"
```

---

### Task 15: Add Note Tool to Concierge

**Files:**
- Modify: `wisp-srv/src/Agents/Concierge.hs`
- Modify: `wisp-srv/test/Agents/ConciergeSpec.hs`

**Step 1: Add add_note tool**

In the system prompt tools section, add:

```
- add_note: Save a piece of knowledge
  Parameters:
    - content: Text content of the note
    - tags: List of tags (will be normalized, suggested from existing)
```

**Step 2: Handle add_note in tool execution**

```haskell
executeAddNote :: EntityId -> Text -> [Text] -> App (Either Text Text)
executeAddNote accountId content tags = do
  let rawMeta = object
        [ "origin" .= ("chat" :: Text)
        , "created_by" .= ("user" :: Text)
        ]
  mActivityId <- insertNote accountId content tags rawMeta
  case mActivityId of
    Just aid -> pure $ Right $ "Note saved with ID: " <> unEntityId aid
    Nothing -> pure $ Left "Failed to save note"
```

**Step 3: Update tool response parsing to handle add_note**

**Step 4: Run tests**

Run: `cd wisp-srv && cabal test --test-option=--match="Concierge"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Agents/Concierge.hs wisp-srv/test/Agents/ConciergeSpec.hs
git commit -m "feat(concierge): add add_note tool for knowledge capture"
```

---

### Task 16: Inject Soul into Agent System Prompts

**Files:**
- Modify: `wisp-srv/src/Agents/Concierge.hs`
- Modify: `wisp-srv/src/Agents/Scheduler.hs`
- Modify: `wisp-srv/src/Agents/Insights.hs`

**Step 1: Update system prompt generation**

For each agent, fetch soul and inject into system prompt:

```haskell
buildSystemPrompt :: Soul -> Text
buildSystemPrompt soul = basePrompt <> soulSection
  where
    soulSection
      | T.null (soulPersonality soul) && null (soulInsights soul) = ""
      | otherwise = "\n\n## Your Personality & Insights\n\n" <>
          (if T.null (soulPersonality soul)
           then ""
           else "Personality: " <> soulPersonality soul <> "\n\n") <>
          (if null (soulInsights soul)
           then ""
           else "Insights about this user:\n" <>
                T.unlines (map ("- " <>) (soulInsights soul)))
```

**Step 2: Fetch soul in context assembly**

```haskell
handleChatWithContext :: ... -> App ...
handleChatWithContext ... = do
  soul <- getOrCreateSoul agentId
  -- Include soul in context assembly
  ...
```

**Step 3: Run tests**

Run: `cd wisp-srv && cabal test`
Expected: PASS

**Step 4: Commit**

```bash
git add wisp-srv/src/Agents/Concierge.hs wisp-srv/src/Agents/Scheduler.hs wisp-srv/src/Agents/Insights.hs
git commit -m "feat(agents): inject soul into system prompts"
```

---

## Phase 4: HTTP API

### Task 17: Tags API Endpoints

**Files:**
- Create: `wisp-srv/src/Http/Handlers/Tags.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create Tags handler**

```haskell
module Http.Handlers.Tags
  ( getTags
  , searchTagsHandler
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=))
import Web.Scotty.Trans (ActionT, json, queryParam)
import App.Monad (Env)
import Infra.Db.Tag (getAllTags, searchTags)

-- GET /tags
getTags :: ActionT (ReaderT Env IO) ()
getTags = do
  tags <- lift getAllTags
  json $ object ["tags" .= tags]

-- GET /tags/search?q=...
searchTagsHandler :: ActionT (ReaderT Env IO) ()
searchTagsHandler = do
  q <- queryParam "q"
  tags <- lift $ searchTags q 20
  json $ object ["tags" .= tags]
```

**Step 2: Add routes**

```haskell
get "/tags" getTags
get "/tags/search" searchTagsHandler
```

**Step 3: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Tags.hs wisp-srv/src/Http/Routes.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(http): add tags API endpoints"
```

---

### Task 18: Sessions API Endpoints

**Files:**
- Create: `wisp-srv/src/Http/Handlers/Sessions.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create Sessions handler**

```haskell
module Http.Handlers.Sessions
  ( getSessions
  , getSessionById
  , endSessionHandler
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Network.HTTP.Types.Status (status404)
import Web.Scotty.Trans (ActionT, json, status, pathParam, queryParam)
import App.Monad (Env)
import Domain.Session (SessionId(..))
import Infra.Db.Session

-- GET /sessions?agent=...
getSessions :: ActionT (ReaderT Env IO) ()
getSessions = do
  agentId <- queryParam "agent"
  sessions <- lift $ getRecentSessions agentId 20
  json $ object ["sessions" .= sessions]

-- GET /sessions/:id
getSessionById :: ActionT (ReaderT Env IO) ()
getSessionById = do
  sid <- pathParam "id"
  mSession <- lift $ getSession (SessionId sid)
  case mSession of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Session not found" :: Text)]
    Just session -> json session

-- POST /sessions/:id/end
endSessionHandler :: ActionT (ReaderT Env IO) ()
endSessionHandler = do
  sid <- pathParam "id"
  lift $ endSession (SessionId sid)
  json $ object ["status" .= ("ended" :: Text)]
```

**Step 2: Add routes**

```haskell
get "/sessions" getSessions
get "/sessions/:id" getSessionById
post "/sessions/:id/end" endSessionHandler
```

**Step 3: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Sessions.hs wisp-srv/src/Http/Routes.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(http): add sessions API endpoints"
```

---

### Task 19: Souls API Endpoints

**Files:**
- Create: `wisp-srv/src/Http/Handlers/Souls.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create Souls handler**

```haskell
module Http.Handlers.Souls
  ( getSoulHandler
  , updateSoulHandler
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=), FromJSON(..), withObject, (.:))
import Data.Text (Text)
import Web.Scotty.Trans (ActionT, json, pathParam, jsonData)
import App.Monad (Env)
import Domain.Soul (Soul(..))
import Infra.Db.Soul

data SoulUpdate = SoulUpdate
  { suPersonality :: Maybe Text
  , suAddInsights :: Maybe [Text]
  , suRemoveInsights :: Maybe [Text]
  }

instance FromJSON SoulUpdate where
  parseJSON = withObject "SoulUpdate" $ \v -> SoulUpdate
    <$> v .:? "personality"
    <*> v .:? "add_insights"
    <*> v .:? "remove_insights"

-- GET /souls/:agent_id
getSoulHandler :: ActionT (ReaderT Env IO) ()
getSoulHandler = do
  agentId <- pathParam "agent_id"
  soul <- lift $ getOrCreateSoul agentId
  json soul

-- PATCH /souls/:agent_id
updateSoulHandler :: ActionT (ReaderT Env IO) ()
updateSoulHandler = do
  agentId <- pathParam "agent_id"
  update <- jsonData
  -- Apply updates
  case suPersonality update of
    Just p -> lift $ updateSoulPersonality agentId p
    Nothing -> pure ()
  case suAddInsights update of
    Just insights -> mapM_ (lift . addInsight agentId) insights
    Nothing -> pure ()
  case suRemoveInsights update of
    Just insights -> mapM_ (lift . removeInsight agentId) insights
    Nothing -> pure ()
  -- Return updated soul
  soul <- lift $ getOrCreateSoul agentId
  json soul
```

**Step 2: Add routes**

```haskell
get "/souls/:agent_id" getSoulHandler
patch "/souls/:agent_id" updateSoulHandler
```

**Step 3: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Souls.hs wisp-srv/src/Http/Routes.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(http): add souls API endpoints"
```

---

### Task 20: Run All Tests and Final Verification

**Step 1: Run full test suite**

Run: `cd wisp-srv && cabal test`
Expected: All tests PASS

**Step 2: Build the project**

Run: `cd wisp-srv && cabal build`
Expected: Build succeeds

**Step 3: Verify migrations apply**

Run: Start the server and verify migrations run without error.

**Step 4: Final commit (if any cleanup needed)**

```bash
git status
# If clean, done. If changes needed:
git add -A
git commit -m "chore: final cleanup for knowledge-memory feature"
```

---

## Summary

| Phase | Tasks | What It Delivers |
|-------|-------|------------------|
| 1. Foundation | 1-5 | Tags table, Note activity source, tag CRUD |
| 2. Memory | 6-12 | Sessions, Summaries, Souls tables and domain models |
| 3. Integration | 13-16 | Context assembly with soul, knowledge retrieval, add_note tool |
| 4. HTTP API | 17-20 | REST endpoints for tags, sessions, souls |

**Total: 20 tasks**

After completing this plan, you'll have:
- Notes as tagged Activities flowing through classification
- Per-agent sessions with message accumulation
- Summaries for compressed session history
- Souls with personality and insights injected into prompts
- HTTP API for managing all new entities
