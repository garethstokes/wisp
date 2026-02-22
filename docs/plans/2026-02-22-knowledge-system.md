# Knowledge System Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Enable agents to capture, retrieve, and use knowledge (notes, projects, preferences) during conversations, with session summaries stored as documents.

**Architecture:** Knowledge lives in the existing `documents` table (types: note, project, preference). Agents access knowledge via context assembly before each chat. Session summaries are stored as documents (type: note) tagged with `session-summary` and the agent name. The "Note: ..." chat prefix triggers note capture.

**Tech Stack:** Haskell, PostgreSQL, existing App monad, documents table, sessions table

---

## Current State

| Component | Status |
|-----------|--------|
| `documents` table | ✅ Exists with project/note/preference types |
| `sessions` table | ✅ Exists with messages, ended_at, summarized |
| `souls` table | ✅ Exists with personality/insights |
| `Domain.Document` | ✅ Complete with NewDocument, DocumentType |
| `Infra.Db.Document` | ✅ Complete with CRUD operations |
| `Infra.Db.Session` | ✅ Complete with getUnsummarizedSessions |
| Knowledge in context | ❌ Not wired into agent prompts |
| Note capture from chat | ❌ Not implemented |
| Session summarization | ❌ Not implemented |

---

## Phase 1: Knowledge Context Assembly

### Task 1: Add getKnowledgeContext function

**Files:**
- Create: `wisp-srv/src/Services/Knowledge.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create the Knowledge service module**

Create `wisp-srv/src/Services/Knowledge.hs`:

```haskell
module Services.Knowledge
  ( getKnowledgeContext
  , KnowledgeContext(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.Document (Document(..), DocumentType(..), NoteData(..), ProjectData(..), PreferenceData(..))
import Domain.Tenant (TenantId)
import Infra.Db.Document (getDocumentsByType, getDocumentsByTags, getActiveDocuments)
import App.Monad (App)
import Data.Aeson (decode, encode)

-- | Knowledge context to inject into agent prompts
data KnowledgeContext = KnowledgeContext
  { kcProjects :: [Document]
  , kcRelevantNotes :: [Document]
  , kcPreferences :: [Document]
  } deriving (Show)

-- | Get knowledge context for an agent conversation
-- Takes optional tags to filter relevant notes
getKnowledgeContext :: TenantId -> [Text] -> App KnowledgeContext
getKnowledgeContext _tenantId mentionedTags = do
  -- Get all active projects
  projects <- getDocumentsByType ProjectDoc True 20

  -- Get preferences
  preferences <- getDocumentsByType PreferenceDoc True 50

  -- Get relevant notes based on mentioned tags
  relevantNotes <- if null mentionedTags
    then getDocumentsByType NoteDoc True 10  -- Recent notes if no tags
    else getDocumentsByTags mentionedTags True 20

  pure KnowledgeContext
    { kcProjects = projects
    , kcRelevantNotes = relevantNotes
    , kcPreferences = preferences
    }

-- | Format knowledge context as text for system prompt
formatKnowledgeContext :: KnowledgeContext -> Text
formatKnowledgeContext kc = T.unlines
  [ "## Knowledge"
  , ""
  , "### Projects"
  , if null (kcProjects kc)
      then "(no projects)"
      else T.unlines $ map formatProject (kcProjects kc)
  , ""
  , "### Preferences"
  , if null (kcPreferences kc)
      then "(no preferences)"
      else T.unlines $ map formatPreference (kcPreferences kc)
  , ""
  , "### Relevant Notes"
  , if null (kcRelevantNotes kc)
      then "(no relevant notes)"
      else T.unlines $ map formatNote (kcRelevantNotes kc)
  ]
  where
    formatProject doc = case decode (encode (documentData doc)) of
      Just (pd :: ProjectData) -> "- " <> projectName pd <> " [" <> T.intercalate ", " (documentTags doc) <> "]"
      Nothing -> "- (unknown project)"

    formatPreference doc = case decode (encode (documentData doc)) of
      Just (pd :: PreferenceData) -> "- " <> prefKey pd <> ": " <> prefValue pd
      Nothing -> "- (unknown preference)"

    formatNote doc = case decode (encode (documentData doc)) of
      Just (nd :: NoteData) -> "- " <> noteTitle nd <> " [" <> T.intercalate ", " (documentTags doc) <> "]"
      Nothing -> "- (unknown note)"
```

**Step 2: Add module to cabal file**

In `wisp-srv/wisp-srv.cabal`, add to the executable's `other-modules`:

```
        Services.Knowledge
```

**Step 3: Verify build**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Services/Knowledge.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(knowledge): add Knowledge service with context assembly"
```

---

### Task 2: Wire knowledge into agent context

**Files:**
- Modify: `wisp-srv/src/Agents/Core.hs`
- Modify: `wisp-srv/src/Agents/Dispatcher.hs`

**Step 1: Update buildSystemPrompt to accept knowledge context**

In `wisp-srv/src/Agents/Core.hs`, add import and update function:

```haskell
import Services.Knowledge (KnowledgeContext(..), formatKnowledgeContext)
```

Update `buildSystemPrompt` signature and implementation:

```haskell
-- | Build system prompt for an agent
-- Combines personality, soul insights, knowledge, and skill prompt
buildSystemPrompt :: Agent -> Maybe Text -> Maybe KnowledgeContext -> Text
buildSystemPrompt agent mSkillPrompt mKnowledge = T.unlines
  [ "You are " <> agentName agent <> ", a personal assistant."
  , ""
  , "## Your Personality"
  , if T.null (agentPersonalitySeed (agentConfig agent))
    then "Be helpful and concise."
    else agentPersonalitySeed (agentConfig agent)
  , ""
  , buildSoulSection (agentSoul agent)
  , ""
  , maybe "" formatKnowledgeContext mKnowledge
  , ""
  , "## Response Format"
  -- ... rest unchanged
  ]
```

**Step 2: Update Dispatcher to fetch knowledge**

In `wisp-srv/src/Agents/Dispatcher.hs`, add import:

```haskell
import Services.Knowledge (getKnowledgeContext)
```

In `dispatchChatStreaming`, after loading the agent, fetch knowledge:

```haskell
    Just agent -> withRunLogging agentName mSessionId msgs $ \ctx messages -> do
      -- Fetch knowledge context
      knowledgeCtx <- getKnowledgeContext tenantId []  -- TODO: extract tags from messages

      -- Load skill prompt if skill is active
      mSkillPrompt <- case agentSkill agent of
        Nothing -> pure Nothing
        Just skill -> loadSkillPromptByTenant tenantId (SkillsRegistry.skillName skill)

      -- Build the system prompt with knowledge
      let systemPrompt = buildSystemPrompt agent mSkillPrompt (Just knowledgeCtx)
```

**Step 3: Verify build**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Agents/Core.hs wisp-srv/src/Agents/Dispatcher.hs
git commit -m "feat(knowledge): wire knowledge context into agent prompts"
```

---

## Phase 2: Note Capture from Chat

### Task 3: Add note capture detection

**Files:**
- Modify: `wisp-srv/src/Services/Knowledge.hs`

**Step 1: Add note detection and capture functions**

Add to `wisp-srv/src/Services/Knowledge.hs`:

```haskell
import Data.Aeson (toJSON)
import Domain.Id (EntityId)
import Infra.Db.Document (insertDocument)

-- | Check if a message is a note command
-- Returns (isNote, noteContent) where noteContent strips the prefix
detectNoteCommand :: Text -> Maybe Text
detectNoteCommand msg
  | "note:" `T.isPrefixOf` T.toLower msg = Just $ T.strip $ T.drop 5 msg
  | "remember:" `T.isPrefixOf` T.toLower msg = Just $ T.strip $ T.drop 9 msg
  | otherwise = Nothing

-- | Capture a note from chat
captureNote :: TenantId -> Text -> Text -> Maybe Text -> App EntityId
captureNote tenantId title content mAgentId = do
  let noteData = NoteData
        { noteTitle = title
        , noteContent = if T.null content then Nothing else Just content
        }
  let newDoc = NewDocument
        { newDocTenantId = Just tenantId
        , newDocType = NoteDoc
        , newDocData = toJSON noteData
        , newDocTags = []  -- TODO: extract tags from content
        , newDocConfidence = Just 0.9
        , newDocSource = mAgentId
        , newDocSupersedesId = Nothing
        }
  insertDocument newDoc
```

**Step 2: Verify build**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Services/Knowledge.hs
git commit -m "feat(knowledge): add note detection and capture"
```

---

### Task 4: Integrate note capture into chat flow

**Files:**
- Modify: `wisp-srv/src/Agents/Dispatcher.hs`

**Step 1: Check for note commands before dispatching**

In `dispatchChatStreaming`, check the last user message for note commands:

```haskell
import Services.Knowledge (getKnowledgeContext, detectNoteCommand, captureNote)

-- At the start of dispatchChatStreaming, check for note command
dispatchChatStreaming tenantId accountId agentName msgs mTimezone mSessionId emit = do
  -- Check if last message is a note command
  let lastUserMsg = case filter isUserMessage msgs of
        [] -> Nothing
        ms -> Just $ messageContent $ last ms
      isUserMessage (ChatMessage "user" _ _) = True
      isUserMessage _ = False

  case lastUserMsg >>= detectNoteCommand of
    Just noteContent -> do
      -- Capture the note
      docId <- captureNote tenantId noteContent "" (Just agentName)
      pure $ Right $ ChatResponse
        { responseMessage = "Got it! I've noted: \"" <> noteContent <> "\""
        , responseToolCalls = []
        }
    Nothing -> do
      -- Normal chat dispatch
      -- ... existing code
```

**Step 2: Verify build**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 3: Test manually**

1. Start server: `cabal run wisp-srv`
2. Send chat: "Note: Alice works at Google"
3. Check documents: `psql -d wisp -c "SELECT * FROM documents WHERE type = 'note' ORDER BY created_at DESC LIMIT 1;"`

**Step 4: Commit**

```bash
git add wisp-srv/src/Agents/Dispatcher.hs
git commit -m "feat(knowledge): integrate note capture into chat flow"
```

---

## Phase 3: Session Summaries as Documents

### Task 5: Add session summarization service

**Files:**
- Create: `wisp-srv/src/Services/Summarizer.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create summarizer service**

Create `wisp-srv/src/Services/Summarizer.hs`:

```haskell
module Services.Summarizer
  ( summarizeSession
  , summarizeUnsummarizedSessions
  ) where

import Control.Monad (forM_)
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Session (Session(..), SessionId(..))
import Domain.Document (NewDocument(..), DocumentType(..), NoteData(..))
import Domain.Chat (ChatMessage(..))
import Infra.Db.Session (getUnsummarizedSessions, markSummarized)
import Infra.Db.Document (insertDocument)
import Infra.LLM.Claude (callClaude)
import App.Monad (App)
import App.Config (getConfig)

-- | Summarize a session and store as a document
summarizeSession :: Session -> App ()
summarizeSession session = do
  -- Build summary prompt
  let messagesText = T.unlines $ map formatMessage (sessionMessages session)
      prompt = T.unlines
        [ "Summarize this conversation in 2-3 sentences. Focus on:"
        , "- Key decisions made"
        , "- Information learned"
        , "- Tasks discussed"
        , ""
        , "Conversation:"
        , messagesText
        ]

  -- Call LLM for summary
  cfg <- getConfig
  result <- liftIO $ callClaude (cfg.claude.apiKey) (cfg.claude.model) prompt

  case result of
    Left _err -> pure ()  -- Skip on error, will retry later
    Right summaryText -> do
      -- Store as document
      let noteData = NoteData
            { noteTitle = "Session summary: " <> sessionAgentId session
            , noteContent = Just summaryText
            }
          tags = ["session-summary", "agent:" <> sessionAgentId session]
      _ <- insertDocument NewDocument
        { newDocTenantId = Nothing  -- TODO: get tenant from session
        , newDocType = NoteDoc
        , newDocData = toJSON noteData
        , newDocTags = tags
        , newDocConfidence = Just 0.8
        , newDocSource = Just "summarizer"
        , newDocSupersedesId = Nothing
        }
      -- Mark session as summarized
      markSummarized (sessionId session)
  where
    formatMessage msg = messageRole msg <> ": " <> messageContent msg

-- | Summarize all unsummarized sessions for an agent
summarizeUnsummarizedSessions :: Text -> App ()
summarizeUnsummarizedSessions agentId = do
  sessions <- getUnsummarizedSessions agentId
  forM_ sessions summarizeSession
```

**Step 2: Add to cabal**

Add `Services.Summarizer` to `other-modules` in cabal file.

**Step 3: Verify build**

Run: `cabal build wisp-srv`
Expected: Build succeeds (may need to add `liftIO` import)

**Step 4: Commit**

```bash
git add wisp-srv/src/Services/Summarizer.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(knowledge): add session summarization service"
```

---

### Task 6: Add summarization endpoint

**Files:**
- Modify: `wisp-srv/src/Http/Handlers/Activities.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`

**Step 1: Add handler**

In `wisp-srv/src/Http/Handlers/Activities.hs`:

```haskell
import Services.Summarizer (summarizeUnsummarizedSessions)

-- POST /admin/summarize-sessions/:agent
summarizeSessions :: ActionT (ReaderT Env IO) ()
summarizeSessions = do
  agentId <- captureParam "agent"
  lift $ summarizeUnsummarizedSessions agentId
  json $ object ["status" .= ("summarization triggered" :: Text)]
```

Add to exports.

**Step 2: Add route**

In `wisp-srv/src/Http/Routes.hs`:

```haskell
  post "/admin/summarize-sessions/:agent" summarizeSessions
```

**Step 3: Verify build**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Activities.hs wisp-srv/src/Http/Routes.hs
git commit -m "feat(knowledge): add session summarization endpoint"
```

---

## Phase 4: Include Summaries in Context

### Task 7: Include recent session summaries in knowledge context

**Files:**
- Modify: `wisp-srv/src/Services/Knowledge.hs`

**Step 1: Update KnowledgeContext and getKnowledgeContext**

```haskell
-- Update KnowledgeContext
data KnowledgeContext = KnowledgeContext
  { kcProjects :: [Document]
  , kcRelevantNotes :: [Document]
  , kcPreferences :: [Document]
  , kcSessionSummaries :: [Document]  -- Add this
  } deriving (Show)

-- Update getKnowledgeContext
getKnowledgeContext :: TenantId -> Text -> [Text] -> App KnowledgeContext
getKnowledgeContext _tenantId agentId mentionedTags = do
  projects <- getDocumentsByType ProjectDoc True 20
  preferences <- getDocumentsByType PreferenceDoc True 50
  relevantNotes <- if null mentionedTags
    then getDocumentsByType NoteDoc True 10
    else getDocumentsByTags mentionedTags True 20

  -- Get recent session summaries for this agent
  let summaryTags = ["session-summary", "agent:" <> agentId]
  summaries <- getDocumentsByTags summaryTags True 5

  pure KnowledgeContext
    { kcProjects = projects
    , kcRelevantNotes = relevantNotes
    , kcPreferences = preferences
    , kcSessionSummaries = summaries
    }

-- Update formatKnowledgeContext
formatKnowledgeContext :: KnowledgeContext -> Text
formatKnowledgeContext kc = T.unlines
  [ -- ... existing sections ...
  , ""
  , "### Recent Conversations"
  , if null (kcSessionSummaries kc)
      then "(no recent conversation history)"
      else T.unlines $ map formatSummary (kcSessionSummaries kc)
  ]
  where
    formatSummary doc = case decode (encode (documentData doc)) of
      Just (nd :: NoteData) -> maybe "(no summary)" id (noteContent nd)
      Nothing -> "(unknown summary)"
```

**Step 2: Update Dispatcher call**

Update `dispatchChatStreaming` to pass agent name:

```haskell
knowledgeCtx <- getKnowledgeContext tenantId agentName []
```

**Step 3: Verify build**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Services/Knowledge.hs wisp-srv/src/Agents/Dispatcher.hs
git commit -m "feat(knowledge): include session summaries in agent context"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Knowledge context service | `Services/Knowledge.hs` |
| 2 | Wire knowledge into agents | `Agents/Core.hs`, `Agents/Dispatcher.hs` |
| 3 | Note detection and capture | `Services/Knowledge.hs` |
| 4 | Integrate note capture | `Agents/Dispatcher.hs` |
| 5 | Session summarization | `Services/Summarizer.hs` |
| 6 | Summarization endpoint | `Http/Handlers/Activities.hs`, `Http/Routes.hs` |
| 7 | Summaries in context | `Services/Knowledge.hs` |

After completion:
- Agents see projects, notes, preferences in their system prompt
- Users can say "Note: ..." to capture knowledge
- Sessions can be summarized to documents
- Recent summaries appear in agent context
