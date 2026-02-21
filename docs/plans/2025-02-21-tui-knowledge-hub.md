# TUI Knowledge Hub Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restructure Documents view into three views: Knowledge (notes/prefs), Skills, and Agents (with memory/sessions).

**Architecture:** Add shared scrolling module, new API endpoints for soul/sessions, client types, then build three views following the Activities pattern (list + detail expansion).

**Tech Stack:** Haskell, Brick TUI, Scotty HTTP, PostgreSQL

---

## Task 1: Shared Scrolling Module

**Files:**
- Create: `wisp-tui/src/Tui/Widgets/Scroll.hs`
- Modify: `wisp-tui/wisp-tui.cabal`

**Step 1: Create the scroll module**

```haskell
module Tui.Widgets.Scroll
  ( handleViewportScroll
  , pageSize
  ) where

import Brick
import qualified Graphics.Vty as V

import Tui.Types (Name)

-- | Number of lines to scroll on page up/down
pageSize :: Int
pageSize = 15

-- | Shared scrolling handler for any viewport.
-- Returns True if the event was handled, False otherwise.
handleViewportScroll :: Name -> V.Event -> EventM Name s Bool
handleViewportScroll vpName = \case
  V.EvKey (V.KChar 'd') [V.MCtrl] -> do
    vScrollBy (viewportScroll vpName) pageSize
    pure True
  V.EvKey (V.KChar 'u') [V.MCtrl] -> do
    vScrollBy (viewportScroll vpName) (-pageSize)
    pure True
  V.EvKey (V.KChar 'j') [] -> do
    vScrollBy (viewportScroll vpName) 1
    pure True
  V.EvKey (V.KChar 'k') [] -> do
    vScrollBy (viewportScroll vpName) (-1)
    pure True
  _ -> pure False
```

**Step 2: Add to cabal file**

In `wisp-tui/wisp-tui.cabal`, add `Tui.Widgets.Scroll` to `other-modules`.

**Step 3: Build to verify**

Run: `cabal build wisp-tui`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add wisp-tui/src/Tui/Widgets/Scroll.hs wisp-tui/wisp-tui.cabal
git commit -m "feat(tui): add shared viewport scroll handler"
```

---

## Task 2: Server Endpoints for Soul and Sessions

**Files:**
- Modify: `wisp-srv/src/Http/Handlers/Agents.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`

**Step 1: Add getAgentSessions handler**

In `wisp-srv/src/Http/Handlers/Agents.hs`, add import and handler:

```haskell
-- Add to imports:
import Infra.Db.Session (getRecentSessions)
import Domain.Session (Session(..), SessionId(..))
import Data.Time (UTCTime)

-- Add to exports:
, getAgentSessions

-- Add handler:
-- | GET /api/agents/:name/sessions - get recent sessions for agent
getAgentSessions :: ActionT (ReaderT Env IO) ()
getAgentSessions = withTenant $ \tid -> do
  name <- captureParam "name"
  -- Use agent name as agent_id (matches how sessions are stored)
  sessions <- lift $ getRecentSessions name 10
  json $ object
    [ "agent" .= name
    , "sessions" .= map sessionToJson sessions
    , "count" .= length sessions
    ]

sessionToJson :: Session -> Value
sessionToJson s = object
  [ "id" .= unSessionId (sessionId s)
  , "message_count" .= length (sessionMessages s)
  , "created_at" .= sessionCreatedAt s
  , "ended_at" .= sessionEndedAt s
  , "summarized" .= sessionSummarized s
  ]
```

**Step 2: Add routes**

In `wisp-srv/src/Http/Routes.hs`:

```haskell
-- Add to imports:
import Http.Handlers.Agents (getAgentsList, getAgentByName, postAgentActivateSkill, postAgentDeactivate, getAgentSessions)

-- Add route after existing agent routes:
get "/api/agents/:name/sessions" getAgentSessions
```

**Step 3: Build to verify**

Run: `cabal build wisp-srv`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Agents.hs wisp-srv/src/Http/Routes.hs
git commit -m "feat(api): add GET /api/agents/:name/sessions endpoint"
```

---

## Task 3: Client Types for Skills, Agents, Sessions

**Files:**
- Modify: `wisp-core/src/Wisp/Client/Types.hs`

**Step 1: Add new types**

```haskell
-- Add to module exports:
, Skill(..)
, AgentInfo(..)
, AgentSoul(..)
, SessionSummary(..)

-- Add imports:
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- Add types:

-- | Skill info from GET /api/skills
data Skill = Skill
  { skillName :: Text
  , skillTools :: [Text]
  , skillAvailable :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON Skill where
  parseJSON = withObject "Skill" $ \v -> Skill
    <$> v .: "name"
    <*> v .: "tools"
    <*> v .: "available"

-- | Agent info from GET /api/agents/:name
data AgentInfo = AgentInfo
  { agentInfoName :: Text
  , agentInfoPersonality :: Text
  , agentInfoActiveSkill :: Maybe Text
  , agentInfoSoul :: AgentSoul
  , agentInfoAvailableSkills :: [Text]
  } deriving (Show, Eq, Generic)

instance FromJSON AgentInfo where
  parseJSON = withObject "AgentInfo" $ \v -> AgentInfo
    <$> v .: "name"
    <*> v .: "personality"
    <*> v .:? "active_skill"
    <*> v .: "soul"
    <*> v .: "available_skills"

-- | Agent soul (memory)
data AgentSoul = AgentSoul
  { soulPersonality :: Text
  , soulInsights :: [Text]
  } deriving (Show, Eq, Generic)

instance FromJSON AgentSoul where
  parseJSON = withObject "AgentSoul" $ \v -> AgentSoul
    <$> v .: "personality"
    <*> v .: "insights"

-- | Session summary from GET /api/agents/:name/sessions
data SessionSummary = SessionSummary
  { sessionSummaryId :: Text
  , sessionSummaryMessageCount :: Int
  , sessionSummaryCreatedAt :: UTCTime
  , sessionSummaryEndedAt :: Maybe UTCTime
  , sessionSummarySummarized :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON SessionSummary where
  parseJSON = withObject "SessionSummary" $ \v -> SessionSummary
    <$> v .: "id"
    <*> v .: "message_count"
    <*> v .: "created_at"
    <*> v .:? "ended_at"
    <*> v .: "summarized"
```

**Step 2: Build to verify**

Run: `cabal build wisp-core`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add wisp-core/src/Wisp/Client/Types.hs
git commit -m "feat(client): add Skill, AgentInfo, SessionSummary types"
```

---

## Task 4: Client Functions for Skills and Agents

**Files:**
- Modify: `wisp-core/src/Wisp/Client.hs`

**Step 1: Add client functions**

```haskell
-- Add to exports:
, getSkills
, getAgents
, getAgentInfo
, getAgentSessions
, Skill(..)
, AgentInfo(..)
, AgentSoul(..)
, SessionSummary(..)

-- Add to imports from Types:
import Wisp.Client.Types (Skill(..), AgentInfo(..), AgentSoul(..), SessionSummary(..))

-- Add functions:

-- | Get list of skills
getSkills :: ClientConfig -> IO (Either ClientError [Skill])
getSkills cfg = do
  result <- httpGet cfg "/api/skills"
  pure $ case result of
    Left e -> Left e
    Right val -> case val of
      Aeson.Object obj -> case KM.lookup "skills" obj of
        Just (Aeson.Array arr) -> case traverse Aeson.fromJSON arr of
          Aeson.Success skills -> Right $ toList skills
          _ -> Left $ ParseError "Failed to parse skills array"
        _ -> Left $ ParseError "Missing skills key"
      _ -> Left $ ParseError "Expected object"

-- | Get list of agent names
getAgents :: ClientConfig -> IO (Either ClientError [Text])
getAgents cfg = do
  result <- httpGet cfg "/api/agents"
  pure $ case result of
    Left e -> Left e
    Right val -> case val of
      Aeson.Object obj -> case KM.lookup "agents" obj of
        Just (Aeson.Array arr) -> Right [t | Aeson.String t <- toList arr]
        _ -> Left $ ParseError "Missing agents key"
      _ -> Left $ ParseError "Expected object"

-- | Get agent details by name
getAgentInfo :: ClientConfig -> Text -> IO (Either ClientError AgentInfo)
getAgentInfo cfg name = httpGet cfg $ "/api/agents/" <> T.unpack name

-- | Get recent sessions for agent
getAgentSessions :: ClientConfig -> Text -> IO (Either ClientError [SessionSummary])
getAgentSessions cfg name = do
  result <- httpGet cfg $ "/api/agents/" <> T.unpack name <> "/sessions"
  pure $ case result of
    Left e -> Left e
    Right val -> case val of
      Aeson.Object obj -> case KM.lookup "sessions" obj of
        Just (Aeson.Array arr) -> case traverse Aeson.fromJSON arr of
          Aeson.Success sessions -> Right $ toList sessions
          _ -> Left $ ParseError "Failed to parse sessions array"
        _ -> Left $ ParseError "Missing sessions key"
      _ -> Left $ ParseError "Expected object"
```

**Step 2: Build to verify**

Run: `cabal build wisp-core`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add wisp-core/src/Wisp/Client.hs
git commit -m "feat(client): add getSkills, getAgents, getAgentInfo, getAgentSessions"
```

---

## Task 5: Update TUI Types for New Views

**Files:**
- Modify: `wisp-tui/src/Tui/Types.hs`

**Step 1: Update View enum**

Change `View` to add new views:

```haskell
-- | Main views
data View = ChatView | ActivitiesView | KnowledgeView | SkillsView | AgentsView | ApprovalsView
  deriving (Show, Eq, Ord, Enum, Bounded)
```

**Step 2: Add KnowledgeTab (replaces DocumentTab)**

```haskell
-- | Knowledge sub-tabs
data KnowledgeTab = NotesTab | PrefsTab
  deriving (Show, Eq, Ord, Enum, Bounded)
```

**Step 3: Replace DocumentsState with KnowledgeState**

```haskell
-- | Knowledge view state (was Documents)
data KnowledgeState = KnowledgeState
  { _ksCurrentTab :: KnowledgeTab
  , _ksNotes :: [Document]
  , _ksPrefs :: [Document]
  , _ksSelected :: Int
  , _ksExpanded :: Maybe Int
  } deriving (Show)

makeLenses ''KnowledgeState
```

**Step 4: Add SkillsState**

```haskell
-- | Skills view state
data SkillsState = SkillsState
  { _ssSkills :: [Skill]
  , _ssSelected :: Int
  , _ssExpanded :: Maybe Int
  } deriving (Show)

makeLenses ''SkillsState
```

**Step 5: Add AgentsState**

```haskell
-- | Agents view state
data AgentsState = AgentsState
  { _agsAgents :: [Text]  -- Agent names
  , _agsSelected :: Int
  , _agsExpanded :: Maybe Int
  , _agsDetail :: Maybe AgentInfo  -- Loaded on expand
  , _agsSessions :: [SessionSummary]  -- Loaded on expand
  } deriving (Show)

makeLenses ''AgentsState
```

**Step 6: Add viewport Names**

```haskell
data Name
  = ChatInput
  | ChatHistory
  | ActivityList
  | ActivityDetail
  | KnowledgeList
  | KnowledgeDetail
  | SkillsList
  | SkillsDetail
  | AgentsList
  | AgentsDetail
  | ApprovalList
  | ApprovalDetail
  deriving (Show, Eq, Ord)
```

**Step 7: Update AppState**

```haskell
data AppState = AppState
  { _currentView :: View
  , _chatState :: ChatState
  , _activitiesState :: ActivitiesState
  , _knowledgeState :: KnowledgeState   -- was documentsState
  , _skillsState :: SkillsState         -- new
  , _agentsState :: AgentsState         -- new
  , _approvalsState :: ApprovalsState
  , _clientConfig :: ClientConfig
  , _statusMessage :: Maybe (Text, UTCTime, StatusSeverity)
  , _currentTime :: Maybe UTCTime
  }

makeLenses ''AppState
```

**Step 8: Update AppEvent**

```haskell
data AppEvent
  = ChatEventReceived ChatEvent
  | ActivitiesLoaded [Activity]
  | KnowledgeLoaded [Document] [Document]  -- notes, prefs (removed projects)
  | SkillsLoaded [Skill]
  | AgentsLoaded [Text]
  | AgentDetailLoaded AgentInfo [SessionSummary]
  | ApprovalsLoaded [(Activity, Text, Text)]
  | LoadError Text
  | RefreshView View
  | Tick
  deriving (Show, Eq)
```

**Step 9: Update module exports**

Update module exports to include all new types, states, and lenses.

**Step 10: Add imports**

```haskell
import Wisp.Client (ClientConfig, Activity, Document, Skill, AgentInfo, SessionSummary)
```

**Step 11: Build to verify**

Run: `cabal build wisp-tui`
Expected: Will fail with missing modules - that's expected, proceed to next tasks

**Step 12: Commit**

```bash
git add wisp-tui/src/Tui/Types.hs
git commit -m "feat(tui): update types for Knowledge, Skills, Agents views"
```

---

## Task 6: Rename Documents View to Knowledge

**Files:**
- Rename: `wisp-tui/src/Tui/Views/Documents.hs` → `wisp-tui/src/Tui/Views/Knowledge.hs`
- Modify: `wisp-tui/wisp-tui.cabal`

**Step 1: Create Knowledge.hs (based on Documents.hs)**

```haskell
module Tui.Views.Knowledge
  ( knowledgeWidget
  , handleKnowledgeEvent
  ) where

import Brick
import Brick.Main (vScrollBy, viewportScroll)
import qualified Graphics.Vty as V
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Tui.Widgets.Scroll (handleViewportScroll, pageSize)
import Wisp.Client (Document(..))

-- | Knowledge view widget
knowledgeWidget :: KnowledgeState -> Widget Name
knowledgeWidget ks = case ks ^. ksExpanded of
  Just idx -> detailView ks idx
  Nothing -> vBox
    [ tabBar ks
    , viewport KnowledgeList Vertical $ documentContent ks
    ]

tabBar :: KnowledgeState -> Widget Name
tabBar ks = vLimit 1 $ hBox
  [ renderTab ks NotesTab "1:Notes"
  , txt "  "
  , renderTab ks PrefsTab "2:Prefs"
  , fill ' '
  ]

renderTab :: KnowledgeState -> KnowledgeTab -> Text -> Widget Name
renderTab ks tab label
  | ks ^. ksCurrentTab == tab = withAttr (attrName "selectedTab") $ txt $ "[" <> label <> "]"
  | otherwise = txt $ " " <> label <> " "

documentContent :: KnowledgeState -> Widget Name
documentContent ks = case ks ^. ksCurrentTab of
  NotesTab -> notesContent ks
  PrefsTab -> prefsContent ks

notesContent :: KnowledgeState -> Widget Name
notesContent ks = vBox $
  [ padBottom (Pad 1) $ txt "  Title                           Tags"
  , txt "  ------"
  ] ++ if null (ks ^. ksNotes)
       then [padAll 1 $ txt "No notes. Press 'r' to refresh."]
       else zipWith (renderNote (ks ^. ksSelected)) [0..] (ks ^. ksNotes)

renderNote :: Int -> Int -> Document -> Widget Name
renderNote selected idx doc =
  let isSelected = idx == selected
      marker = if isSelected then "> " else "  "
      title = extractField "title" (documentData doc)
      tags = T.intercalate ", " (documentTags doc)
  in hBox
    [ txt marker
    , txt $ T.justifyLeft 32 ' ' (T.take 30 title)
    , txt tags
    ]

prefsContent :: KnowledgeState -> Widget Name
prefsContent ks = vBox $
  [ padBottom (Pad 1) $ txt "  Key                     Value               Context"
  , txt "  ------"
  ] ++ if null (ks ^. ksPrefs)
       then [padAll 1 $ txt "No preferences. Press 'r' to refresh."]
       else zipWith (renderPref (ks ^. ksSelected)) [0..] (ks ^. ksPrefs)

renderPref :: Int -> Int -> Document -> Widget Name
renderPref selected idx doc =
  let isSelected = idx == selected
      marker = if isSelected then "> " else "  "
      key = extractField "key" (documentData doc)
      value = extractField "value" (documentData doc)
      context = extractField "context" (documentData doc)
  in hBox
    [ txt marker
    , txt $ T.justifyLeft 22 ' ' key
    , txt $ T.justifyLeft 20 ' ' value
    , txt context
    ]

detailView :: KnowledgeState -> Int -> Widget Name
detailView ks idx =
  let docs = currentDocs ks
  in if idx >= length docs
     then txt "Invalid selection"
     else let doc = docs !! idx
          in padAll 1 $ vBox
            [ txt $ "ID: " <> documentId doc
            , txt ""
            , txt $ "Type: " <> T.pack (show (documentType doc))
            , txt $ "Tags: " <> T.intercalate ", " (documentTags doc)
            , txt ""
            , txt "─────────────────────────────────────────"
            , txt ""
            , viewport KnowledgeDetail Vertical $ txtWrap $ showDocumentData (documentData doc)
            , txt ""
            , txt "Press Esc or 'h' to go back"
            ]

showDocumentData :: Value -> Text
showDocumentData (Object obj) = T.intercalate "\n" $
  [ k <> ": " <> showValue v | (k, v) <- KM.toList obj ]
  where
    showValue (String s) = s
    showValue v = T.pack (show v)
showDocumentData v = T.pack (show v)

extractField :: Text -> Value -> Text
extractField key (Object obj) = case KM.lookup (fromString $ T.unpack key) obj of
  Just (String s) -> s
  _ -> ""
extractField _ _ = ""

currentDocs :: KnowledgeState -> [Document]
currentDocs ks = case ks ^. ksCurrentTab of
  NotesTab -> ks ^. ksNotes
  PrefsTab -> ks ^. ksPrefs

-- | Handle knowledge-specific events
handleKnowledgeEvent :: V.Event -> EventM Name AppState ()
handleKnowledgeEvent evt = do
  s <- get
  case s ^. knowledgeState . ksExpanded of
    Just _ -> handleDetailEvent evt
    Nothing -> handleListEvent evt

handleListEvent :: V.Event -> EventM Name AppState ()
handleListEvent (V.EvKey (V.KChar '1') []) =
  modify $ knowledgeState . ksCurrentTab .~ NotesTab
handleListEvent (V.EvKey (V.KChar '2') []) =
  modify $ knowledgeState . ksCurrentTab .~ PrefsTab
handleListEvent (V.EvKey V.KEsc []) = pure ()
handleListEvent (V.EvKey (V.KChar 'l') []) = openDetail
handleListEvent (V.EvKey V.KEnter []) = openDetail
handleListEvent (V.EvKey (V.KChar 'j') []) = do
  s <- get
  let maxIdx = length (currentDocs (s ^. knowledgeState)) - 1
  modify $ knowledgeState . ksSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
  vScrollBy (viewportScroll KnowledgeList) 1
handleListEvent (V.EvKey (V.KChar 'k') []) = do
  modify $ knowledgeState . ksSelected %~ (\i -> max 0 (i - 1))
  vScrollBy (viewportScroll KnowledgeList) (-1)
handleListEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) = do
  s <- get
  let maxIdx = length (currentDocs (s ^. knowledgeState)) - 1
  modify $ knowledgeState . ksSelected %~ (\i -> min (i + pageSize) (max 0 maxIdx))
  vScrollBy (viewportScroll KnowledgeList) pageSize
handleListEvent (V.EvKey (V.KChar 'u') [V.MCtrl]) = do
  modify $ knowledgeState . ksSelected %~ (\i -> max 0 (i - pageSize))
  vScrollBy (viewportScroll KnowledgeList) (-pageSize)
handleListEvent _ = pure ()

handleDetailEvent :: V.Event -> EventM Name AppState ()
handleDetailEvent (V.EvKey V.KEsc []) = closeDetail
handleDetailEvent (V.EvKey (V.KChar 'h') []) = closeDetail
handleDetailEvent evt = do
  _ <- handleViewportScroll KnowledgeDetail evt
  pure ()

openDetail :: EventM Name AppState ()
openDetail = do
  s <- get
  let sel = s ^. knowledgeState . ksSelected
  modify $ knowledgeState . ksExpanded .~ Just sel

closeDetail :: EventM Name AppState ()
closeDetail = modify $ knowledgeState . ksExpanded .~ Nothing
```

**Step 2: Update cabal file**

Replace `Tui.Views.Documents` with `Tui.Views.Knowledge` in `other-modules`.

**Step 3: Delete old file**

```bash
rm wisp-tui/src/Tui/Views/Documents.hs
```

**Step 4: Commit**

```bash
git add wisp-tui/src/Tui/Views/Knowledge.hs wisp-tui/wisp-tui.cabal
git rm wisp-tui/src/Tui/Views/Documents.hs
git commit -m "refactor(tui): rename Documents view to Knowledge with expansion"
```

---

## Task 7: Create Skills View

**Files:**
- Create: `wisp-tui/src/Tui/Views/Skills.hs`
- Modify: `wisp-tui/wisp-tui.cabal`

**Step 1: Create Skills.hs**

```haskell
module Tui.Views.Skills
  ( skillsWidget
  , handleSkillsEvent
  ) where

import Brick
import Brick.Main (vScrollBy, viewportScroll)
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Tui.Widgets.Scroll (handleViewportScroll, pageSize)
import Wisp.Client (Skill(..))

-- | Skills view widget
skillsWidget :: SkillsState -> Widget Name
skillsWidget ss = case ss ^. ssExpanded of
  Just idx -> detailView ss idx
  Nothing -> viewport SkillsList Vertical $ skillsList ss

skillsList :: SkillsState -> Widget Name
skillsList ss =
  let skills = ss ^. ssSkills
      selected = ss ^. ssSelected
  in if null skills
     then padAll 1 $ txt "No skills available. Press 'r' to refresh."
     else vBox $
       [ padBottom (Pad 1) $ txt "  Skill             Tools"
       , txt "  ------"
       ] ++ zipWith (renderSkillRow selected) [0..] skills

renderSkillRow :: Int -> Int -> Skill -> Widget Name
renderSkillRow selected idx skill =
  let isSelected = idx == selected
      marker = if isSelected then "> " else "  "
      name = skillName skill
      toolCount = length (skillTools skill)
      status = if skillAvailable skill then "" else " (unavailable)"
  in hBox
    [ txt marker
    , txt $ T.justifyLeft 18 ' ' name
    , txt $ T.pack (show toolCount) <> " tools" <> status
    ]

detailView :: SkillsState -> Int -> Widget Name
detailView ss idx =
  let skills = ss ^. ssSkills
  in if idx >= length skills
     then txt "Invalid selection"
     else let skill = skills !! idx
          in padAll 1 $ vBox
            [ withAttr (attrName "title") $ txt $ "Skill: " <> skillName skill
            , txt ""
            , txt $ "Status: " <> if skillAvailable skill then "Available" else "Unavailable"
            , txt ""
            , txt "─── Tools ───────────────────────────────"
            , txt ""
            , viewport SkillsDetail Vertical $ vBox $
                if null (skillTools skill)
                then [txt "(no tools)"]
                else map (\t -> txt $ "  • " <> t) (skillTools skill)
            , txt ""
            , txt "Press Esc or 'h' to go back"
            ]

-- | Handle skills-specific events
handleSkillsEvent :: V.Event -> EventM Name AppState ()
handleSkillsEvent evt = do
  s <- get
  case s ^. skillsState . ssExpanded of
    Just _ -> handleDetailEvent evt
    Nothing -> handleListEvent evt

handleListEvent :: V.Event -> EventM Name AppState ()
handleListEvent (V.EvKey V.KEsc []) = pure ()
handleListEvent (V.EvKey (V.KChar 'l') []) = openDetail
handleListEvent (V.EvKey V.KEnter []) = openDetail
handleListEvent (V.EvKey (V.KChar 'j') []) = do
  s <- get
  let maxIdx = length (s ^. skillsState . ssSkills) - 1
  modify $ skillsState . ssSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
  vScrollBy (viewportScroll SkillsList) 1
handleListEvent (V.EvKey (V.KChar 'k') []) = do
  modify $ skillsState . ssSelected %~ (\i -> max 0 (i - 1))
  vScrollBy (viewportScroll SkillsList) (-1)
handleListEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) = do
  s <- get
  let maxIdx = length (s ^. skillsState . ssSkills) - 1
  modify $ skillsState . ssSelected %~ (\i -> min (i + pageSize) (max 0 maxIdx))
  vScrollBy (viewportScroll SkillsList) pageSize
handleListEvent (V.EvKey (V.KChar 'u') [V.MCtrl]) = do
  modify $ skillsState . ssSelected %~ (\i -> max 0 (i - pageSize))
  vScrollBy (viewportScroll SkillsList) (-pageSize)
handleListEvent _ = pure ()

handleDetailEvent :: V.Event -> EventM Name AppState ()
handleDetailEvent (V.EvKey V.KEsc []) = closeDetail
handleDetailEvent (V.EvKey (V.KChar 'h') []) = closeDetail
handleDetailEvent evt = do
  _ <- handleViewportScroll SkillsDetail evt
  pure ()

openDetail :: EventM Name AppState ()
openDetail = do
  s <- get
  let sel = s ^. skillsState . ssSelected
  modify $ skillsState . ssExpanded .~ Just sel

closeDetail :: EventM Name AppState ()
closeDetail = modify $ skillsState . ssExpanded .~ Nothing
```

**Step 2: Add to cabal file**

Add `Tui.Views.Skills` to `other-modules`.

**Step 3: Commit**

```bash
git add wisp-tui/src/Tui/Views/Skills.hs wisp-tui/wisp-tui.cabal
git commit -m "feat(tui): add Skills view with list and detail"
```

---

## Task 8: Create Agents View

**Files:**
- Create: `wisp-tui/src/Tui/Views/Agents.hs`
- Modify: `wisp-tui/wisp-tui.cabal`

**Step 1: Create Agents.hs**

```haskell
module Tui.Views.Agents
  ( agentsWidget
  , handleAgentsEvent
  ) where

import Brick
import Brick.Main (vScrollBy, viewportScroll)
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime)
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Tui.Widgets.Scroll (handleViewportScroll, pageSize)
import Wisp.Client (AgentInfo(..), AgentSoul(..), SessionSummary(..))

-- | Agents view widget
agentsWidget :: Maybe UTCTime -> AgentsState -> Widget Name
agentsWidget mNow ags = case ags ^. agsExpanded of
  Just idx -> detailView mNow ags idx
  Nothing -> viewport AgentsList Vertical $ agentsList ags

agentsList :: AgentsState -> Widget Name
agentsList ags =
  let agents = ags ^. agsAgents
      selected = ags ^. agsSelected
  in if null agents
     then padAll 1 $ txt "No agents configured. Press 'r' to refresh."
     else vBox $
       [ padBottom (Pad 1) $ txt "  Agent"
       , txt "  ------"
       ] ++ zipWith (renderAgentRow selected) [0..] agents

renderAgentRow :: Int -> Int -> Text -> Widget Name
renderAgentRow selected idx name =
  let isSelected = idx == selected
      marker = if isSelected then "> " else "  "
  in txt $ marker <> name

detailView :: Maybe UTCTime -> AgentsState -> Int -> Widget Name
detailView mNow ags idx =
  let agents = ags ^. agsAgents
  in if idx >= length agents
     then txt "Invalid selection"
     else let agentName = agents !! idx
          in padAll 1 $ vBox
            [ withAttr (attrName "title") $ txt $ "Agent: " <> agentName
            , txt ""
            , case ags ^. agsDetail of
                Nothing -> txt "Loading..."
                Just info -> agentDetailContent mNow info (ags ^. agsSessions)
            , txt ""
            , txt "Press Esc or 'h' to go back"
            ]

agentDetailContent :: Maybe UTCTime -> AgentInfo -> [SessionSummary] -> Widget Name
agentDetailContent mNow info sessions = viewport AgentsDetail Vertical $ vBox
  [ txt $ "Active Skill: " <> maybe "(none)" id (agentInfoActiveSkill info)
  , txt ""
  , txt "─── Personality ──────────────────────────"
  , txtWrap $ agentInfoPersonality info
  , txt ""
  , txt "─── Soul ─────────────────────────────────"
  , if T.null (soulPersonality (agentInfoSoul info))
    then txt "(no soul evolution yet)"
    else txtWrap $ soulPersonality (agentInfoSoul info)
  , txt ""
  , txt "─── Insights ─────────────────────────────"
  , if null (soulInsights (agentInfoSoul info))
    then txt "(no insights yet)"
    else vBox $ map (\i -> txt $ "  • " <> i) (soulInsights (agentInfoSoul info))
  , txt ""
  , txt "─── Recent Sessions ──────────────────────"
  , if null sessions
    then txt "(no sessions)"
    else vBox $ map (renderSession mNow) sessions
  ]

renderSession :: Maybe UTCTime -> SessionSummary -> Widget Name
renderSession mNow sess =
  let timeStr = case mNow of
        Nothing -> T.take 16 $ T.pack $ show (sessionSummaryCreatedAt sess)
        Just now -> formatRelative now (sessionSummaryCreatedAt sess)
      status = if sessionSummarySummarized sess then " [summarized]" else ""
  in txt $ "  " <> timeStr <> " - " <> T.pack (show (sessionSummaryMessageCount sess)) <> " messages" <> status

formatRelative :: UTCTime -> UTCTime -> Text
formatRelative now created =
  let diff = diffUTCTime now created
  in if diff < 60 then "just now"
     else if diff < 3600 then T.pack (show (round (diff / 60) :: Int)) <> "m ago"
     else if diff < 86400 then T.pack (show (round (diff / 3600) :: Int)) <> "h ago"
     else T.pack (show (round (diff / 86400) :: Int)) <> "d ago"

-- | Handle agents-specific events
handleAgentsEvent :: V.Event -> EventM Name AppState ()
handleAgentsEvent evt = do
  s <- get
  case s ^. agentsState . agsExpanded of
    Just _ -> handleDetailEvent evt
    Nothing -> handleListEvent evt

handleListEvent :: V.Event -> EventM Name AppState ()
handleListEvent (V.EvKey V.KEsc []) = pure ()
handleListEvent (V.EvKey (V.KChar 'l') []) = openDetail
handleListEvent (V.EvKey V.KEnter []) = openDetail
handleListEvent (V.EvKey (V.KChar 'j') []) = do
  s <- get
  let maxIdx = length (s ^. agentsState . agsAgents) - 1
  modify $ agentsState . agsSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
  vScrollBy (viewportScroll AgentsList) 1
handleListEvent (V.EvKey (V.KChar 'k') []) = do
  modify $ agentsState . agsSelected %~ (\i -> max 0 (i - 1))
  vScrollBy (viewportScroll AgentsList) (-1)
handleListEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) = do
  s <- get
  let maxIdx = length (s ^. agentsState . agsAgents) - 1
  modify $ agentsState . agsSelected %~ (\i -> min (i + pageSize) (max 0 maxIdx))
  vScrollBy (viewportScroll AgentsList) pageSize
handleListEvent (V.EvKey (V.KChar 'u') [V.MCtrl]) = do
  modify $ agentsState . agsSelected %~ (\i -> max 0 (i - pageSize))
  vScrollBy (viewportScroll AgentsList) (-pageSize)
handleListEvent _ = pure ()

handleDetailEvent :: V.Event -> EventM Name AppState ()
handleDetailEvent (V.EvKey V.KEsc []) = closeDetail
handleDetailEvent (V.EvKey (V.KChar 'h') []) = closeDetail
handleDetailEvent evt = do
  _ <- handleViewportScroll AgentsDetail evt
  pure ()

openDetail :: EventM Name AppState ()
openDetail = do
  s <- get
  let sel = s ^. agentsState . agsSelected
  modify $ agentsState . agsExpanded .~ Just sel
  -- Note: actual detail loading happens via triggerAgentDetailLoad in Main.hs

closeDetail :: EventM Name AppState ()
closeDetail = do
  modify $ agentsState . agsExpanded .~ Nothing
  modify $ agentsState . agsDetail .~ Nothing
  modify $ agentsState . agsSessions .~ []
```

**Step 2: Add to cabal file**

Add `Tui.Views.Agents` to `other-modules`.

**Step 3: Commit**

```bash
git add wisp-tui/src/Tui/Views/Agents.hs wisp-tui/wisp-tui.cabal
git commit -m "feat(tui): add Agents view with soul and sessions"
```

---

## Task 9: Update DataLoader

**Files:**
- Modify: `wisp-tui/src/Tui/DataLoader.hs`

**Step 1: Update DataLoader with new load functions**

```haskell
module Tui.DataLoader
  ( loadActivities
  , loadKnowledge
  , loadSkills
  , loadAgents
  , loadAgentDetail
  , loadApprovals
  , DataLoadResult(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Wisp.Client
  ( ClientConfig
  , ClientError(..)
  , Activity
  , Document
  , Skill
  , AgentInfo
  , SessionSummary
  , ApprovalItem(..)
  , getActivities
  , getNotes
  , getPreferences
  , getSkills
  , getAgents
  , getAgentInfo
  , getAgentSessions
  , getApprovals
  )

-- | Result of a data load operation
data DataLoadResult
  = ActivitiesLoaded [Activity]
  | KnowledgeLoaded [Document] [Document]  -- notes, prefs
  | SkillsLoaded [Skill]
  | AgentsLoaded [Text]
  | AgentDetailLoaded AgentInfo [SessionSummary]
  | ApprovalsLoaded [(Activity, Text, Text)]
  | LoadError Text
  deriving (Show, Eq)

-- | Load activities from server
loadActivities :: ClientConfig -> IO DataLoadResult
loadActivities cfg = do
  result <- getActivities cfg
  pure $ case result of
    Left err -> LoadError $ "Failed to load activities: " <> showError err
    Right acts -> ActivitiesLoaded acts

-- | Load knowledge (notes + prefs) from server
loadKnowledge :: ClientConfig -> IO DataLoadResult
loadKnowledge cfg = do
  notesResult <- getNotes cfg
  prefsResult <- getPreferences cfg

  let notes = either (const []) id notesResult
      prefs = either (const []) id prefsResult

  pure $ KnowledgeLoaded notes prefs

-- | Load skills from server
loadSkills :: ClientConfig -> IO DataLoadResult
loadSkills cfg = do
  result <- getSkills cfg
  pure $ case result of
    Left err -> LoadError $ "Failed to load skills: " <> showError err
    Right skills -> SkillsLoaded skills

-- | Load agent names from server
loadAgents :: ClientConfig -> IO DataLoadResult
loadAgents cfg = do
  result <- getAgents cfg
  pure $ case result of
    Left err -> LoadError $ "Failed to load agents: " <> showError err
    Right agents -> AgentsLoaded agents

-- | Load agent detail (info + sessions)
loadAgentDetail :: ClientConfig -> Text -> IO DataLoadResult
loadAgentDetail cfg agentName = do
  infoResult <- getAgentInfo cfg agentName
  sessionsResult <- getAgentSessions cfg agentName
  case (infoResult, sessionsResult) of
    (Left err, _) -> pure $ LoadError $ "Failed to load agent: " <> showError err
    (_, Left err) -> pure $ LoadError $ "Failed to load sessions: " <> showError err
    (Right info, Right sessions) -> pure $ AgentDetailLoaded info sessions

-- | Load approvals from server
loadApprovals :: ClientConfig -> IO DataLoadResult
loadApprovals cfg = do
  result <- getApprovals cfg
  pure $ case result of
    Left err -> LoadError $ "Failed to load approvals: " <> showError err
    Right items -> ApprovalsLoaded
      [ (approvalActivity item, approvalType item, approvalReason item)
      | item <- items
      ]

showError :: ClientError -> Text
showError (HttpError t) = t
showError (ParseError t) = t
showError (ServerError code t) = "Server error " <> T.pack (show code) <> ": " <> t
```

**Step 2: Commit**

```bash
git add wisp-tui/src/Tui/DataLoader.hs
git commit -m "feat(tui): update DataLoader for Knowledge, Skills, Agents"
```

---

## Task 10: Update Layout Widget

**Files:**
- Modify: `wisp-tui/src/Tui/Widgets/Layout.hs`

**Step 1: Update viewTabName for new views**

```haskell
-- | View tab display names
viewTabName :: View -> Text
viewTabName ChatView = "Chat"
viewTabName ActivitiesView = "Activities"
viewTabName KnowledgeView = "Knowledge"
viewTabName SkillsView = "Skills"
viewTabName AgentsView = "Agents"
viewTabName ApprovalsView = "Approvals"
```

**Step 2: Commit**

```bash
git add wisp-tui/src/Tui/Widgets/Layout.hs
git commit -m "feat(tui): update tab names for new views"
```

---

## Task 11: Wire Up Main.hs

**Files:**
- Modify: `wisp-tui/app/Main.hs`

**Step 1: Update imports**

```haskell
import qualified Tui.DataLoader as DL
import Wisp.Client.SSE (ChatEvent(..))
import Tui.Types
import Tui.Views.Activities (activitiesWidget, handleActivitiesEvent)
import Tui.Views.Approvals (approvalsWidget, handleApprovalsEvent)
import Tui.Views.Chat (chatWidget, handleChatEvent, sendChatMessage)
import Tui.Views.Knowledge (knowledgeWidget, handleKnowledgeEvent)
import Tui.Views.Skills (skillsWidget, handleSkillsEvent)
import Tui.Views.Agents (agentsWidget, handleAgentsEvent)
import Tui.Widgets.Layout (headerWidget, statusBarWidget)
import Wisp.Client (defaultConfig)
```

**Step 2: Update initial state**

```haskell
let initialState = AppState
      { _currentView = ChatView
      , _chatState = ChatState
          { _csMessages = []
          , _csInputBuffer = ""
          , _csCurrentAgent = "wisp/concierge"
          , _csCurrentSession = "default"
          , _csStreaming = False
          , _csStreamBuffer = ""
          }
      , _activitiesState = ActivitiesState [] 0 Nothing ""
      , _knowledgeState = KnowledgeState NotesTab [] [] 0 Nothing
      , _skillsState = SkillsState [] 0 Nothing
      , _agentsState = AgentsState [] 0 Nothing Nothing []
      , _approvalsState = ApprovalsState [] 0 Nothing
      , _clientConfig = defaultConfig
      , _statusMessage = Just ("Welcome to wisp-tui | Ctrl-Q to quit", now, StatusInfo)
      , _currentTime = Just now
      }
```

**Step 3: Update viewContent**

```haskell
viewContent :: AppState -> Widget Name
viewContent s = case s ^. currentView of
  ChatView -> chatWidget (s ^. chatState)
  ActivitiesView -> activitiesWidget (s ^. currentTime) (s ^. activitiesState)
  KnowledgeView -> knowledgeWidget (s ^. knowledgeState)
  SkillsView -> skillsWidget (s ^. skillsState)
  AgentsView -> agentsWidget (s ^. currentTime) (s ^. agentsState)
  ApprovalsView -> approvalsWidget (s ^. approvalsState)
```

**Step 4: Update event handlers**

```haskell
handleEvent _ (AppEvent (KnowledgeLoaded notes prefs)) = do
  modify $ knowledgeState . ksNotes .~ notes
  modify $ knowledgeState . ksPrefs .~ prefs
  modify $ knowledgeState . ksSelected .~ 0
handleEvent _ (AppEvent (SkillsLoaded skills)) = do
  modify $ skillsState . ssSkills .~ skills
  modify $ skillsState . ssSelected .~ 0
handleEvent _ (AppEvent (AgentsLoaded agents)) = do
  modify $ agentsState . agsAgents .~ agents
  modify $ agentsState . agsSelected .~ 0
handleEvent _ (AppEvent (AgentDetailLoaded info sessions)) = do
  modify $ agentsState . agsDetail .~ Just info
  modify $ agentsState . agsSessions .~ sessions
handleEvent _ (AppEvent (LoadError msg)) = do
  now <- liftIO getCurrentTime
  modify $ statusMessage .~ Just (msg, now, StatusError)
```

**Step 5: Update VtyEvent handlers for new views**

```haskell
handleEvent chan (VtyEvent (V.EvKey V.KEnter [])) = do
  s <- get
  case s ^. currentView of
    ChatView -> handleChatEnter chan
    ActivitiesView -> handleActivitiesEvent (V.EvKey V.KEnter [])
    KnowledgeView -> handleKnowledgeEvent (V.EvKey V.KEnter [])
    SkillsView -> handleSkillsEvent (V.EvKey V.KEnter [])
    AgentsView -> do
      handleAgentsEvent (V.EvKey V.KEnter [])
      triggerAgentDetailLoad chan
    ApprovalsView -> handleApprovalsEvent (V.EvKey V.KEnter [])

handleEvent _ (VtyEvent e) = do
  s <- get
  case s ^. currentView of
    ChatView -> handleChatEvent e
    ActivitiesView -> handleActivitiesEvent e
    KnowledgeView -> handleKnowledgeEvent e
    SkillsView -> handleSkillsEvent e
    AgentsView -> handleAgentsEvent e
    ApprovalsView -> handleApprovalsEvent e
```

**Step 6: Update triggerDataLoad**

```haskell
triggerDataLoad :: BChan AppEvent -> EventM Name AppState ()
triggerDataLoad chan = do
  s <- get
  let cfg = s ^. clientConfig
      view = s ^. currentView

  void $ liftIO $ async $ do
    result <- case view of
      ActivitiesView -> DL.loadActivities cfg
      KnowledgeView -> DL.loadKnowledge cfg
      SkillsView -> DL.loadSkills cfg
      AgentsView -> DL.loadAgents cfg
      ApprovalsView -> DL.loadApprovals cfg
      ChatView -> pure $ DL.LoadError "Chat doesn't need loading"

    case result of
      DL.ActivitiesLoaded acts ->
        writeBChan chan (ActivitiesLoaded acts)
      DL.KnowledgeLoaded notes prefs ->
        writeBChan chan (KnowledgeLoaded notes prefs)
      DL.SkillsLoaded skills ->
        writeBChan chan (SkillsLoaded skills)
      DL.AgentsLoaded agents ->
        writeBChan chan (AgentsLoaded agents)
      DL.AgentDetailLoaded info sessions ->
        writeBChan chan (AgentDetailLoaded info sessions)
      DL.ApprovalsLoaded items ->
        writeBChan chan (ApprovalsLoaded items)
      DL.LoadError msg ->
        writeBChan chan (LoadError msg)
```

**Step 7: Add triggerAgentDetailLoad**

```haskell
-- | Load agent detail when expanding
triggerAgentDetailLoad :: BChan AppEvent -> EventM Name AppState ()
triggerAgentDetailLoad chan = do
  s <- get
  case s ^. agentsState . agsExpanded of
    Nothing -> pure ()
    Just idx -> do
      let agents = s ^. agentsState . agsAgents
          cfg = s ^. clientConfig
      when (idx < length agents) $ do
        let agentName = agents !! idx
        void $ liftIO $ async $ do
          result <- DL.loadAgentDetail cfg agentName
          case result of
            DL.AgentDetailLoaded info sessions ->
              writeBChan chan (AgentDetailLoaded info sessions)
            DL.LoadError msg ->
              writeBChan chan (LoadError msg)
            _ -> pure ()
```

**Step 8: Update nextView/prevView**

```haskell
nextView :: View -> View
nextView ChatView = ActivitiesView
nextView ActivitiesView = KnowledgeView
nextView KnowledgeView = SkillsView
nextView SkillsView = AgentsView
nextView AgentsView = ApprovalsView
nextView ApprovalsView = ChatView

prevView :: View -> View
prevView ChatView = ApprovalsView
prevView ActivitiesView = ChatView
prevView KnowledgeView = ActivitiesView
prevView SkillsView = KnowledgeView
prevView AgentsView = SkillsView
prevView ApprovalsView = AgentsView
```

**Step 9: Add import for Control.Monad**

```haskell
import Control.Monad (forever, void, when)
```

**Step 10: Build to verify**

Run: `cabal build wisp-tui`
Expected: Compiles without errors

**Step 11: Commit**

```bash
git add wisp-tui/app/Main.hs
git commit -m "feat(tui): wire up Knowledge, Skills, Agents views in Main"
```

---

## Task 12: Final Build and Test

**Step 1: Full build**

Run: `cabal build all`
Expected: All packages compile

**Step 2: Start server**

Run: `cabal run wisp-srv`

**Step 3: Start TUI (in another terminal)**

Run: `cabal run wisp-tui`

**Step 4: Test each view**

- Press Tab to cycle through views
- Test j/k navigation in each view
- Test Ctrl+D/Ctrl+U page scrolling
- Test Enter/l to expand details
- Test Esc/h to close details
- Verify errors show in status bar (red)

**Step 5: Final commit**

```bash
git add -A
git commit -m "feat(tui): complete Knowledge Hub implementation"
```
