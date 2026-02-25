module Tui.Views.Knowledge
  ( knowledgeWidget
  , handleKnowledgeEvent
  , handleKnowledgeEventWithAction
  , KnowledgeAction(..)
  ) where

import Brick
import qualified Graphics.Vty as V
import Data.Aeson (Value(..))
import Data.Foldable (toList)
import Data.List (intersperse)
import qualified Data.Aeson.KeyMap as KM
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Tui.Widgets.Scroll (handleViewportScroll)
import Tui.Widgets.Time (relativeTime, humanDate)
import Wisp.Client (Document(..), DocumentType(..))

-- | Actions that can be triggered from knowledge view
data KnowledgeAction
  = LoadProjectChildren Text  -- Load children for project with given ID
  | ViewActivitiesForTag Text -- Navigate to activities filtered by tag
  | KnowledgeNoAction
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Main Widget
--------------------------------------------------------------------------------

-- | Knowledge view widget
knowledgeWidget :: Maybe UTCTime -> KnowledgeState -> Widget Name
knowledgeWidget mNow ks = case ks ^. ksExpanded of
  Nothing -> listView ks
  Just idx -> projectDetailView mNow ks idx

--------------------------------------------------------------------------------
-- List View (project/notes/prefs list)
--------------------------------------------------------------------------------

listView :: KnowledgeState -> Widget Name
listView ks = vBox
  [ tabBar ks
  , viewport KnowledgeList Vertical $ knowledgeContent ks
  ]

tabBar :: KnowledgeState -> Widget Name
tabBar ks = vLimit 1 $ hBox
  [ renderTab ks ProjectsTab "1:Projects"
  , txt "  "
  , renderTab ks NotesTab "2:Notes"
  , txt "  "
  , renderTab ks PrefsTab "3:Prefs"
  , fill ' '
  ]

renderTab :: KnowledgeState -> KnowledgeTab -> Text -> Widget Name
renderTab ks tab label
  | ks ^. ksCurrentTab == tab = withAttr (attrName "selectedTab") $ txt $ "[" <> label <> "]"
  | otherwise = txt $ " " <> label <> " "

knowledgeContent :: KnowledgeState -> Widget Name
knowledgeContent ks = case ks ^. ksCurrentTab of
  ProjectsTab -> projectsContent ks
  NotesTab -> notesContent ks
  PrefsTab -> prefsContent ks

projectsContent :: KnowledgeState -> Widget Name
projectsContent ks = vBox $
  [ padBottom (Pad 1) $ txt "  Name                            Type"
  , txt "  ------"
  ] ++ if null (ks ^. ksProjects)
       then [padAll 1 $ txt "No projects."]
       else zipWith (renderProject (ks ^. ksSelected)) [0..] (ks ^. ksProjects)

renderProject :: Int -> Int -> Document -> Widget Name
renderProject selected idx doc =
  let isSelected = idx == selected
      marker = if isSelected then "> " else "  "
      name = extractField "name" (documentData doc)
      projType = extractField "type" (documentData doc)
  in hBox
    [ txt marker
    , txt $ T.justifyLeft 32 ' ' (T.take 30 name)
    , txt projType
    ]

notesContent :: KnowledgeState -> Widget Name
notesContent ks = vBox $
  [ padBottom (Pad 1) $ txt "  Title                           Tags"
  , txt "  ------"
  ] ++ if null (ks ^. ksNotes)
       then [padAll 1 $ txt "No notes."]
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
       then [padAll 1 $ txt "No preferences."]
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

--------------------------------------------------------------------------------
-- Project Detail View (card-based layout)
--------------------------------------------------------------------------------

projectDetailView :: Maybe UTCTime -> KnowledgeState -> Int -> Widget Name
projectDetailView mNow ks idx =
  let docs = currentDocs' ks
      mDoc = if idx < length docs then Just (docs !! idx) else Nothing
  in case mDoc of
    Nothing -> txt "Document not found"
    Just doc -> case ks ^. ksDocExpanded of
      -- Show expanded knowledge document
      Just docIdx -> knowledgeDocExpandedView mNow ks doc docIdx
      -- Show project card + document list
      Nothing -> projectCardView mNow ks doc

-- | Project card view with header and document list
projectCardView :: Maybe UTCTime -> KnowledgeState -> Document -> Widget Name
projectCardView mNow ks doc = vBox
  [ withAttr (attrName "dim") $ txt "[Esc/h to return]  [a] view activities"
  , txt ""
  , viewport KnowledgeDetail Vertical $ vBox
      [ projectHeaderCard mNow doc
      , txt ""
      , knowledgeDocsList mNow ks (ks ^. ksProjectChildren)
      ]
  ]

-- | Render the project header card
projectHeaderCard :: Maybe UTCTime -> Document -> Widget Name
projectHeaderCard mNow doc =
  let name = T.toUpper $ extractField "name" (documentData doc)
      projType = extractField "type" (documentData doc)
      status = extractField "status" (documentData doc)
      activityCount = extractNumber "activity_count" (documentData doc)
      participants = extractArray "participants" (documentData doc)
      tags = documentTags doc
      docId = documentId doc
      created = documentCreatedAt doc
      lastActivity = documentLastActivityAt doc

      -- Status styling
      statusWidget = if status == "active" || T.null status
        then withAttr (attrName "statusActive") $ txt "active"
        else withAttr (attrName "statusArchived") $ txt status

      -- Format activity count
      actCountText = T.pack (show activityCount) <> " activities"
      actWidget = hBox [txt actCountText, withAttr (attrName "hotkey") $ txt " [a]"]

      -- Format times
      lastActivityText = case (mNow, lastActivity) of
        (Just now, Just la) -> "Last active: " <> relativeTime now la
        _ -> ""
      createdText = "Created: " <> humanDate created

      -- Tags
      tagsWidget = hBox $ txt "Tags: " :
        intersperse (txt " ") [withAttr (attrName "tag") $ txt t | t <- tags]

      -- Participants
      participantsWidget = if null participants
        then emptyWidget
        else vBox $ txt "Participants:" :
          [txt $ "  - " <> p | p <- participants]

  in vBox
    [ -- Top line: name + status
      hBox
        [ withAttr (attrName "mdBold") $ txt $ "  " <> name
        , fill ' '
        , statusWidget
        , txt "  "
        ]
    -- Type + ID line
    , hBox
        [ withAttr (attrName "dim") $ txt $ "  " <> projType <> " project"
        , fill ' '
        , withAttr (attrName "dim") $ txt docId
        , txt "  "
        ]
    , txt $ "  " <> T.replicate 60 "─"
    -- Stats line
    , hBox
        [ txt "  "
        , actWidget
        , txt "   •   "
        , txt lastActivityText
        ]
    , txt $ "  " <> createdText
    , txt ""
    , txt "  " <+> tagsWidget
    , participantsWidget
    ]

-- | Knowledge documents list (selectable)
knowledgeDocsList :: Maybe UTCTime -> KnowledgeState -> [Document] -> Widget Name
knowledgeDocsList mNow ks children = vBox $
  [ hBox
      [ withAttr (attrName "mdHeader") $ txt "Knowledge Documents"
      , fill ' '
      , withAttr (attrName "dim") $ txt "[j/k ↑↓ select, Enter view]"
      ]
  , txt ""
  ] ++ if null children
       then [txt "  No knowledge documents yet."
            , withAttr (attrName "dim") $ txt "  Run 'wisp librarian <project-tag>' to generate them."
            ]
       else zipWith (renderKnowledgeDocItem mNow (ks ^. ksDocSelected)) [0..] children

-- | Render a knowledge document as a selectable list item
renderKnowledgeDocItem :: Maybe UTCTime -> Int -> Int -> Document -> Widget Name
renderKnowledgeDocItem mNow selected idx doc =
  let isSelected = idx == selected
      marker = if isSelected then "▸ " else "  "
      kind = extractField "kind" (documentData doc)
      kindLabel = case kind of
        "product_research" -> "Product Research"
        "roadmap" -> "Roadmap"
        "architecture" -> "Architecture"
        "activity_log" -> "Activity Log"
        _ -> kind
      updatedAt = documentCreatedAt doc  -- Use created_at as proxy for now
      updatedText = case mNow of
        Just now -> "updated " <> relativeTime now updatedAt
        Nothing -> ""
      attr = if isSelected then attrName "selected" else attrName ""
  in withAttr attr $ hBox
    [ txt marker
    , txt $ T.justifyLeft 25 ' ' kindLabel
    , fill ' '
    , withAttr (attrName "dim") $ txt updatedText
    ]

--------------------------------------------------------------------------------
-- Knowledge Document Expanded View
--------------------------------------------------------------------------------

knowledgeDocExpandedView :: Maybe UTCTime -> KnowledgeState -> Document -> Int -> Widget Name
knowledgeDocExpandedView _mNow _ks _parentDoc docIdx =
  let children = _ks ^. ksProjectChildren
      mDoc = if docIdx < length children then Just (children !! docIdx) else Nothing
  in case mDoc of
    Nothing -> txt "Document not found"
    Just doc ->
      let kind = extractField "kind" (documentData doc)
          kindLabel = case kind of
            "product_research" -> "Product Research"
            "roadmap" -> "Roadmap"
            "architecture" -> "Architecture"
            "activity_log" -> "Activity Log"
            _ -> kind
          content = renderKnowledgeContent kind (documentData doc)
      in vBox
        [ withAttr (attrName "dim") $ txt "[Esc to return to project]"
        , txt ""
        , hBox
            [ txt "┌─ "
            , withAttr (attrName "mdHeader") $ txt kindLabel
            , txt " "
            , txt $ T.replicate 50 "─"
            ]
        , txt "│"
        , viewport KnowledgeDetail Vertical $ vBox $
            map (hBox . (txt "│  " :) . (:[])) content
        , txt "│"
        , txt $ "└" <> T.replicate 65 "─"
        ]

-- | Render content based on knowledge document kind
renderKnowledgeContent :: Text -> Value -> [Widget Name]
renderKnowledgeContent "product_research" (Object obj) =
  let vision = extractFromObj "vision" obj
      valueProp = extractFromObj "value_proposition" obj
  in [ renderLabeledField "Vision" vision
     , renderLabeledField "Value Proposition" valueProp
     ]
renderKnowledgeContent "roadmap" (Object obj) =
  let timelineNotes = extractFromObj "timeline_notes" obj
  in [ renderLabeledField "Timeline" timelineNotes
     ]
renderKnowledgeContent "architecture" (Object obj) =
  let usersPersonas = extractFromObj "users_personas" obj
      testing = extractFromObj "testing" obj
      codeStructure = extractFromObj "code_structure" obj
      dataStructure = extractFromObj "data_structure" obj
      infrastructure = extractFromObj "infrastructure" obj
  in [ renderLabeledField "Users/Personas" usersPersonas
     , renderLabeledField "Testing" testing
     , renderLabeledField "Code Structure" codeStructure
     , renderLabeledField "Data Structure" dataStructure
     , renderLabeledField "Infrastructure" infrastructure
     ]
renderKnowledgeContent "activity_log" (Object obj) =
  let period = extractFromObj "period" obj
      summary = extractFromObj "summary" obj
  in [ renderLabeledField "Period" period
     , renderLabeledField "Summary" summary
     ]
renderKnowledgeContent _ _ = [txt "(unknown format)"]

-- | Render a labeled field with word wrapping
renderLabeledField :: Text -> Text -> Widget Name
renderLabeledField label value
  | T.null value = emptyWidget
  | otherwise = vBox
      [ txt ""
      , withAttr (attrName "mdBold") $ txt $ label <> ":"
      , txtWrap value
      ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

extractField :: Text -> Value -> Text
extractField key (Object obj) = case KM.lookup (fromString $ T.unpack key) obj of
  Just (String s) -> s
  _ -> ""
extractField _ _ = ""

extractNumber :: Text -> Value -> Int
extractNumber key (Object obj) = case KM.lookup (fromString $ T.unpack key) obj of
  Just (Number n) -> round n
  _ -> 0
extractNumber _ _ = 0

extractArray :: Text -> Value -> [Text]
extractArray key (Object obj) = case KM.lookup (fromString $ T.unpack key) obj of
  Just (Array arr) -> [s | String s <- toList arr]
  _ -> []
extractArray _ _ = []

extractFromObj :: Text -> KM.KeyMap Value -> Text
extractFromObj key obj = case KM.lookup (fromString $ T.unpack key) obj of
  Just (String s) -> s
  _ -> ""

currentDocs' :: KnowledgeState -> [Document]
currentDocs' ks = case ks ^. ksCurrentTab of
  ProjectsTab -> ks ^. ksProjects
  NotesTab -> ks ^. ksNotes
  PrefsTab -> ks ^. ksPrefs

--------------------------------------------------------------------------------
-- Event Handling
--------------------------------------------------------------------------------

-- | Handle knowledge-specific events
handleKnowledgeEvent :: V.Event -> EventM Name AppState ()
handleKnowledgeEvent evt = do
  _ <- handleKnowledgeEventWithAction evt
  pure ()

-- | Handle knowledge-specific events, returning an action
handleKnowledgeEventWithAction :: V.Event -> EventM Name AppState KnowledgeAction
handleKnowledgeEventWithAction evt = do
  s <- get
  case s ^. knowledgeState . ksExpanded of
    Just _ -> case s ^. knowledgeState . ksDocExpanded of
      Just _ -> handleDocExpandedEvent evt
      Nothing -> handleProjectDetailEvent evt
    Nothing -> handleListEventWithAction evt

-- | Events in the main list view
handleListEventWithAction :: V.Event -> EventM Name AppState KnowledgeAction
handleListEventWithAction (V.EvKey (V.KChar '1') []) = do
  modify $ knowledgeState . ksCurrentTab .~ ProjectsTab
  pure KnowledgeNoAction
handleListEventWithAction (V.EvKey (V.KChar '2') []) = do
  modify $ knowledgeState . ksCurrentTab .~ NotesTab
  pure KnowledgeNoAction
handleListEventWithAction (V.EvKey (V.KChar '3') []) = do
  modify $ knowledgeState . ksCurrentTab .~ PrefsTab
  pure KnowledgeNoAction
handleListEventWithAction (V.EvKey (V.KChar 'j') []) = listMoveDown
handleListEventWithAction (V.EvKey V.KDown []) = listMoveDown
handleListEventWithAction (V.EvKey (V.KChar 'k') []) = listMoveUp
handleListEventWithAction (V.EvKey V.KUp []) = listMoveUp
handleListEventWithAction (V.EvKey V.KEnter []) = expandProjectWithAction
handleListEventWithAction (V.EvKey (V.KChar 'l') []) = expandProjectWithAction
handleListEventWithAction evt = do
  _ <- handleViewportScroll KnowledgeList evt
  pure KnowledgeNoAction

listMoveDown :: EventM Name AppState KnowledgeAction
listMoveDown = do
  s <- get
  let docs = currentDocs' (s ^. knowledgeState)
      maxIdx = length docs - 1
  modify $ knowledgeState . ksSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
  pure KnowledgeNoAction

listMoveUp :: EventM Name AppState KnowledgeAction
listMoveUp = do
  modify $ knowledgeState . ksSelected %~ (\i -> max 0 (i - 1))
  pure KnowledgeNoAction

-- | Expand a project and return action to load children if it's a project
expandProjectWithAction :: EventM Name AppState KnowledgeAction
expandProjectWithAction = do
  s <- get
  let idx = s ^. knowledgeState . ksSelected
      docs = currentDocs' (s ^. knowledgeState)
      tab = s ^. knowledgeState . ksCurrentTab
  modify $ knowledgeState . ksExpanded .~ Just idx
  modify $ knowledgeState . ksProjectChildren .~ []
  modify $ knowledgeState . ksDocSelected .~ 0
  modify $ knowledgeState . ksDocExpanded .~ Nothing
  if tab == ProjectsTab && idx < length docs
    then pure $ LoadProjectChildren (documentId (docs !! idx))
    else pure KnowledgeNoAction

-- | Events in project detail view (card + doc list)
handleProjectDetailEvent :: V.Event -> EventM Name AppState KnowledgeAction
handleProjectDetailEvent (V.EvKey V.KEsc []) = do
  modify $ knowledgeState . ksExpanded .~ Nothing
  modify $ knowledgeState . ksDocSelected .~ 0
  pure KnowledgeNoAction
handleProjectDetailEvent (V.EvKey (V.KChar 'h') []) = do
  modify $ knowledgeState . ksExpanded .~ Nothing
  modify $ knowledgeState . ksDocSelected .~ 0
  pure KnowledgeNoAction
handleProjectDetailEvent (V.EvKey (V.KChar 'a') []) = do
  -- View activities for this project
  s <- get
  let idx = s ^. knowledgeState . ksSelected
      docs = s ^. knowledgeState . ksProjects
  if idx < length docs
    then do
      let tags = documentTags (docs !! idx)
      case tags of
        (tag:_) -> pure $ ViewActivitiesForTag tag
        [] -> pure KnowledgeNoAction
    else pure KnowledgeNoAction
handleProjectDetailEvent (V.EvKey (V.KChar 'j') []) = docMoveDown
handleProjectDetailEvent (V.EvKey V.KDown []) = docMoveDown
handleProjectDetailEvent (V.EvKey (V.KChar 'k') []) = docMoveUp
handleProjectDetailEvent (V.EvKey V.KUp []) = docMoveUp
handleProjectDetailEvent (V.EvKey V.KEnter []) = expandDocWithAction
handleProjectDetailEvent (V.EvKey (V.KChar 'l') []) = expandDocWithAction
handleProjectDetailEvent evt = do
  _ <- handleViewportScroll KnowledgeDetail evt
  pure KnowledgeNoAction

docMoveDown :: EventM Name AppState KnowledgeAction
docMoveDown = do
  s <- get
  let children = s ^. knowledgeState . ksProjectChildren
      maxIdx = length children - 1
  modify $ knowledgeState . ksDocSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
  pure KnowledgeNoAction

docMoveUp :: EventM Name AppState KnowledgeAction
docMoveUp = do
  modify $ knowledgeState . ksDocSelected %~ (\i -> max 0 (i - 1))
  pure KnowledgeNoAction

expandDocWithAction :: EventM Name AppState KnowledgeAction
expandDocWithAction = do
  s <- get
  let docIdx = s ^. knowledgeState . ksDocSelected
      children = s ^. knowledgeState . ksProjectChildren
  if docIdx < length children
    then do
      modify $ knowledgeState . ksDocExpanded .~ Just docIdx
      pure KnowledgeNoAction
    else pure KnowledgeNoAction

-- | Events in expanded document view
handleDocExpandedEvent :: V.Event -> EventM Name AppState KnowledgeAction
handleDocExpandedEvent (V.EvKey V.KEsc []) = do
  modify $ knowledgeState . ksDocExpanded .~ Nothing
  pure KnowledgeNoAction
handleDocExpandedEvent (V.EvKey (V.KChar 'h') []) = do
  modify $ knowledgeState . ksDocExpanded .~ Nothing
  pure KnowledgeNoAction
handleDocExpandedEvent evt = do
  _ <- handleViewportScroll KnowledgeDetail evt
  pure KnowledgeNoAction
