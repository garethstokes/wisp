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
import qualified Data.Aeson.KeyMap as KM
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Tui.Widgets.Scroll (handleViewportScroll)
import Wisp.Client (Document(..), DocumentType(..))

-- | Actions that can be triggered from knowledge view
data KnowledgeAction
  = LoadProjectChildren Text  -- Load children for project with given ID
  | KnowledgeNoAction
  deriving (Show, Eq)

-- | Knowledge view widget
knowledgeWidget :: KnowledgeState -> Widget Name
knowledgeWidget ks = case ks ^. ksExpanded of
  Nothing -> listView ks
  Just idx -> detailView ks idx

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

detailView :: KnowledgeState -> Int -> Widget Name
detailView ks idx =
  let docs = currentDocs' ks
      mDoc = if idx < length docs then Just (docs !! idx) else Nothing
  in case mDoc of
    Nothing -> txt "Document not found"
    Just doc -> vBox
      [ txt "[Esc/h to return]"
      , txt ""
      , viewport KnowledgeDetail Vertical $ documentDetailWidget doc (ks ^. ksProjectChildren)
      ]

documentDetailWidget :: Document -> [Document] -> Widget Name
documentDetailWidget doc children = vBox $
  [ txt $ "ID: " <> documentId doc
  , txt ""
  , case documentData doc of
      Object obj -> vBox $ map renderField (KM.toList obj)
      _ -> txt "(no data)"
  , txt ""
  , txt $ "Tags: " <> T.intercalate ", " (documentTags doc)
  , txt $ "Active: " <> if documentActive doc then "yes" else "no"
  , txt $ "Created: " <> T.pack (show (documentCreatedAt doc))
  , txt $ "Last activity: " <> maybe "none" (T.pack . show) (documentLastActivityAt doc)
  ] ++ projectChildrenSection doc children

-- | Render project children section (knowledge documents)
projectChildrenSection :: Document -> [Document] -> [Widget Name]
projectChildrenSection doc children
  | documentType doc /= ProjectDoc = []
  | null children =
      [ txt ""
      , withAttr (attrName "mdHeader") $ txt "Knowledge Documents"
      , txt "  No knowledge documents yet."
      , txt "  Run 'wisp-cli librarian run' to generate them."
      ]
  | otherwise =
      [ txt ""
      , withAttr (attrName "mdHeader") $ txt "Knowledge Documents"
      ] ++ map renderKnowledgeChild children

-- | Render a knowledge document child with full content
renderKnowledgeChild :: Document -> Widget Name
renderKnowledgeChild doc =
  let kind = extractField "kind" (documentData doc)
      kindLabel = case kind of
        "product_research" -> "Product Research"
        "roadmap" -> "Roadmap"
        "architecture" -> "Architecture"
        "activity_log" -> "Activity Log"
        _ -> kind
      content = renderKnowledgeContent kind (documentData doc)
  in vBox $
    [ txt ""
    , withAttr (attrName "mdBold") $ txt $ "  " <> kindLabel
    ] ++ map (\w -> padLeft (Pad 4) w) content

-- | Render content based on knowledge document kind
renderKnowledgeContent :: T.Text -> Value -> [Widget Name]
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

-- | Extract text from object
extractFromObj :: T.Text -> KM.KeyMap Value -> T.Text
extractFromObj key obj = case KM.lookup (fromString $ T.unpack key) obj of
  Just (String s) -> s
  _ -> ""

-- | Render a labeled field with word wrapping
renderLabeledField :: T.Text -> T.Text -> Widget Name
renderLabeledField label value
  | T.null value = emptyWidget
  | otherwise = vBox
      [ withAttr (attrName "dim") $ txt $ label <> ":"
      , txtWrap value
      ]

renderField :: (KM.Key, Value) -> Widget Name
renderField (key, val) =
  let k = T.pack $ show key
  in case val of
    String s -> txt $ k <> ": " <> s
    Array arr ->
      -- Render arrays with each element on a new line
      let items = toList arr
      in vBox $ txt (k <> ":") : map renderArrayItem items
    _ -> txt $ k <> ": " <> T.pack (show val)

renderArrayItem :: Value -> Widget Name
renderArrayItem val = case val of
  String s -> txt $ "  - " <> s
  _ -> txt $ "  - " <> T.pack (show val)

extractField :: Text -> Value -> Text
extractField key (Object obj) = case KM.lookup (fromString $ T.unpack key) obj of
  Just (String s) -> s
  _ -> ""
extractField _ _ = ""

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
    Just _ -> do
      handleDetailEvent evt
      pure KnowledgeNoAction
    Nothing -> handleListEventWithAction evt

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
  modify $ knowledgeState . ksProjectChildren .~ []  -- Clear children
  -- If we're in projects tab, return action to load children
  if tab == ProjectsTab && idx < length docs
    then pure $ LoadProjectChildren (documentId (docs !! idx))
    else pure KnowledgeNoAction

handleDetailEvent :: V.Event -> EventM Name AppState ()
handleDetailEvent (V.EvKey V.KEsc []) =
  modify $ knowledgeState . ksExpanded .~ Nothing
handleDetailEvent (V.EvKey (V.KChar 'h') []) =
  modify $ knowledgeState . ksExpanded .~ Nothing
handleDetailEvent evt = do
  handled <- handleViewportScroll KnowledgeDetail evt
  if handled then pure () else pure ()

currentDocs' :: KnowledgeState -> [Document]
currentDocs' ks = case ks ^. ksCurrentTab of
  ProjectsTab -> ks ^. ksProjects
  NotesTab -> ks ^. ksNotes
  PrefsTab -> ks ^. ksPrefs
