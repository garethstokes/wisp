module Tui.Views.Knowledge
  ( knowledgeWidget
  , handleKnowledgeEvent
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
import Wisp.Client (Document(..))

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
      , viewport KnowledgeDetail Vertical $ documentDetailWidget doc
      ]

documentDetailWidget :: Document -> Widget Name
documentDetailWidget doc = vBox
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
  s <- get
  case s ^. knowledgeState . ksExpanded of
    Just _ -> handleDetailEvent evt
    Nothing -> handleListEvent evt

handleListEvent :: V.Event -> EventM Name AppState ()
handleListEvent (V.EvKey (V.KChar '1') []) =
  modify $ knowledgeState . ksCurrentTab .~ ProjectsTab
handleListEvent (V.EvKey (V.KChar '2') []) =
  modify $ knowledgeState . ksCurrentTab .~ NotesTab
handleListEvent (V.EvKey (V.KChar '3') []) =
  modify $ knowledgeState . ksCurrentTab .~ PrefsTab
handleListEvent (V.EvKey (V.KChar 'j') []) = do
  s <- get
  let docs = currentDocs' (s ^. knowledgeState)
      maxIdx = length docs - 1
  modify $ knowledgeState . ksSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
handleListEvent (V.EvKey (V.KChar 'k') []) =
  modify $ knowledgeState . ksSelected %~ (\i -> max 0 (i - 1))
handleListEvent (V.EvKey V.KEnter []) = do
  s <- get
  let idx = s ^. knowledgeState . ksSelected
  modify $ knowledgeState . ksExpanded .~ Just idx
handleListEvent (V.EvKey (V.KChar 'l') []) = do
  s <- get
  let idx = s ^. knowledgeState . ksSelected
  modify $ knowledgeState . ksExpanded .~ Just idx
handleListEvent evt = do
  handled <- handleViewportScroll KnowledgeList evt
  if handled then pure () else pure ()

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
