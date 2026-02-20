module Tui.Views.Documents
  ( documentsWidget
  , handleDocumentsEvent
  ) where

import Brick
import qualified Graphics.Vty as V
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Wisp.Client (Document(..))

-- | Documents view widget
documentsWidget :: DocumentsState -> Widget Name
documentsWidget ds = vBox
  [ tabBar ds
  , documentContent ds
  ]

tabBar :: DocumentsState -> Widget Name
tabBar ds = vLimit 1 $ hBox
  [ renderTab ds ProjectsTab "1:Projects"
  , txt "  "
  , renderTab ds NotesTab "2:Notes"
  , txt "  "
  , renderTab ds PrefsTab "3:Prefs"
  , fill ' '
  ]

renderTab :: DocumentsState -> DocumentTab -> Text -> Widget Name
renderTab ds tab label
  | ds ^. dsCurrentTab == tab = withAttr (attrName "selectedTab") $ txt $ "[" <> label <> "]"
  | otherwise = txt $ " " <> label <> " "

documentContent :: DocumentsState -> Widget Name
documentContent ds = case ds ^. dsCurrentTab of
  ProjectsTab -> projectsContent ds
  NotesTab -> notesContent ds
  PrefsTab -> prefsContent ds

projectsContent :: DocumentsState -> Widget Name
projectsContent ds = vBox $
  [ padBottom (Pad 1) $ hBox
    [ txt "  Name                    Type        Last Activity"
    ]
  , txt "  ------"
  ] ++ if null (ds ^. dsProjects)
       then [padAll 1 $ txt "No projects."]
       else zipWith (renderProject (ds ^. dsSelected)) [0..] (ds ^. dsProjects)

renderProject :: Int -> Int -> Document -> Widget Name
renderProject selected idx doc =
  let isSelected = idx == selected
      marker = if isSelected then "> " else "  "
      name = extractField "name" (documentData doc)
      ptype = extractField "type" (documentData doc)
  in hBox
    [ txt marker
    , txt $ T.justifyLeft 24 ' ' name
    , txt $ T.justifyLeft 12 ' ' ptype
    , txt $ maybe "-" (T.take 15 . T.pack . show) (documentLastActivityAt doc)
    ]

notesContent :: DocumentsState -> Widget Name
notesContent ds = vBox $
  [ padBottom (Pad 1) $ txt "  Title                           Tags"
  , txt "  ------"
  ] ++ if null (ds ^. dsNotes)
       then [padAll 1 $ txt "No notes."]
       else zipWith (renderNote (ds ^. dsSelected)) [0..] (ds ^. dsNotes)

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

prefsContent :: DocumentsState -> Widget Name
prefsContent ds = vBox $
  [ padBottom (Pad 1) $ txt "  Key                     Value               Context"
  , txt "  ------"
  ] ++ if null (ds ^. dsPrefs)
       then [padAll 1 $ txt "No preferences."]
       else zipWith (renderPref (ds ^. dsSelected)) [0..] (ds ^. dsPrefs)

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

extractField :: Text -> Value -> Text
extractField key (Object obj) = case KM.lookup (fromString $ T.unpack key) obj of
  Just (String s) -> s
  _ -> ""
extractField _ _ = ""

-- | Handle documents-specific events
handleDocumentsEvent :: V.Event -> EventM Name AppState ()
handleDocumentsEvent (V.EvKey (V.KChar '1') []) =
  modify $ documentsState . dsCurrentTab .~ ProjectsTab
handleDocumentsEvent (V.EvKey (V.KChar '2') []) =
  modify $ documentsState . dsCurrentTab .~ NotesTab
handleDocumentsEvent (V.EvKey (V.KChar '3') []) =
  modify $ documentsState . dsCurrentTab .~ PrefsTab
handleDocumentsEvent (V.EvKey (V.KChar 'j') []) = do
  s <- get
  let docs = currentDocs s
      maxIdx = length docs - 1
  modify $ documentsState . dsSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
handleDocumentsEvent (V.EvKey (V.KChar 'k') []) =
  modify $ documentsState . dsSelected %~ (\i -> max 0 (i - 1))
handleDocumentsEvent _ = pure ()

currentDocs :: AppState -> [Document]
currentDocs s = case s ^. documentsState . dsCurrentTab of
  ProjectsTab -> s ^. documentsState . dsProjects
  NotesTab -> s ^. documentsState . dsNotes
  PrefsTab -> s ^. documentsState . dsPrefs
