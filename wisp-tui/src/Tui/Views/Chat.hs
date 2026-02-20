module Tui.Views.Chat
  ( chatWidget
  , handleChatEvent
  ) where

import Brick
import Brick.Widgets.Border (hBorder)
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types

-- | Chat view widget
chatWidget :: ChatState -> Widget Name
chatWidget cs = vBox
  [ sessionHeader cs
  , messagesWidget cs
  , hBorder
  , inputWidget cs
  ]

sessionHeader :: ChatState -> Widget Name
sessionHeader cs = vLimit 1 $ hBox
  [ txt " session: "
  , txt (cs ^. csCurrentSession)
  , fill ' '
  ]

messagesWidget :: ChatState -> Widget Name
messagesWidget cs = viewport ChatHistory Vertical $ vBox $
  map renderMessage (cs ^. csMessages)
  ++ streamingIndicator cs

renderMessage :: ChatMessage -> Widget Name
renderMessage msg = padBottom (Pad 1) $ vBox
  [ withAttr (roleAttr $ cmRole msg) $ txt $ "[" <> cmRole msg <> "]"
  , padLeft (Pad 2) $ txtWrap (cmContent msg)
  ]

roleAttr :: Text -> AttrName
roleAttr "You" = attrName "userRole"
roleAttr _ = attrName "assistantRole"

streamingIndicator :: ChatState -> [Widget Name]
streamingIndicator cs
  | cs ^. csStreaming =
      [ txt $ cs ^. csStreamBuffer
      , withAttr (attrName "cursor") $ txt "▌"
      ]
  | otherwise = []

inputWidget :: ChatState -> Widget Name
inputWidget cs = vLimit 1 $ hBox
  [ txt "> "
  , txt (cs ^. csInputBuffer)
  , withAttr (attrName "cursor") $ txt "│"
  ]

-- | Handle chat-specific events
handleChatEvent :: V.Event -> EventM Name AppState ()
handleChatEvent (V.EvKey V.KEnter []) = do
  s <- get
  let input = s ^. chatState . csInputBuffer
  if T.null input
    then pure ()
    else do
      -- Clear input (message sending will be wired up in Task 14)
      modify $ chatState . csInputBuffer .~ ""
handleChatEvent (V.EvKey V.KBS []) = do
  modify $ chatState . csInputBuffer %~ (\t -> if T.null t then t else T.init t)
handleChatEvent (V.EvKey (V.KChar c) []) = do
  modify $ chatState . csInputBuffer %~ (<> T.singleton c)
handleChatEvent _ = pure ()
