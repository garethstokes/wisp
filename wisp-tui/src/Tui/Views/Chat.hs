module Tui.Views.Chat
  ( chatWidget
  , handleChatEvent
  , sendChatMessage
  ) where

import Brick
import Brick.BChan (BChan, writeBChan)
import Brick.Widgets.Border (hBorder)
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Data.Aeson (Value, object, (.=))
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((^.), (%~))

import Tui.Types
import Wisp.Client (ClientConfig, ClientError(..))
import Wisp.Client.SSE (streamChat, ChatRequest(..), ChatEvent(..))

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
  -- Enter is handled in Main with access to BChan
  pure ()
handleChatEvent (V.EvKey V.KBS []) = do
  modify $ chatState . csInputBuffer %~ (\t -> if T.null t then t else T.init t)
handleChatEvent (V.EvKey (V.KChar c) []) = do
  modify $ chatState . csInputBuffer %~ (<> T.singleton c)
handleChatEvent _ = pure ()

-- | Send a chat message and stream responses
sendChatMessage :: ClientConfig -> Text -> Text -> [ChatMessage] -> BChan AppEvent -> IO ()
sendChatMessage cfg agent session messages chan = void $ async $ do
  let req = ChatRequest
        { chatAgent = agent
        , chatMessages = map messageToJson messages
        , chatSession = Just session
        , chatTimezone = Nothing
        }
  result <- streamChat cfg req $ \evt ->
    writeBChan chan (ChatEventReceived evt)
  case result of
    Left err ->
      writeBChan chan (ChatEventReceived (ErrorEvent (renderClientError err) "client_error"))
    Right () -> pure ()
  where
    messageToJson :: ChatMessage -> Value
    messageToJson m = object
      [ "role" .= normalizeRole (cmRole m)
      , "content" .= cmContent m
      ]

    normalizeRole :: Text -> Text
    normalizeRole "You" = "user"
    normalizeRole "Assistant" = "assistant"
    normalizeRole other = other

    renderClientError :: ClientError -> Text
    renderClientError = \case
      HttpError msg -> "HTTP error: " <> msg
      ParseError msg -> "Parse error: " <> msg
      ServerError code msg -> "Server error (" <> T.pack (show code) <> "): " <> msg
