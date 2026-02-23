module Wisp.Client.SSE
  ( ChatEvent(..)
  , ChatRequest(..)
  , streamChat
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (ToJSON(..), Value, encode, object, (.=))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

import Wisp.Client.Types

-- | Request to send a chat message
data ChatRequest = ChatRequest
  { chatAgent :: Text
  , chatMessages :: [Value]
  , chatSession :: Maybe Text
  , chatTimezone :: Maybe Text
  } deriving (Show, Eq)

instance ToJSON ChatRequest where
  toJSON r =
    object $
      [ "agent" .= chatAgent r
      , "messages" .= chatMessages r
      ]
        <> maybe [] (\sid -> ["session" .= sid]) (chatSession r)
        <> maybe [] (\tz -> ["timezone" .= tz]) (chatTimezone r)

-- | Events received from SSE stream
data ChatEvent
  = ChunkEvent Text
  | AgentRunning              -- signals agent started processing
  | ToolCallStart Text  -- tool name
  | ToolCallResult Text Int  -- tool name, duration_ms
  | DoneEvent Text Int  -- session_id, token_count
  | ErrorEvent Text Text  -- message, code
  deriving (Show, Eq)

-- | Stream chat with callback for each event
streamChat
  :: ClientConfig
  -> ChatRequest
  -> (ChatEvent -> IO ())  -- Callback for each event
  -> IO (Either ClientError ())
streamChat cfg req callback = do
  -- Use longer timeout for SSE (5 minutes) since LLM calls can be slow
  let settings = defaultManagerSettings
        { managerResponseTimeout = responseTimeoutMicro (5 * 60 * 1000000)
        }
  manager <- newManager settings
  result <- try $ do
    initialReq <- parseRequest $ T.unpack (configBaseUrl cfg) <> "/api/chat/stream"
    let httpReq = initialReq
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Accept", "text/event-stream")
              ]
          , requestBody = RequestBodyLBS (encode req)
          }
    withResponse httpReq manager $ \response -> do
      let code = statusCode $ responseStatus response
      if code >= 200 && code < 300
        then do
          parseSSEStream (responseBody response) callback
          pure $ Right ()
        else pure $ Left $ ServerError code "SSE request failed"
  case result of
    Left (e :: SomeException) -> pure $ Left $ HttpError $ T.pack $ show e
    Right r -> pure r

-- | Parse SSE stream and call handler for each event
parseSSEStream :: BodyReader -> (ChatEvent -> IO ()) -> IO ()
parseSSEStream body callback = loop ""
  where
    loop :: BS.ByteString -> IO ()
    loop buffer = do
      chunk <- body
      if BS.null chunk
        then pure ()  -- Stream ended
        else do
          let newBuffer = buffer <> chunk
          let (events, remaining) = extractEvents newBuffer
          shouldStop <- processEvents events
          if shouldStop
            then pure ()
            else loop remaining

    -- Extract complete SSE events (delimited by double newline)
    extractEvents :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
    extractEvents bs =
      let parts = BS.breakSubstring "\n\n" bs
      in case parts of
        (event, rest)
          | BS.null rest -> ([], bs)  -- No complete event yet
          | otherwise ->
              let (moreEvents, remaining) = extractEvents (BS.drop 2 rest)
              in (event : moreEvents, remaining)

    -- Process events, return True if we should stop
    processEvents :: [BS.ByteString] -> IO Bool
    processEvents [] = pure False
    processEvents (e:es) = do
      case parseSSEEvent e of
        Just evt -> do
          callback evt
          case evt of
            DoneEvent _ _ -> pure True
            ErrorEvent _ _ -> pure True
            _ -> processEvents es
        Nothing -> processEvents es

-- | Parse a single SSE event block
parseSSEEvent :: BS.ByteString -> Maybe ChatEvent
parseSSEEvent bs =
  let txt = TE.decodeUtf8 bs
      lines' = T.lines txt
      eventLine = findLine "event:" lines'
      dataLine = findLine "data:" lines'
  in case (eventLine, dataLine) of
    (Just event, Just dataJson) -> parseEventData event dataJson
    _ -> Nothing
  where
    findLine prefix = fmap (T.strip . T.drop (T.length prefix)) . find (T.isPrefixOf prefix)
    find f = foldr (\x acc -> if f x then Just x else acc) Nothing

    parseEventData :: Text -> Text -> Maybe ChatEvent
    parseEventData event dataJson = case event of
      "chunk" -> parseChunk dataJson
      "agent_running" -> Just AgentRunning
      "tool_call_start" -> parseToolStart dataJson
      "tool_call_result" -> parseToolResult dataJson
      "done" -> parseDone dataJson
      "error" -> parseError dataJson
      _ -> Nothing

    parseChunk json = ChunkEvent <$> extractTextField "text" json
    parseToolStart json = ToolCallStart <$> extractTextField "tool" json
    parseToolResult json = do
      tool <- extractTextField "tool" json
      dur <- extractIntField "duration_ms" json
      pure $ ToolCallResult tool dur
    parseDone json = do
      sid <- extractTextField "session_id" json
      tokens <- extractIntField "token_count" json
      pure $ DoneEvent sid tokens
    parseError json = do
      msg <- extractTextField "message" json
      code <- extractTextField "code" json
      pure $ ErrorEvent msg code

    -- Simple JSON field extractors (avoiding full aeson parse for efficiency)
    extractTextField :: Text -> Text -> Maybe Text
    extractTextField field json =
      let pattern = "\"" <> field <> "\":\""
          after = T.breakOn pattern json
      in case after of
        (_, rest)
          | T.null rest -> Nothing
          | otherwise ->
              let valueStart = T.drop (T.length pattern) rest
                  (value, _) = T.breakOn "\"" valueStart
              in Just value

    extractIntField :: Text -> Text -> Maybe Int
    extractIntField field json =
      let pattern = "\"" <> field <> "\":"
          after = T.breakOn pattern json
      in case after of
        (_, rest)
          | T.null rest -> Nothing
          | otherwise ->
              let valueStart = T.drop (T.length pattern) rest
                  digits = T.takeWhile (\c -> c >= '0' && c <= '9') valueStart
              in if T.null digits then Nothing else Just (read $ T.unpack digits)
