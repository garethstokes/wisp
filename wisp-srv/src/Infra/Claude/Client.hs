module Infra.Claude.Client
  ( ClaudeResponse(..)
  , ClaudeContent(..)
  , callClaude
  , callClaudeWithSystem
  , responseText
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.:?), object, (.=), encode)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

data ClaudeContent = ClaudeContent
  { contentType :: Text
  , contentText :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON ClaudeContent where
  parseJSON = withObject "ClaudeContent" $ \v -> ClaudeContent
    <$> v .: "type"
    <*> v .:? "text"

data ClaudeResponse = ClaudeResponse
  { responseContent :: [ClaudeContent]
  , responseModel :: Text
  , responseStopReason :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON ClaudeResponse where
  parseJSON = withObject "ClaudeResponse" $ \v -> ClaudeResponse
    <$> v .: "content"
    <*> v .: "model"
    <*> v .:? "stop_reason"

-- Extract text from response
responseText :: ClaudeResponse -> Maybe Text
responseText resp = case responseContent resp of
  [] -> Nothing
  (c:_) -> contentText c

-- Call Claude API
callClaude :: Text -> Text -> Text -> IO (Either Text Text)
callClaude apiKey model prompt = do
  manager <- newManager tlsManagerSettings
  let url = "https://api.anthropic.com/v1/messages"
  initReq <- parseRequest url
  let reqBody = object
        [ "model" .= model
        , "max_tokens" .= (1024 :: Int)
        , "messages" .= [object ["role" .= ("user" :: Text), "content" .= prompt]]
        ]
  let req = initReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("x-api-key", encodeUtf8 apiKey)
            , ("anthropic-version", "2023-06-01")
            ]
        , requestBody = RequestBodyLBS (encode reqBody)
        }
  response <- httpLbs req manager
  let status = statusCode (responseStatus response)
  if status == 200
    then case decodeResponse (responseBody response) of
      Just txt -> pure $ Right txt
      Nothing -> pure $ Left "Failed to parse Claude response"
    else pure $ Left $ "Claude API error: " <> decodeUtf8 (toStrict $ responseBody response)
  where
    decodeResponse body = do
      resp <- Aeson.decode body :: Maybe ClaudeResponse
      responseText resp

-- Call Claude API with system prompt
callClaudeWithSystem :: Text -> Text -> Text -> Text -> IO (Either Text Text)
callClaudeWithSystem apiKey model systemPrompt userMessage = do
  manager <- newManager tlsManagerSettings
  let url = "https://api.anthropic.com/v1/messages"
  initReq <- parseRequest url
  let reqBody = object
        [ "model" .= model
        , "max_tokens" .= (2048 :: Int)
        , "system" .= systemPrompt
        , "messages" .= [object ["role" .= ("user" :: Text), "content" .= userMessage]]
        ]
  let req = initReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("x-api-key", encodeUtf8 apiKey)
            , ("anthropic-version", "2023-06-01")
            ]
        , requestBody = RequestBodyLBS (encode reqBody)
        }
  response <- httpLbs req manager
  let status = statusCode (responseStatus response)
  if status == 200
    then case decodeResponse (responseBody response) of
      Just txt -> pure $ Right txt
      Nothing -> pure $ Left "Failed to parse Claude response"
    else pure $ Left $ "Claude API error: " <> decodeUtf8 (toStrict $ responseBody response)
  where
    decodeResponse body = do
      resp <- Aeson.decode body :: Maybe ClaudeResponse
      responseText resp
