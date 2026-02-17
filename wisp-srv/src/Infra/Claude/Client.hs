module Infra.Claude.Client
  ( ClaudeResponse(..)
  , ClaudeContent(..)
  , ClaudeUsage(..)
  , callClaude
  , callClaudeWithSystem
  , responseText
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?), object, (.=), encode)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import qualified Data.Text as T
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

data ClaudeUsage = ClaudeUsage
  { usageInputTokens :: Int
  , usageOutputTokens :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON ClaudeUsage where
  parseJSON = withObject "ClaudeUsage" $ \v -> ClaudeUsage
    <$> v .: "input_tokens"
    <*> v .: "output_tokens"

data ClaudeResponse = ClaudeResponse
  { responseContent :: [ClaudeContent]
  , responseModel :: Text
  , responseStopReason :: Maybe Text
  , responseUsage :: Maybe ClaudeUsage
  } deriving (Show, Eq, Generic)

instance FromJSON ClaudeResponse where
  parseJSON = withObject "ClaudeResponse" $ \v -> ClaudeResponse
    <$> v .: "content"
    <*> v .: "model"
    <*> v .:? "stop_reason"
    <*> v .:? "usage"

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

-- Call Claude API with system prompt, returning text and usage data
callClaudeWithSystem :: Text -> Text -> Text -> Text -> IO (Either Text (Text, Maybe Int, Maybe Int))
callClaudeWithSystem apiKey model systemPrompt userMessage = do
  result <- try $ do
    manager <- newManager tlsManagerSettings
    let url = "https://api.anthropic.com/v1/messages"
    initReq <- parseRequest url
    let reqBody = object
          [ "model" .= model
          , "max_tokens" .= (4096 :: Int)
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
    httpLbs req manager
  case result of
    Left (e :: SomeException) -> pure $ Left $ "Network error: " <> T.pack (show e)
    Right response -> do
      let status = statusCode (responseStatus response)
      if status == 200
        then case decodeResponse (responseBody response) of
          Just result' -> pure $ Right result'
          Nothing -> pure $ Left "Failed to parse Claude response"
        else pure $ Left $ "Claude API error: " <> decodeUtf8 (toStrict $ responseBody response)
  where
    decodeResponse body = do
      resp <- Aeson.decode body :: Maybe ClaudeResponse
      txt <- responseText resp
      let inputTokens = usageInputTokens <$> responseUsage resp
      let outputTokens = usageOutputTokens <$> responseUsage resp
      pure (txt, inputTokens, outputTokens)
