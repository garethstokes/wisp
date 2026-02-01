module Infra.Google.Gmail
  ( GmailMessage(..)
  , GmailMessageList(..)
  , GmailMessageRef(..)
  , GmailPayload(..)
  , GmailHeader(..)
  , GmailBody(..)
  , GmailHistoryList(..)
  , GmailHistory(..)
  , GmailHistoryMessage(..)
  , listMessages
  , getMessage
  , listHistory
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), withObject, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusIsSuccessful, statusCode)

-- | A full Gmail message with all details
data GmailMessage = GmailMessage
  { gmailId :: Text
  , gmailThreadId :: Text
  , gmailLabelIds :: Maybe [Text]
  , gmailSnippet :: Maybe Text
  , gmailPayload :: Maybe GmailPayload
  , gmailInternalDate :: Maybe Text
  , gmailHistoryId :: Maybe Text  -- Used for incremental sync
  , gmailRaw :: Maybe Text
  } deriving (Show, Eq)

instance ToJSON GmailMessage where
  toJSON msg = object
    [ "id" .= gmailId msg
    , "threadId" .= gmailThreadId msg
    , "labelIds" .= gmailLabelIds msg
    , "snippet" .= gmailSnippet msg
    , "payload" .= gmailPayload msg
    , "internalDate" .= gmailInternalDate msg
    , "historyId" .= gmailHistoryId msg
    , "raw" .= gmailRaw msg
    ]

instance FromJSON GmailMessage where
  parseJSON = withObject "GmailMessage" $ \v -> GmailMessage
    <$> v .: "id"
    <*> v .: "threadId"
    <*> v .:? "labelIds"
    <*> v .:? "snippet"
    <*> v .:? "payload"
    <*> v .:? "internalDate"
    <*> v .:? "historyId"
    <*> v .:? "raw"

-- | Message payload containing headers and body
data GmailPayload = GmailPayload
  { payloadHeaders :: Maybe [GmailHeader]
  , payloadMimeType :: Maybe Text
  , payloadBody :: Maybe GmailBody
  } deriving (Show, Eq)

instance ToJSON GmailPayload where
  toJSON p = object
    [ "headers" .= payloadHeaders p
    , "mimeType" .= payloadMimeType p
    , "body" .= payloadBody p
    ]

instance FromJSON GmailPayload where
  parseJSON = withObject "GmailPayload" $ \v -> GmailPayload
    <$> v .:? "headers"
    <*> v .:? "mimeType"
    <*> v .:? "body"

-- | Email header (name/value pair)
data GmailHeader = GmailHeader
  { headerName :: Text
  , headerValue :: Text
  } deriving (Show, Eq)

instance ToJSON GmailHeader where
  toJSON h = object
    [ "name" .= headerName h
    , "value" .= headerValue h
    ]

instance FromJSON GmailHeader where
  parseJSON = withObject "GmailHeader" $ \v -> GmailHeader
    <$> v .: "name"
    <*> v .: "value"

-- | Message body with size and optional data
data GmailBody = GmailBody
  { bodySize :: Int
  , bodyData :: Maybe Text
  } deriving (Show, Eq)

instance ToJSON GmailBody where
  toJSON b = object
    [ "size" .= bodySize b
    , "data" .= bodyData b
    ]

instance FromJSON GmailBody where
  parseJSON = withObject "GmailBody" $ \v -> GmailBody
    <$> v .: "size"
    <*> v .:? "data"

-- | Response from messages.list endpoint
data GmailMessageList = GmailMessageList
  { messages :: Maybe [GmailMessageRef]
  , nextPageToken :: Maybe Text
  , resultSizeEstimate :: Maybe Int
  } deriving (Show, Eq)

instance FromJSON GmailMessageList where
  parseJSON = withObject "GmailMessageList" $ \v -> GmailMessageList
    <$> v .:? "messages"
    <*> v .:? "nextPageToken"
    <*> v .:? "resultSizeEstimate"

-- | Reference to a message (id and threadId only)
data GmailMessageRef = GmailMessageRef
  { refId :: Text
  , refThreadId :: Text
  } deriving (Show, Eq)

instance FromJSON GmailMessageRef where
  parseJSON = withObject "GmailMessageRef" $ \v -> GmailMessageRef
    <$> v .: "id"
    <*> v .: "threadId"

-- | Response from history.list endpoint
data GmailHistoryList = GmailHistoryList
  { history :: Maybe [GmailHistory]
  , historyNextPageToken :: Maybe Text
  , historyId :: Maybe Text
  } deriving (Show, Eq)

instance FromJSON GmailHistoryList where
  parseJSON = withObject "GmailHistoryList" $ \v -> GmailHistoryList
    <$> v .:? "history"
    <*> v .:? "nextPageToken"
    <*> v .:? "historyId"

-- | A single history record
data GmailHistory = GmailHistory
  { historyRecordId :: Text
  , historyMessagesAdded :: Maybe [GmailHistoryMessage]
  } deriving (Show, Eq)

instance FromJSON GmailHistory where
  parseJSON = withObject "GmailHistory" $ \v -> GmailHistory
    <$> v .: "id"
    <*> v .:? "messagesAdded"

-- | A message reference within a history record
data GmailHistoryMessage = GmailHistoryMessage
  { historyMessage :: GmailMessageRef
  } deriving (Show, Eq)

instance FromJSON GmailHistoryMessage where
  parseJSON = withObject "GmailHistoryMessage" $ \v -> GmailHistoryMessage
    <$> v .: "message"

-- | Base URL for Gmail API
baseUrl :: String
baseUrl = "https://gmail.googleapis.com/gmail/v1/users/me"

-- | List messages in the user's mailbox
listMessages :: Text -> Maybe Text -> IO (Either Text GmailMessageList)
listMessages accessToken pageToken = do
  manager <- newManager tlsManagerSettings
  let url = baseUrl <> "/messages" <> maybe "" (\t -> "?pageToken=" <> T.unpack t) pageToken
  initialReq <- parseRequest url
  let req = initialReq
        { requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 accessToken)]
        }
  result <- try $ httpLbs req manager
  pure $ case result of
    Left (e :: SomeException) -> Left $ "HTTP error: " <> T.pack (show e)
    Right response
      | statusIsSuccessful (responseStatus response) ->
          case Aeson.decode (responseBody response) of
            Just msgList -> Right msgList
            Nothing -> Left $ "Failed to parse messages list: " <> T.pack (show $ responseBody response)
      | otherwise ->
          Left $ "Gmail API error " <> T.pack (show $ statusCode $ responseStatus response)
               <> ": " <> T.pack (show $ responseBody response)

-- | Get a single message by ID
getMessage :: Text -> Text -> IO (Either Text GmailMessage)
getMessage accessToken messageId = do
  manager <- newManager tlsManagerSettings
  let url = baseUrl <> "/messages/" <> T.unpack messageId
  initialReq <- parseRequest url
  let req = initialReq
        { requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 accessToken)]
        }
  result <- try $ httpLbs req manager
  pure $ case result of
    Left (e :: SomeException) -> Left $ "HTTP error: " <> T.pack (show e)
    Right response
      | statusIsSuccessful (responseStatus response) ->
          case Aeson.decode (responseBody response) of
            Just msg -> Right msg
            Nothing -> Left $ "Failed to parse message: " <> T.pack (show $ responseBody response)
      | otherwise ->
          Left $ "Gmail API error " <> T.pack (show $ statusCode $ responseStatus response)
               <> ": " <> T.pack (show $ responseBody response)

-- | List history starting from a given historyId
listHistory :: Text -> Text -> Maybe Text -> IO (Either Text GmailHistoryList)
listHistory accessToken startHistoryId pageToken = do
  manager <- newManager tlsManagerSettings
  let params = "?startHistoryId=" <> T.unpack startHistoryId
             <> maybe "" (\t -> "&pageToken=" <> T.unpack t) pageToken
  let url = baseUrl <> "/history" <> params
  initialReq <- parseRequest url
  let req = initialReq
        { requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 accessToken)]
        }
  result <- try $ httpLbs req manager
  pure $ case result of
    Left (e :: SomeException) -> Left $ "HTTP error: " <> T.pack (show e)
    Right response
      | statusIsSuccessful (responseStatus response) ->
          case Aeson.decode (responseBody response) of
            Just histList -> Right histList
            Nothing -> Left $ "Failed to parse history list: " <> T.pack (show $ responseBody response)
      | otherwise ->
          Left $ "Gmail API error " <> T.pack (show $ statusCode $ responseStatus response)
               <> ": " <> T.pack (show $ responseBody response)
