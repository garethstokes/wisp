module Infra.GitHub.Events
  ( GitHubEvent(..)
  , EventsResponse(..)
  , listEvents
  , extractEventTitle
  , CommitInfo(..)
  , extractCommitsFromPayload
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.:), withObject, genericToJSON, defaultOptions)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Maybe (mapMaybe)
import Data.Foldable (toList)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI

data GitHubEvent = GitHubEvent
  { ghEventId :: Text
  , ghEventType :: Text
  , ghEventActor :: Text
  , ghEventRepo :: Text
  , ghEventPayload :: Value
  , ghEventCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance FromJSON GitHubEvent where
  parseJSON = withObject "GitHubEvent" $ \v -> do
    eid <- v .: "id"
    etype <- v .: "type"
    actor <- v .: "actor"
    actorLogin <- actor .: "login"
    repo <- v .: "repo"
    repoName <- repo .: "name"
    payload <- v .: "payload"
    createdAt <- v .: "created_at"
    pure $ GitHubEvent eid etype actorLogin repoName payload createdAt

instance ToJSON GitHubEvent where
  toJSON = genericToJSON defaultOptions

-- Response includes events and optional new ETag
data EventsResponse = EventsResponse
  { eventsData :: [GitHubEvent]
  , eventsETag :: Maybe Text
  , eventsNotModified :: Bool
  } deriving (Show)

-- | Commit info extracted from PushEvent payload
data CommitInfo = CommitInfo
  { commitSha :: Text
  , commitMessage :: Text
  , commitAuthor :: Text
  , commitUrl :: Text
  } deriving (Show, Eq)

-- | Extract commits from PushEvent payload JSON
extractCommitsFromPayload :: Value -> [CommitInfo]
extractCommitsFromPayload (Object obj) =
  case KM.lookup "commits" obj of
    Just (Aeson.Array arr) -> mapMaybe parseCommit (toList arr)
    _ -> []
  where
    parseCommit :: Value -> Maybe CommitInfo
    parseCommit (Object c) = do
      sha <- case KM.lookup "sha" c of
               Just (String s) -> Just s
               _ -> Nothing
      message <- case KM.lookup "message" c of
                   Just (String s) -> Just s
                   _ -> Nothing
      author <- case KM.lookup "author" c of
                  Just (Object a) -> case KM.lookup "name" a of
                                       Just (String s) -> Just s
                                       _ -> Nothing
                  _ -> Nothing
      url <- case KM.lookup "url" c of
               Just (String s) -> Just s
               _ -> Nothing
      pure $ CommitInfo sha message author url
    parseCommit _ = Nothing
extractCommitsFromPayload _ = []

-- Fetch events for a user, with optional ETag for caching
-- Returns (events, Maybe newETag, notModified)
listEvents :: Text -> Text -> Maybe Text -> IO (Either Text EventsResponse)
listEvents username accessToken mETag = do
  manager <- newManager tlsManagerSettings
  let url = "https://api.github.com/users/" <> T.unpack username <> "/events"
  req <- parseRequest url
  let headers =
        [ ("Authorization", "Bearer " <> encodeUtf8 accessToken)
        , ("User-Agent", "wisp-srv")
        , ("Accept", "application/vnd.github+json")
        ] <> maybe [] (\etag -> [("If-None-Match", encodeUtf8 etag)]) mETag
  let authReq = req { requestHeaders = headers }
  response <- httpLbs authReq manager

  let status = statusCode (responseStatus response)
  let respHeaders = responseHeaders response
  let newETag = lookup (CI.mk "ETag") respHeaders >>= Just . T.pack . BS.unpack

  case status of
    304 -> pure $ Right $ EventsResponse [] newETag True
    200 -> case Aeson.decode (responseBody response) of
      Just events -> pure $ Right $ EventsResponse events newETag False
      Nothing -> pure $ Left "Failed to parse events response"
    _ -> pure $ Left $ "GitHub API error: " <> T.pack (show status)

-- Extract a human-readable title for an event
extractEventTitle :: GitHubEvent -> Text
extractEventTitle e = ghEventType e <> " to " <> ghEventRepo e
