module Infra.GitHub.Commits
  ( fetchCommitDiff
  , fetchCompareDiff
  , CommitWithDiff(..)
  , PushDiff(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), (.:?), withObject)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

-- | A commit with its diff content
data CommitWithDiff = CommitWithDiff
  { cwdSha :: Text
  , cwdMessage :: Text
  , cwdAuthor :: Text
  , cwdUrl :: Text
  , cwdDiff :: Maybe Text
  , cwdDiffError :: Maybe Text
  } deriving (Show, Eq)

instance ToJSON CommitWithDiff where
  toJSON c = object
    [ "sha" .= cwdSha c
    , "message" .= cwdMessage c
    , "author" .= cwdAuthor c
    , "url" .= cwdUrl c
    , "diff" .= cwdDiff c
    , "diff_error" .= cwdDiffError c
    ]

instance FromJSON CommitWithDiff where
  parseJSON = withObject "CommitWithDiff" $ \v -> CommitWithDiff
    <$> v .: "sha"
    <*> v .: "message"
    <*> v .: "author"
    <*> v .: "url"
    <*> v .: "diff"
    <*> v .: "diff_error"

-- | Fetch the diff for a single commit
-- Returns the raw diff text or an error message
fetchCommitDiff
  :: Text  -- ^ Repository owner
  -> Text  -- ^ Repository name
  -> Text  -- ^ Commit SHA
  -> Text  -- ^ Access token
  -> IO (Either Text Text)
fetchCommitDiff owner repo sha accessToken = do
  manager <- newManager tlsManagerSettings
  let url = "https://api.github.com/repos/" <> T.unpack owner <> "/" <> T.unpack repo <> "/commits/" <> T.unpack sha
  req <- parseRequest url
  let headers =
        [ ("Authorization", "Bearer " <> TE.encodeUtf8 accessToken)
        , ("User-Agent", "wisp-srv")
        , ("Accept", "application/vnd.github.diff")
        ]
  let authReq = req { requestHeaders = headers }
  response <- httpLbs authReq manager

  let status = statusCode (responseStatus response)
  case status of
    200 -> pure $ Right $ TE.decodeUtf8 $ LBS.toStrict $ responseBody response
    404 -> pure $ Left "404: Commit not found (may have been force-pushed)"
    403 -> pure $ Left "403: Rate limited or access denied"
    _ -> pure $ Left $ "GitHub API error: " <> T.pack (show status)

-- | Result of comparing two commits
data PushDiff = PushDiff
  { pdBaseSha :: Text
  , pdHeadSha :: Text
  , pdDiff :: Maybe Text
  , pdDiffError :: Maybe Text
  , pdCommitCount :: Int
  } deriving (Show, Eq)

instance ToJSON PushDiff where
  toJSON p = object
    [ "base_sha" .= pdBaseSha p
    , "head_sha" .= pdHeadSha p
    , "diff" .= pdDiff p
    , "diff_error" .= pdDiffError p
    , "commit_count" .= pdCommitCount p
    ]

instance FromJSON PushDiff where
  parseJSON = withObject "PushDiff" $ \v -> PushDiff
    <$> v .: "base_sha"
    <*> v .: "head_sha"
    <*> v .:? "diff"
    <*> v .:? "diff_error"
    <*> v .: "commit_count"

-- | Fetch the diff between two commits using GitHub's compare API
-- Returns the raw diff text or an error message
fetchCompareDiff
  :: Text  -- ^ Repository owner
  -> Text  -- ^ Repository name
  -> Text  -- ^ Base SHA (before)
  -> Text  -- ^ Head SHA (after)
  -> Text  -- ^ Access token
  -> IO (Either Text Text)
fetchCompareDiff owner repo baseSha headSha accessToken = do
  manager <- newManager tlsManagerSettings
  let url = "https://api.github.com/repos/" <> T.unpack owner <> "/" <> T.unpack repo
            <> "/compare/" <> T.unpack baseSha <> "..." <> T.unpack headSha
  req <- parseRequest url
  let headers =
        [ ("Authorization", "Bearer " <> TE.encodeUtf8 accessToken)
        , ("User-Agent", "wisp-srv")
        , ("Accept", "application/vnd.github.diff")
        ]
  let authReq = req { requestHeaders = headers }
  response <- httpLbs authReq manager

  let status = statusCode (responseStatus response)
  case status of
    200 -> pure $ Right $ TE.decodeUtf8 $ LBS.toStrict $ responseBody response
    404 -> pure $ Left "404: Commits not found (may have been force-pushed)"
    403 -> pure $ Left "403: Rate limited or access denied"
    _ -> pure $ Left $ "GitHub API error: " <> T.pack (show status)
