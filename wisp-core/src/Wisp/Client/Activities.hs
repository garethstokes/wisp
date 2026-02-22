module Wisp.Client.Activities
  ( Activity(..)
  , ActivitySource(..)
  , ActivityStatus(..)
  , ActivityMetrics(..)
  , SourceCount(..)
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), ToJSON(..), Value, withObject, withText, (.:), (.:?))
import Data.Aeson.Encode.Pretty (encodePretty', defConfig, confIndent, Indent(..), Config)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data ActivitySource = Email | Calendar | Conversation | Note | GitHubEvent | UnknownSource
  deriving (Show, Eq, Generic)

instance FromJSON ActivitySource where
  parseJSON = withText "ActivitySource" $ \case
    "email" -> pure Email
    "calendar" -> pure Calendar
    "conversation" -> pure Conversation
    "note" -> pure Note
    "github_event" -> pure GitHubEvent
    _ -> pure UnknownSource

instance ToJSON ActivitySource where
  toJSON Email = "email"
  toJSON Calendar = "calendar"
  toJSON Conversation = "conversation"
  toJSON Note = "note"
  toJSON GitHubEvent = "github_event"
  toJSON UnknownSource = "unknown"

data ActivityStatus = Pending | Stored | Surfaced | Quarantined | Archived | NeedsReview | Processed
  deriving (Show, Eq, Generic)

instance FromJSON ActivityStatus where
  parseJSON = withText "ActivityStatus" $ \case
    "pending" -> pure Pending
    "stored" -> pure Stored
    "surfaced" -> pure Surfaced
    "quarantined" -> pure Quarantined
    "archived" -> pure Archived
    "needs_review" -> pure NeedsReview
    "processed" -> pure Processed
    other -> fail $ "Unknown status: " <> show other

instance ToJSON ActivityStatus where
  toJSON Pending = "pending"
  toJSON Stored = "stored"
  toJSON Surfaced = "surfaced"
  toJSON Quarantined = "quarantined"
  toJSON Archived = "archived"
  toJSON NeedsReview = "needs_review"
  toJSON Processed = "processed"

data Activity = Activity
  { activityId :: Text
  , activitySource :: ActivitySource
  , activityTitle :: Maybe Text
  , activityRaw :: Text  -- Stringified from JSON
  , activityStatus :: ActivityStatus
  , activityTags :: [Text]
  , activityCreatedAt :: UTCTime
  , activityConfidence :: Maybe Double
  , activityUrgency :: Maybe Text
  , activityAutonomyTier :: Maybe Int
  } deriving (Show, Eq, Generic)

instance FromJSON Activity where
  parseJSON = withObject "Activity" $ \v -> Activity
    <$> v .: "id"
    <*> v .: "source"
    <*> v .:? "title"
    <*> (rawToText <$> v .: "raw")
    <*> v .: "status"
    <*> (v .: "tags" <|> pure [])  -- Default to empty if missing
    <*> v .: "created_at"
    <*> v .:? "confidence"
    <*> v .:? "urgency"
    <*> v .:? "autonomy_tier"
    where
      -- Pretty-print JSON with 2-space indentation
      rawToText :: Value -> Text
      rawToText val = TE.decodeUtf8 $ BL.toStrict $
        encodePretty' (defConfig { confIndent = Spaces 2 }) val

-- | Per-source activity counts
data SourceCount = SourceCount
  { sourceCountSource :: ActivitySource
  , sourceCountTotal :: Int
  , sourceCountRecent :: Int  -- Last 24 hours
  } deriving (Show, Eq, Generic)

instance FromJSON SourceCount where
  parseJSON = withObject "SourceCount" $ \v -> SourceCount
    <$> v .: "source"
    <*> v .: "total"
    <*> v .: "recent"

-- | Activity metrics summary
data ActivityMetrics = ActivityMetrics
  { metricsTotal :: Int
  , metricsRecent :: Int  -- Last 24 hours
  , metricsBySource :: [SourceCount]
  } deriving (Show, Eq, Generic)

instance FromJSON ActivityMetrics where
  parseJSON = withObject "ActivityMetrics" $ \v -> ActivityMetrics
    <$> v .: "total"
    <*> v .: "recent"
    <*> v .: "by_source"
