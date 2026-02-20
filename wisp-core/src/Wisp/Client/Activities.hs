module Wisp.Client.Activities
  ( Activity(..)
  , ActivitySource(..)
  , ActivityStatus(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, withText, (.:), (.:?))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data ActivitySource = Email | Calendar | Conversation | Note | GitHubEvent
  deriving (Show, Eq, Generic)

instance FromJSON ActivitySource where
  parseJSON = withText "ActivitySource" $ \case
    "email" -> pure Email
    "calendar" -> pure Calendar
    "conversation" -> pure Conversation
    "note" -> pure Note
    "github_event" -> pure GitHubEvent
    other -> fail $ "Unknown source: " <> show other

instance ToJSON ActivitySource where
  toJSON Email = "email"
  toJSON Calendar = "calendar"
  toJSON Conversation = "conversation"
  toJSON Note = "note"
  toJSON GitHubEvent = "github_event"

data ActivityStatus = Pending | Stored | Surfaced | Quarantined | Archived
  deriving (Show, Eq, Generic)

instance FromJSON ActivityStatus where
  parseJSON = withText "ActivityStatus" $ \case
    "pending" -> pure Pending
    "stored" -> pure Stored
    "surfaced" -> pure Surfaced
    "quarantined" -> pure Quarantined
    "archived" -> pure Archived
    other -> fail $ "Unknown status: " <> show other

instance ToJSON ActivityStatus where
  toJSON Pending = "pending"
  toJSON Stored = "stored"
  toJSON Surfaced = "surfaced"
  toJSON Quarantined = "quarantined"
  toJSON Archived = "archived"

data Activity = Activity
  { activityId :: Text
  , activitySource :: ActivitySource
  , activityTitle :: Maybe Text
  , activityRaw :: Text
  , activityStatus :: ActivityStatus
  , activityTags :: [Text]
  , activityCreatedAt :: UTCTime
  , activityConfidence :: Maybe Double
  } deriving (Show, Eq, Generic)

instance FromJSON Activity where
  parseJSON = withObject "Activity" $ \v -> Activity
    <$> v .: "id"
    <*> v .: "source"
    <*> v .:? "title"
    <*> v .: "raw"
    <*> v .: "status"
    <*> v .: "tags"
    <*> v .: "created_at"
    <*> v .:? "confidence"
