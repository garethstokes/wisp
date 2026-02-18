-- src/Domain/Activity.hs
module Domain.Activity
  ( Activity(..)
  , ActivitySource(..)
  , ActivityStatus(..)
  , NewActivity(..)
  , normalizeTag
  , normalizeTags
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value, withText)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Domain.Id (EntityId)
import GHC.Generics (Generic)

data ActivitySource = Email | Calendar | Conversation | Note
  deriving (Eq, Show, Generic)

instance ToJSON ActivitySource where
  toJSON Email = "email"
  toJSON Calendar = "calendar"
  toJSON Conversation = "conversation"
  toJSON Note = "note"

instance FromJSON ActivitySource where
  parseJSON = withText "ActivitySource" $ \case
    "email" -> pure Email
    "calendar" -> pure Calendar
    "conversation" -> pure Conversation
    "note" -> pure Note
    _ -> fail "Invalid activity source"

data ActivityStatus
  = Pending
  | NeedsReview
  | Quarantined
  | Processed
  | Surfaced
  | Archived
  deriving (Eq, Show, Generic)

instance ToJSON ActivityStatus where
  toJSON Pending = "pending"
  toJSON NeedsReview = "needs_review"
  toJSON Quarantined = "quarantined"
  toJSON Processed = "processed"
  toJSON Surfaced = "surfaced"
  toJSON Archived = "archived"

instance FromJSON ActivityStatus where
  parseJSON = withText "ActivityStatus" $ \case
    "pending" -> pure Pending
    "needs_review" -> pure NeedsReview
    "quarantined" -> pure Quarantined
    "processed" -> pure Processed
    "surfaced" -> pure Surfaced
    "archived" -> pure Archived
    _ -> fail "Invalid activity status"

-- Full activity record from database
data Activity = Activity
  { activityId :: EntityId
  , activityAccountId :: EntityId
  , activitySource :: ActivitySource
  , activitySourceId :: Text
  , activityRaw :: Value
  , activityStatus :: ActivityStatus
  , activityTitle :: Maybe Text
  , activitySummary :: Maybe Text
  , activitySenderEmail :: Maybe Text
  , activityStartsAt :: Maybe UTCTime
  , activityEndsAt :: Maybe UTCTime
  , activityCreatedAt :: UTCTime
  -- Classification fields (populated by classifier):
  , activityPersonas :: Maybe [Text]
  , activityType :: Maybe Text
  , activityUrgency :: Maybe Text
  , activityAutonomyTier :: Maybe Int
  , activityConfidence :: Maybe Double
  , activityPersonId :: Maybe EntityId
  , activityTags :: [Text]
  , activityParentId :: Maybe EntityId
  } deriving (Show)

-- For creating new activities
data NewActivity = NewActivity
  { newActivityAccountId :: EntityId
  , newActivitySource :: ActivitySource
  , newActivitySourceId :: Text
  , newActivityRaw :: Value
  , newActivityTitle :: Maybe Text
  , newActivitySenderEmail :: Maybe Text
  , newActivityStartsAt :: Maybe UTCTime
  , newActivityEndsAt :: Maybe UTCTime
  } deriving (Show)

-- Tag normalization
normalizeTag :: Text -> Text
normalizeTag = T.toLower . T.strip

normalizeTags :: [Text] -> [Text]
normalizeTags = map normalizeTag
