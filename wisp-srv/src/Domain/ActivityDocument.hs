module Domain.ActivityDocument
  ( ActivityDocument(..)
  , NewActivityDocument(..)
  , LinkSource(..)
  , Relationship(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Time (UTCTime)
import Domain.Id (EntityId)
import GHC.Generics (Generic)

-- | Source of the activity-document link
data LinkSource = Classifier | User | Reflection
  deriving (Eq, Show, Generic)

instance ToJSON LinkSource where
  toJSON Classifier = "classifier"
  toJSON User = "user"
  toJSON Reflection = "reflection"

instance FromJSON LinkSource where
  parseJSON = withText "LinkSource" $ \case
    "classifier" -> pure Classifier
    "user" -> pure User
    "reflection" -> pure Reflection
    other -> fail $ "Unknown link source: " <> show other

-- | Type of relationship between activity and document
data Relationship = Project | Mentions
  deriving (Eq, Show, Generic)

instance ToJSON Relationship where
  toJSON Project = "project"
  toJSON Mentions = "mentions"

instance FromJSON Relationship where
  parseJSON = withText "Relationship" $ \case
    "project" -> pure Project
    "mentions" -> pure Mentions
    other -> fail $ "Unknown relationship: " <> show other

-- | Full activity-document link from database
data ActivityDocument = ActivityDocument
  { adActivityId :: EntityId
  , adDocumentId :: EntityId
  , adRelationship :: Relationship
  , adConfidence :: Maybe Double
  , adSource :: LinkSource
  , adCreatedAt :: UTCTime
  } deriving (Eq, Show)

-- | For creating new links
data NewActivityDocument = NewActivityDocument
  { newAdActivityId :: EntityId
  , newAdDocumentId :: EntityId
  , newAdRelationship :: Relationship
  , newAdConfidence :: Maybe Double
  , newAdSource :: LinkSource
  } deriving (Eq, Show)
