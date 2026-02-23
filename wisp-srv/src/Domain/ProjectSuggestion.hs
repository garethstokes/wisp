module Domain.ProjectSuggestion
  ( ProjectSuggestion(..)
  , ClusterKey(..)
  , unClusterKey
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, withObject, (.:), (.=))
import Data.Text (Text)
import Domain.Id (EntityId)
import GHC.Generics (Generic)

newtype ClusterKey = ClusterKey { unClusterKey :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON ClusterKey where
  toJSON (ClusterKey k) = toJSON k

instance FromJSON ClusterKey where
  parseJSON v = ClusterKey <$> parseJSON v

data ProjectSuggestion = ProjectSuggestion
  { psSuggestedName :: Text
  , psClusterKey :: ClusterKey
  , psReason :: Text
  , psSampleActivityIds :: [EntityId]
  , psActivityCount :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON ProjectSuggestion where
  toJSON s = object
    [ "suggested_name" .= psSuggestedName s
    , "cluster_key" .= psClusterKey s
    , "reason" .= psReason s
    , "sample_activity_ids" .= psSampleActivityIds s
    , "activity_count" .= psActivityCount s
    ]

instance FromJSON ProjectSuggestion where
  parseJSON = withObject "ProjectSuggestion" $ \v -> ProjectSuggestion
    <$> v .: "suggested_name"
    <*> v .: "cluster_key"
    <*> v .: "reason"
    <*> v .: "sample_activity_ids"
    <*> v .: "activity_count"
