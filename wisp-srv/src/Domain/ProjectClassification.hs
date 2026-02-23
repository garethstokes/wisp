module Domain.ProjectClassification
  ( ProjectAssignment(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, object, (.:), (.=))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A project assignment from the classifier
data ProjectAssignment = ProjectAssignment
  { paName :: Text       -- Lowercase project identifier (e.g., "wisp", "superit")
  , paConfidence :: Double
  } deriving (Eq, Show, Generic)

instance FromJSON ProjectAssignment where
  parseJSON = withObject "ProjectAssignment" $ \v -> ProjectAssignment
    <$> v .: "name"
    <*> v .: "confidence"

instance ToJSON ProjectAssignment where
  toJSON pa = object
    [ "name" .= paName pa
    , "confidence" .= paConfidence pa
    ]
