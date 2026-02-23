module Domain.Classification
  ( Classification(..)
  , ActivityType(..)
  , Urgency(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withText, withObject, (.:), (.:?), (.!=), object, (.=))
import Domain.ProjectClassification (ProjectAssignment)
import Data.Text (Text)
import GHC.Generics (Generic)

data ActivityType
  = Request
  | Information
  | ActionRequired
  | FYI
  | Event
  deriving (Eq, Show, Generic)

instance ToJSON ActivityType where
  toJSON Request = "request"
  toJSON Information = "information"
  toJSON ActionRequired = "action_required"
  toJSON FYI = "fyi"
  toJSON Event = "event"

instance FromJSON ActivityType where
  parseJSON = withText "ActivityType" $ \case
    "request" -> pure Request
    "information" -> pure Information
    "action_required" -> pure ActionRequired
    "fyi" -> pure FYI
    "event" -> pure Event
    other -> fail $ "Invalid activity type: " <> show other

data Urgency = High | Normal | Low
  deriving (Eq, Show, Generic)

instance ToJSON Urgency where
  toJSON High = "high"
  toJSON Normal = "normal"
  toJSON Low = "low"

instance FromJSON Urgency where
  parseJSON = withText "Urgency" $ \case
    "high" -> pure High
    "normal" -> pure Normal
    "low" -> pure Low
    other -> fail $ "Invalid urgency: " <> show other

data Classification = Classification
  { classificationPersonas :: [Text]
  , classificationActivityType :: ActivityType
  , classificationUrgency :: Urgency
  , classificationAutonomyTier :: Int
  , classificationConfidence :: Double
  , classificationSummary :: Text
  , classificationReasoning :: Text
  , classificationSuggestedActions :: [Text]
  , classificationOptionFraming :: Maybe Text
  , classificationProjects :: [ProjectAssignment]
  } deriving (Eq, Show, Generic)

instance FromJSON Classification where
  parseJSON = withObject "Classification" $ \v -> Classification
    <$> v .: "personas"
    <*> v .: "activity_type"
    <*> v .: "urgency"
    <*> v .: "autonomy_tier"
    <*> v .: "confidence"
    <*> v .: "summary"
    <*> v .: "reasoning"
    <*> v .: "suggested_actions"
    <*> v .:? "option_framing"
    <*> v .:? "projects" .!= []

instance ToJSON Classification where
  toJSON c = object
    [ "personas" .= classificationPersonas c
    , "activity_type" .= classificationActivityType c
    , "urgency" .= classificationUrgency c
    , "autonomy_tier" .= classificationAutonomyTier c
    , "confidence" .= classificationConfidence c
    , "summary" .= classificationSummary c
    , "reasoning" .= classificationReasoning c
    , "suggested_actions" .= classificationSuggestedActions c
    , "option_framing" .= classificationOptionFraming c
    , "projects" .= classificationProjects c
    ]
