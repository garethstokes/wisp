module Domain.ProjectKnowledge
  ( ProjectKnowledgeKind(..)
  , ProductResearchData(..)
  , RoadmapData(..)
  , ArchitectureData(..)
  , ActivityLogData(..)
  , KeyContact(..)
  , OpenQuestion(..)
  , Milestone(..)
  , ActivityHighlight(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withText, withObject, (.:), (.:?), (.!=), object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | The kind of project knowledge document
data ProjectKnowledgeKind
  = ProductResearch
  | Roadmap
  | Architecture
  | ActivityLog
  deriving (Eq, Show, Generic)

instance ToJSON ProjectKnowledgeKind where
  toJSON ProductResearch = "product_research"
  toJSON Roadmap = "roadmap"
  toJSON Architecture = "architecture"
  toJSON ActivityLog = "activity_log"

instance FromJSON ProjectKnowledgeKind where
  parseJSON = withText "ProjectKnowledgeKind" $ \case
    "product_research" -> pure ProductResearch
    "roadmap" -> pure Roadmap
    "architecture" -> pure Architecture
    "activity_log" -> pure ActivityLog
    other -> fail $ "Invalid project knowledge kind: " <> show other

-- | A key contact for a project
data KeyContact = KeyContact
  { kcName :: Text
  , kcEmail :: Maybe Text
  , kcRole :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON KeyContact where
  parseJSON = withObject "KeyContact" $ \v -> KeyContact
    <$> v .:? "name" .!= ""
    <*> v .:? "email"
    <*> v .:? "role"

instance ToJSON KeyContact where
  toJSON kc = object
    [ "name" .= kcName kc
    , "email" .= kcEmail kc
    , "role" .= kcRole kc
    ]

-- | An open question about a project
data OpenQuestion = OpenQuestion
  { oqQuestion :: Text
  , oqRaisedAt :: Maybe UTCTime
  , oqContext :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON OpenQuestion where
  parseJSON = withObject "OpenQuestion" $ \v -> OpenQuestion
    <$> v .:? "question" .!= ""
    <*> v .:? "raised_at"
    <*> v .:? "context"

instance ToJSON OpenQuestion where
  toJSON oq = object
    [ "question" .= oqQuestion oq
    , "raised_at" .= oqRaisedAt oq
    , "context" .= oqContext oq
    ]

-- | A milestone in a project roadmap
data Milestone = Milestone
  { msTitle :: Text
  , msDate :: Maybe Text
  , msStatus :: Maybe Text
  , msSource :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON Milestone where
  parseJSON = withObject "Milestone" $ \v -> Milestone
    <$> v .:? "title" .!= ""
    <*> v .:? "date"
    <*> v .:? "status"
    <*> v .:? "source"

instance ToJSON Milestone where
  toJSON ms = object
    [ "title" .= msTitle ms
    , "date" .= msDate ms
    , "status" .= msStatus ms
    , "source" .= msSource ms
    ]

-- | A highlight from project activity
data ActivityHighlight = ActivityHighlight
  { ahDate :: Text
  , ahDescription :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON ActivityHighlight where
  parseJSON = withObject "ActivityHighlight" $ \v -> ActivityHighlight
    <$> v .:? "date" .!= ""
    <*> v .:? "description" .!= ""

instance ToJSON ActivityHighlight where
  toJSON ah = object
    [ "date" .= ahDate ah
    , "description" .= ahDescription ah
    ]

-- | Product research data for a project
data ProductResearchData = ProductResearchData
  { prdVision :: Text
  , prdValueProposition :: Text
  , prdKeyContacts :: [KeyContact]
  , prdOpenQuestions :: [OpenQuestion]
  } deriving (Eq, Show, Generic)

instance FromJSON ProductResearchData where
  parseJSON = withObject "ProductResearchData" $ \v -> ProductResearchData
    <$> v .:? "vision" .!= ""
    <*> v .:? "value_proposition" .!= ""
    <*> v .:? "key_contacts" .!= []
    <*> v .:? "open_questions" .!= []

instance ToJSON ProductResearchData where
  toJSON prd = object
    [ "kind" .= ProductResearch
    , "vision" .= prdVision prd
    , "value_proposition" .= prdValueProposition prd
    , "key_contacts" .= prdKeyContacts prd
    , "open_questions" .= prdOpenQuestions prd
    ]

-- | Roadmap data for a project
data RoadmapData = RoadmapData
  { rdMilestones :: [Milestone]
  , rdTimelineNotes :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON RoadmapData where
  parseJSON = withObject "RoadmapData" $ \v -> RoadmapData
    <$> v .:? "milestones" .!= []
    <*> v .:? "timeline_notes" .!= ""

instance ToJSON RoadmapData where
  toJSON rd = object
    [ "kind" .= Roadmap
    , "milestones" .= rdMilestones rd
    , "timeline_notes" .= rdTimelineNotes rd
    ]

-- | Architecture data for a project
data ArchitectureData = ArchitectureData
  { adUsersPersonas :: Text
  , adSpecsLinks :: Text
  , adTesting :: Text
  , adCodeStructure :: Text
  , adDataStructure :: Text
  , adInfrastructure :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON ArchitectureData where
  parseJSON = withObject "ArchitectureData" $ \v -> ArchitectureData
    <$> v .:? "users_personas" .!= ""
    <*> v .:? "specs_links" .!= ""
    <*> v .:? "testing" .!= ""
    <*> v .:? "code_structure" .!= ""
    <*> v .:? "data_structure" .!= ""
    <*> v .:? "infrastructure" .!= ""

instance ToJSON ArchitectureData where
  toJSON ad = object
    [ "kind" .= Architecture
    , "users_personas" .= adUsersPersonas ad
    , "specs_links" .= adSpecsLinks ad
    , "testing" .= adTesting ad
    , "code_structure" .= adCodeStructure ad
    , "data_structure" .= adDataStructure ad
    , "infrastructure" .= adInfrastructure ad
    ]

-- | Activity log data for a project
data ActivityLogData = ActivityLogData
  { aldGeneratedAt :: Maybe UTCTime  -- Optional, will be set by system
  , aldPeriod :: Text
  , aldSummary :: Text
  , aldHighlights :: [ActivityHighlight]
  } deriving (Eq, Show, Generic)

instance FromJSON ActivityLogData where
  parseJSON = withObject "ActivityLogData" $ \v -> ActivityLogData
    <$> v .:? "generated_at"
    <*> v .:? "period" .!= "unknown"
    <*> v .:? "summary" .!= ""
    <*> v .:? "highlights" .!= []

instance ToJSON ActivityLogData where
  toJSON ald = object
    [ "kind" .= ActivityLog
    , "generated_at" .= aldGeneratedAt ald
    , "period" .= aldPeriod ald
    , "summary" .= aldSummary ald
    , "highlights" .= aldHighlights ald
    ]
