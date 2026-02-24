-- src/Domain/Document.hs
module Domain.Document
  ( Document(..)
  , NewDocument(..)
  , DocumentType(..)
  , ProjectType(..)
  , ProjectData(..)
  , ExtendedProjectData(..)
  , NoteData(..)
  , PreferenceData(..)
  , DocumentLogEntry(..)
  , NewDocumentLogEntry(..)
  , LogSource(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value, object, withObject, withText, (.:), (.:?), (.=), (.!=))
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Id (EntityId(..), unEntityId)
import Domain.Tenant (TenantId)
import GHC.Generics (Generic)

-- Project types
data ProjectType = Work | Personal | Family | Health | Spiritual
  deriving (Eq, Show, Generic)

instance ToJSON ProjectType where
  toJSON Work = "work"
  toJSON Personal = "personal"
  toJSON Family = "family"
  toJSON Health = "health"
  toJSON Spiritual = "spiritual"

instance FromJSON ProjectType where
  parseJSON = withText "ProjectType" $ \case
    "work" -> pure Work
    "personal" -> pure Personal
    "family" -> pure Family
    "health" -> pure Health
    "spiritual" -> pure Spiritual
    other -> fail $ "Unknown project type: " <> show other

-- Document type discriminator
data DocumentType = ProjectDoc | NoteDoc | PreferenceDoc | ProjectKnowledgeDoc
  deriving (Eq, Show, Generic)

instance ToJSON DocumentType where
  toJSON ProjectDoc = "project"
  toJSON NoteDoc = "note"
  toJSON PreferenceDoc = "preference"
  toJSON ProjectKnowledgeDoc = "project_knowledge"

instance FromJSON DocumentType where
  parseJSON = withText "DocumentType" $ \case
    "project" -> pure ProjectDoc
    "note" -> pure NoteDoc
    "preference" -> pure PreferenceDoc
    "project_knowledge" -> pure ProjectKnowledgeDoc
    other -> fail $ "Unknown document type: " <> show other

-- Type-specific data
data ProjectData = ProjectData
  { projectName :: Text
  , projectType :: ProjectType
  } deriving (Eq, Show, Generic)

instance ToJSON ProjectData where
  toJSON p = object ["name" .= projectName p, "type" .= projectType p]

instance FromJSON ProjectData where
  parseJSON = withObject "ProjectData" $ \v ->
    ProjectData <$> v .: "name" <*> v .: "type"

-- Extended project data with accumulated state
data ExtendedProjectData = ExtendedProjectData
  { extProjectName :: Text
  , extProjectType :: ProjectType
  , extProjectSummary :: Text
  , extProjectStatus :: Text  -- "active", "stalled", "completed"
  , extProjectParticipants :: [Text]
  , extProjectActivityCount :: Int
  , extProjectLastActivityAt :: Maybe UTCTime
  } deriving (Eq, Show, Generic)

instance ToJSON ExtendedProjectData where
  toJSON p = object
    [ "name" .= extProjectName p
    , "type" .= extProjectType p
    , "summary" .= extProjectSummary p
    , "status" .= extProjectStatus p
    , "participants" .= extProjectParticipants p
    , "activity_count" .= extProjectActivityCount p
    , "last_activity_at" .= extProjectLastActivityAt p
    ]

instance FromJSON ExtendedProjectData where
  parseJSON = withObject "ExtendedProjectData" $ \v -> ExtendedProjectData
    <$> v .: "name"
    <*> v .: "type"
    <*> v .:? "summary" .!= ""
    <*> v .:? "status" .!= "active"
    <*> v .:? "participants" .!= []
    <*> v .:? "activity_count" .!= 0
    <*> v .:? "last_activity_at"

data NoteData = NoteData
  { noteTitle :: Text
  , noteContent :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON NoteData where
  toJSON n = object ["title" .= noteTitle n, "content" .= noteContent n]

instance FromJSON NoteData where
  parseJSON = withObject "NoteData" $ \v ->
    NoteData <$> v .: "title" <*> v .:? "content"

data PreferenceData = PreferenceData
  { prefKey :: Text
  , prefValue :: Text
  , prefContext :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON PreferenceData where
  toJSON p = object ["key" .= prefKey p, "value" .= prefValue p, "context" .= prefContext p]

instance FromJSON PreferenceData where
  parseJSON = withObject "PreferenceData" $ \v ->
    PreferenceData <$> v .: "key" <*> v .: "value" <*> v .:? "context"

-- Main document type
data Document = Document
  { documentId :: EntityId
  , documentTenantId :: Maybe TenantId
  , documentType :: DocumentType
  , documentData :: Value  -- Raw JSONB, parsed per-type as needed
  , documentTags :: [Text]
  , documentConfidence :: Maybe Double
  , documentSource :: Maybe Text
  , documentActive :: Bool
  , documentCreatedAt :: UTCTime
  , documentArchivedAt :: Maybe UTCTime
  , documentSupersedesId :: Maybe EntityId
  , documentLastActivityAt :: Maybe UTCTime
  , documentParentId :: Maybe EntityId
  } deriving (Eq, Show)

instance ToJSON Document where
  toJSON d = object
    [ "id" .= unEntityId (documentId d)
    , "tenant_id" .= documentTenantId d
    , "type" .= documentType d
    , "data" .= documentData d
    , "tags" .= documentTags d
    , "confidence" .= documentConfidence d
    , "source" .= documentSource d
    , "active" .= documentActive d
    , "created_at" .= documentCreatedAt d
    , "archived_at" .= documentArchivedAt d
    , "supersedes_id" .= fmap unEntityId (documentSupersedesId d)
    , "last_activity_at" .= documentLastActivityAt d
    , "parent_id" .= fmap unEntityId (documentParentId d)
    ]

-- For creating new documents
data NewDocument = NewDocument
  { newDocTenantId :: Maybe TenantId
  , newDocType :: DocumentType
  , newDocData :: Value
  , newDocTags :: [Text]
  , newDocConfidence :: Maybe Double
  , newDocSource :: Maybe Text
  , newDocSupersedesId :: Maybe EntityId
  , newDocParentId :: Maybe EntityId
  } deriving (Eq, Show)

-- Log entry source
data LogSource = LogAgent | LogUser | LogClassifier | LogReflection
  deriving (Eq, Show, Generic)

instance ToJSON LogSource where
  toJSON LogAgent = "agent"
  toJSON LogUser = "user"
  toJSON LogClassifier = "classifier"
  toJSON LogReflection = "reflection"

instance FromJSON LogSource where
  parseJSON = withText "LogSource" $ \case
    "agent" -> pure LogAgent
    "user" -> pure LogUser
    "classifier" -> pure LogClassifier
    "reflection" -> pure LogReflection
    other -> fail $ "Unknown log source: " <> show other

-- Document log entry
data DocumentLogEntry = DocumentLogEntry
  { logId :: EntityId
  , logDocumentId :: EntityId
  , logActivityId :: Maybe EntityId
  , logDescription :: Maybe Text
  , logSource :: LogSource
  , logAgentId :: Maybe Text
  , logCreatedAt :: UTCTime
  } deriving (Eq, Show)

instance ToJSON DocumentLogEntry where
  toJSON l = object
    [ "id" .= unEntityId (logId l)
    , "document_id" .= unEntityId (logDocumentId l)
    , "activity_id" .= fmap unEntityId (logActivityId l)
    , "description" .= logDescription l
    , "source" .= logSource l
    , "agent_id" .= logAgentId l
    , "created_at" .= logCreatedAt l
    ]

-- For creating new log entries
data NewDocumentLogEntry = NewDocumentLogEntry
  { newLogDocumentId :: EntityId
  , newLogActivityId :: Maybe EntityId
  , newLogDescription :: Maybe Text
  , newLogSource :: LogSource
  , newLogAgentId :: Maybe Text
  } deriving (Eq, Show)
