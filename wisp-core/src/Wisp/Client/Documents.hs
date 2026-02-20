module Wisp.Client.Documents
  ( Document(..)
  , DocumentType(..)
  , ProjectData(..)
  , NoteData(..)
  , PreferenceData(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value, withObject, withText, (.:), (.:?), object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data DocumentType = ProjectDoc | NoteDoc | PreferenceDoc
  deriving (Show, Eq, Generic)

instance FromJSON DocumentType where
  parseJSON = withText "DocumentType" $ \case
    "project" -> pure ProjectDoc
    "note" -> pure NoteDoc
    "preference" -> pure PreferenceDoc
    other -> fail $ "Unknown document type: " <> show other

instance ToJSON DocumentType where
  toJSON ProjectDoc = "project"
  toJSON NoteDoc = "note"
  toJSON PreferenceDoc = "preference"

data Document = Document
  { documentId :: Text
  , documentType :: DocumentType
  , documentData :: Value
  , documentTags :: [Text]
  , documentActive :: Bool
  , documentCreatedAt :: UTCTime
  , documentLastActivityAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance FromJSON Document where
  parseJSON = withObject "Document" $ \v -> Document
    <$> v .: "id"
    <*> v .: "type"
    <*> v .: "data"
    <*> v .: "tags"
    <*> v .: "active"
    <*> v .: "created_at"
    <*> v .:? "last_activity_at"

data ProjectData = ProjectData
  { projectName :: Text
  , projectType :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON ProjectData where
  parseJSON = withObject "ProjectData" $ \v -> ProjectData
    <$> v .: "name"
    <*> v .: "type"

instance ToJSON ProjectData where
  toJSON p = object ["name" .= projectName p, "type" .= projectType p]

data NoteData = NoteData
  { noteTitle :: Text
  , noteContent :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON NoteData where
  parseJSON = withObject "NoteData" $ \v -> NoteData
    <$> v .: "title"
    <*> v .:? "content"

instance ToJSON NoteData where
  toJSON n = object ["title" .= noteTitle n, "content" .= noteContent n]

data PreferenceData = PreferenceData
  { prefKey :: Text
  , prefValue :: Text
  , prefContext :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON PreferenceData where
  parseJSON = withObject "PreferenceData" $ \v -> PreferenceData
    <$> v .: "key"
    <*> v .: "value"
    <*> v .:? "context"

instance ToJSON PreferenceData where
  toJSON p = object ["key" .= prefKey p, "value" .= prefValue p, "context" .= prefContext p]
