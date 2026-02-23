-- src/Infra/Db/ActivityDocument.hs
module Infra.Db.ActivityDocument
  ( linkActivityToDocument
  , getActivityProjects
  , getDocumentActivities
  , unlinkActivityFromDocument
  ) where

import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField (fromField, returnError, Conversion, FromField(..))
import Database.PostgreSQL.Simple.ToField (toField, ToField(..))
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Id (EntityId(..), unEntityId)
import Domain.ActivityDocument
import Domain.Document (Document)
import App.Monad (App, getConn)
import Infra.Db.Document ()  -- Import FromRow Document instance

-- ToField instances
instance ToField LinkSource where
  toField Classifier = toField ("classifier" :: Text)
  toField User = toField ("user" :: Text)
  toField Reflection = toField ("reflection" :: Text)

instance ToField Relationship where
  toField Project = toField ("project" :: Text)
  toField Mentions = toField ("mentions" :: Text)

-- FromField instances
instance FromField LinkSource where
  fromField f mbs = do
    txt <- fromField f mbs :: Conversion Text
    case txt of
      "classifier" -> pure Classifier
      "user" -> pure User
      "reflection" -> pure Reflection
      other -> returnError ConversionFailed f $ "Unknown link source: " <> T.unpack other

instance FromField Relationship where
  fromField f mbs = do
    txt <- fromField f mbs :: Conversion Text
    case txt of
      "project" -> pure Project
      "mentions" -> pure Mentions
      other -> returnError ConversionFailed f $ "Unknown relationship: " <> T.unpack other

-- FromRow instance for ActivityDocument
instance FromRow ActivityDocument where
  fromRow = ActivityDocument
    <$> (EntityId <$> field)  -- activity_id
    <*> (EntityId <$> field)  -- document_id
    <*> field                 -- relationship
    <*> field                 -- confidence
    <*> field                 -- source
    <*> field                 -- created_at

-- | Link an activity to a document
linkActivityToDocument :: NewActivityDocument -> App ()
linkActivityToDocument link = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "INSERT INTO activity_documents (activity_id, document_id, relationship, confidence, source) \
    \VALUES (?, ?, ?, ?, ?) \
    \ON CONFLICT (activity_id, document_id, relationship) DO UPDATE SET \
    \confidence = EXCLUDED.confidence, source = EXCLUDED.source"
    ( unEntityId (newAdActivityId link)
    , unEntityId (newAdDocumentId link)
    , newAdRelationship link
    , newAdConfidence link
    , newAdSource link
    )
  pure ()

-- | Get all project documents linked to an activity
getActivityProjects :: EntityId -> App [Document]
getActivityProjects actId = do
  conn <- getConn
  liftIO $ query conn
    "SELECT d.id, d.tenant_id, d.type, d.data, d.tags, d.confidence, d.source, d.active, \
    \d.created_at, d.archived_at, d.supersedes_id, d.last_activity_at \
    \FROM documents d \
    \JOIN activity_documents ad ON d.id = ad.document_id \
    \WHERE ad.activity_id = ? AND ad.relationship = 'project' \
    \ORDER BY ad.confidence DESC NULLS LAST"
    (Only $ unEntityId actId)

-- | Get all activities linked to a document
getDocumentActivities :: EntityId -> Int -> App [ActivityDocument]
getDocumentActivities docId limit = do
  conn <- getConn
  liftIO $ query conn
    "SELECT activity_id, document_id, relationship, confidence, source, created_at \
    \FROM activity_documents \
    \WHERE document_id = ? \
    \ORDER BY created_at DESC \
    \LIMIT ?"
    (unEntityId docId, limit)

-- | Remove a link
unlinkActivityFromDocument :: EntityId -> EntityId -> Relationship -> App ()
unlinkActivityFromDocument actId docId rel = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "DELETE FROM activity_documents WHERE activity_id = ? AND document_id = ? AND relationship = ?"
    (unEntityId actId, unEntityId docId, rel)
  pure ()
