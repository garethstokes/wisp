-- src/Infra/Db/Document.hs
module Infra.Db.Document
  ( insertDocument
  , getDocumentById
  , getDocumentsByType
  , getDocumentsByTags
  , getActiveDocuments
  , updateDocumentLastActivity
  , archiveDocument
  , insertDocumentLog
  , getDocumentLog
  , getProjectByTag
  , getAllProjects
  , updateProjectData
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField (fromField, returnError, Conversion, FromField(..))
import Database.PostgreSQL.Simple.ToField (toField, ToField(..))
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Domain.Id (EntityId(..), newEntityId)
import Domain.Document
import Domain.Tenant (TenantId(..))
import App.Monad (App, getConn)

-- ToField instances
instance ToField DocumentType where
  toField ProjectDoc = toField ("project" :: Text)
  toField NoteDoc = toField ("note" :: Text)
  toField PreferenceDoc = toField ("preference" :: Text)

instance ToField LogSource where
  toField LogAgent = toField ("agent" :: Text)
  toField LogUser = toField ("user" :: Text)
  toField LogClassifier = toField ("classifier" :: Text)
  toField LogReflection = toField ("reflection" :: Text)

-- FromRow for Document
instance FromRow Document where
  fromRow = Document
    <$> (EntityId <$> field)                    -- id
    <*> (fmap TenantId <$> field)               -- tenant_id
    <*> typeField                               -- type
    <*> field                                   -- data (JSONB -> Value)
    <*> (fromPGArray <$> field)                 -- tags
    <*> field                                   -- confidence
    <*> field                                   -- source
    <*> field                                   -- active
    <*> field                                   -- created_at
    <*> field                                   -- archived_at
    <*> (fmap EntityId <$> field)               -- supersedes_id
    <*> field                                   -- last_activity_at
    where
      typeField = fieldWith $ \f mbs -> do
        txt <- fromField f mbs :: Conversion Text
        case txt of
          "project" -> pure ProjectDoc
          "note" -> pure NoteDoc
          "preference" -> pure PreferenceDoc
          other -> returnError ConversionFailed f $ "Unknown document type: " <> T.unpack other

-- FromRow for DocumentLogEntry
instance FromRow DocumentLogEntry where
  fromRow = DocumentLogEntry
    <$> (EntityId <$> field)                    -- id
    <*> (EntityId <$> field)                    -- document_id
    <*> (fmap EntityId <$> field)               -- activity_id
    <*> field                                   -- description
    <*> sourceField                             -- source
    <*> field                                   -- agent_id
    <*> field                                   -- created_at
    where
      sourceField = fieldWith $ \f mbs -> do
        txt <- fromField f mbs :: Conversion Text
        case txt of
          "agent" -> pure LogAgent
          "user" -> pure LogUser
          "classifier" -> pure LogClassifier
          "reflection" -> pure LogReflection
          other -> returnError ConversionFailed f $ "Unknown log source: " <> T.unpack other

-- Insert a new document
insertDocument :: NewDocument -> App EntityId
insertDocument new = do
  conn <- getConn
  docId <- liftIO newEntityId
  let dataJson = LBS.toStrict $ encode (newDocData new)
  _ <- liftIO $ execute conn
    "INSERT INTO documents (id, tenant_id, type, data, tags, confidence, source, supersedes_id) \
    \VALUES (?, ?, ?, ?::jsonb, ?, ?, ?, ?)"
    ( unEntityId docId
    , fmap (\(TenantId u) -> u) (newDocTenantId new)
    , newDocType new
    , dataJson
    , PGArray (newDocTags new)
    , newDocConfidence new
    , newDocSource new
    , fmap unEntityId (newDocSupersedesId new)
    )
  pure docId

-- Get document by ID
getDocumentById :: EntityId -> App (Maybe Document)
getDocumentById docId = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
    \created_at, archived_at, supersedes_id, last_activity_at \
    \FROM documents WHERE id = ?"
    (Only $ unEntityId docId)
  pure $ case results of
    [doc] -> Just doc
    _ -> Nothing

-- Get documents by type
getDocumentsByType :: DocumentType -> Bool -> Int -> App [Document]
getDocumentsByType docType activeOnly limit = do
  conn <- getConn
  if activeOnly
    then liftIO $ query conn
      "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
      \created_at, archived_at, supersedes_id, last_activity_at \
      \FROM documents WHERE type = ? AND active = TRUE \
      \ORDER BY last_activity_at DESC NULLS LAST, created_at DESC \
      \LIMIT ?"
      (docType, limit)
    else liftIO $ query conn
      "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
      \created_at, archived_at, supersedes_id, last_activity_at \
      \FROM documents WHERE type = ? \
      \ORDER BY created_at DESC \
      \LIMIT ?"
      (docType, limit)

-- Get documents by tags (any match)
getDocumentsByTags :: [Text] -> Bool -> Int -> App [Document]
getDocumentsByTags tags activeOnly limit = do
  conn <- getConn
  if activeOnly
    then liftIO $ query conn
      "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
      \created_at, archived_at, supersedes_id, last_activity_at \
      \FROM documents WHERE tags && ? AND active = TRUE \
      \ORDER BY last_activity_at DESC NULLS LAST, created_at DESC \
      \LIMIT ?"
      (PGArray tags, limit)
    else liftIO $ query conn
      "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
      \created_at, archived_at, supersedes_id, last_activity_at \
      \FROM documents WHERE tags && ? \
      \ORDER BY created_at DESC \
      \LIMIT ?"
      (PGArray tags, limit)

-- Get all active documents
getActiveDocuments :: Int -> App [Document]
getActiveDocuments limit = do
  conn <- getConn
  liftIO $ query conn
    "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
    \created_at, archived_at, supersedes_id, last_activity_at \
    \FROM documents WHERE active = TRUE AND supersedes_id IS NULL \
    \ORDER BY last_activity_at DESC NULLS LAST, created_at DESC \
    \LIMIT ?"
    (Only limit)

-- Update last_activity_at timestamp
updateDocumentLastActivity :: EntityId -> App ()
updateDocumentLastActivity docId = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "UPDATE documents SET last_activity_at = now() WHERE id = ?"
    (Only $ unEntityId docId)
  pure ()

-- Archive a document
archiveDocument :: EntityId -> App ()
archiveDocument docId = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "UPDATE documents SET active = FALSE, archived_at = now() WHERE id = ?"
    (Only $ unEntityId docId)
  pure ()

-- Insert a log entry
insertDocumentLog :: NewDocumentLogEntry -> App EntityId
insertDocumentLog entry = do
  conn <- getConn
  logId <- liftIO newEntityId
  _ <- liftIO $ execute conn
    "INSERT INTO document_log (id, document_id, activity_id, description, source, agent_id) \
    \VALUES (?, ?, ?, ?, ?, ?)"
    ( unEntityId logId
    , unEntityId (newLogDocumentId entry)
    , fmap unEntityId (newLogActivityId entry)
    , newLogDescription entry
    , newLogSource entry
    , newLogAgentId entry
    )
  -- Update last_activity_at on the document
  updateDocumentLastActivity (newLogDocumentId entry)
  pure logId

-- Get log entries for a document
getDocumentLog :: EntityId -> Int -> App [DocumentLogEntry]
getDocumentLog docId limit = do
  conn <- getConn
  liftIO $ query conn
    "SELECT id, document_id, activity_id, description, source, agent_id, created_at \
    \FROM document_log WHERE document_id = ? \
    \ORDER BY created_at DESC \
    \LIMIT ?"
    (unEntityId docId, limit)

-- | Get a project document by its tag (for classifier lookup)
getProjectByTag :: Text -> App (Maybe Document)
getProjectByTag tag = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
    \created_at, archived_at, supersedes_id, last_activity_at \
    \FROM documents \
    \WHERE type = 'project' AND active = TRUE AND ? = ANY(tags) \
    \LIMIT 1"
    (Only tag)
  pure $ case results of
    [doc] -> Just doc
    _ -> Nothing

-- | Get all active projects
getAllProjects :: App [Document]
getAllProjects = do
  conn <- getConn
  liftIO $ query_ conn
    "SELECT id, tenant_id, type, data, tags, confidence, source, active, \
    \created_at, archived_at, supersedes_id, last_activity_at \
    \FROM documents \
    \WHERE type = 'project' AND active = TRUE \
    \ORDER BY last_activity_at DESC NULLS LAST, created_at DESC"

-- | Update project document data (for accumulated state)
updateProjectData :: EntityId -> Value -> App ()
updateProjectData docId newData = do
  conn <- getConn
  let dataJson = LBS.toStrict $ encode newData
  _ <- liftIO $ execute conn
    "UPDATE documents SET data = ?::jsonb, last_activity_at = now() WHERE id = ?"
    (dataJson, unEntityId docId)
  pure ()
