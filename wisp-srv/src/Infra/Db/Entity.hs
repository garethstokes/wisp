module Infra.Db.Entity
  ( insertEntity
  , insertEntityVersion
  , fetchEntity
  , fetchAllEntities
  , updateEntity
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, FromJSON, toJSON, decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Domain.Id (EntityId(..))
import App.Monad (App, getConn)

-- Insert new version of entity (version auto-assigned by trigger)
insertEntity :: (ToJSON a) => Text -> EntityId -> a -> App ()
insertEntity entityType entityId payload = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "insert into entities (entity_type, entity_id, payload) values (?, ?, ?)"
    (entityType, unEntityId entityId, toJSON payload)
  pure ()

-- Insert with explicit version (for optimistic concurrency)
insertEntityVersion :: (ToJSON a) => Text -> EntityId -> Int -> a -> App Bool
insertEntityVersion entityType entityId version payload = do
  conn <- getConn
  n <- liftIO $ execute conn
    "insert into entities (entity_type, entity_id, entity_version, payload) \
    \values (?, ?, ?, ?) on conflict do nothing"
    (entityType, unEntityId entityId, version, toJSON payload)
  pure (n > 0)

-- Fetch latest version
fetchEntity :: (FromJSON a) => Text -> EntityId -> App (Maybe (Int, a))
fetchEntity entityType entityId = do
  conn <- getConn
  results <- liftIO $ query conn
    "select entity_version, payload from entities \
    \where entity_type = ? and entity_id = ? \
    \order by entity_version desc limit 1"
    (entityType, unEntityId entityId)
  pure $ case results of
    [(v, p)] -> case decode (fromStrict p) of
      Just a -> Just (v, a)
      Nothing -> Nothing
    _ -> Nothing

-- Fetch all latest versions of a type
fetchAllEntities :: (FromJSON a) => Text -> App [(EntityId, Int, a)]
fetchAllEntities entityType = do
  conn <- getConn
  results <- liftIO $ query conn
    "select distinct on (entity_id) entity_id, entity_version, payload \
    \from entities where entity_type = ? \
    \order by entity_id, entity_version desc"
    (Only entityType)
  pure [ (EntityId eid, v, a)
       | (eid, v, p) <- results
       , Just a <- [decode (fromStrict p)]
       ]

-- Optimistic update: insert expected version + 1
updateEntity :: (ToJSON a) => Text -> EntityId -> Int -> a -> App Bool
updateEntity entityType entityId expectedVersion payload =
  insertEntityVersion entityType entityId (expectedVersion + 1) payload
