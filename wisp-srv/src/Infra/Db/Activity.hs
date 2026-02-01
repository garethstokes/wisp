-- src/Infra/Db/Activity.hs
module Infra.Db.Activity
  ( insertActivity
  , activityExists
  , getActivity
  , getActivitiesByStatus
  , updateActivityStatus
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson ()
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Domain.Id (EntityId(..), newEntityId)
import Domain.Activity
import App.Monad (App, getConn)

instance FromRow Activity where
  fromRow = Activity
    <$> (EntityId <$> field)          -- id
    <*> (parseSource <$> field)       -- source
    <*> field                          -- source_id
    <*> field                          -- raw (jsonb)
    <*> (parseStatus <$> field)       -- status
    <*> field                          -- title
    <*> field                          -- summary
    <*> field                          -- sender_email
    <*> field                          -- starts_at
    <*> field                          -- ends_at
    <*> field                          -- created_at
    where
      parseSource :: Text -> ActivitySource
      parseSource "email" = Email
      parseSource "calendar" = Calendar
      parseSource _ = Email  -- default

      parseStatus :: Text -> ActivityStatus
      parseStatus "pending" = Pending
      parseStatus "quarantined" = Quarantined
      parseStatus "processed" = Processed
      parseStatus "surfaced" = Surfaced
      parseStatus "archived" = Archived
      parseStatus _ = Pending  -- default

-- Insert a new activity (returns Nothing if duplicate)
insertActivity :: NewActivity -> App (Maybe EntityId)
insertActivity new = do
  conn <- getConn
  aid <- liftIO newEntityId
  let srcText = case newActivitySource new of
        Email -> "email" :: Text
        Calendar -> "calendar"
  n <- liftIO $ execute conn
    "insert into activities \
    \(id, source, source_id, raw, title, sender_email, starts_at, ends_at) \
    \values (?, ?, ?, ?, ?, ?, ?, ?) \
    \on conflict (source, source_id) do nothing"
    ( unEntityId aid
    , srcText
    , newActivitySourceId new
    , newActivityRaw new
    , newActivityTitle new
    , newActivitySenderEmail new
    , newActivityStartsAt new
    , newActivityEndsAt new
    )
  pure $ if n > 0 then Just aid else Nothing

-- Check if activity already exists
activityExists :: ActivitySource -> Text -> App Bool
activityExists src srcId = do
  conn <- getConn
  let srcText = case src of
        Email -> "email" :: Text
        Calendar -> "calendar"
  results <- liftIO $ query conn
    "select 1 from activities where source = ? and source_id = ? limit 1"
    (srcText, srcId)
  pure $ not (null (results :: [Only Int]))

-- Get activity by ID
getActivity :: EntityId -> App (Maybe Activity)
getActivity aid = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at \
    \from activities where id = ?"
    (Only $ unEntityId aid)
  pure $ case results of
    [a] -> Just a
    _ -> Nothing

-- Get activities by status
getActivitiesByStatus :: ActivityStatus -> Int -> App [Activity]
getActivitiesByStatus status limit = do
  conn <- getConn
  let statusText = case status of
        Pending -> "pending" :: Text
        Quarantined -> "quarantined"
        Processed -> "processed"
        Surfaced -> "surfaced"
        Archived -> "archived"
  liftIO $ query conn
    "select id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at \
    \from activities where status = ? \
    \order by created_at desc limit ?"
    (statusText, limit)

-- Update activity status
updateActivityStatus :: EntityId -> ActivityStatus -> App ()
updateActivityStatus aid status = do
  conn <- getConn
  let statusText = case status of
        Pending -> "pending" :: Text
        Quarantined -> "quarantined"
        Processed -> "processed"
        Surfaced -> "surfaced"
        Archived -> "archived"
  _ <- liftIO $ execute conn
    "update activities set status = ?, updated_at = now() where id = ?"
    (statusText, unEntityId aid)
  pure ()
