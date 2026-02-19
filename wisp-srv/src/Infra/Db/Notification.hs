module Infra.Db.Notification
  ( getLastNotificationTime
  , updateLastNotificationTime
  , getNotifiableActivities
  , markActivitiesNotified
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Domain.Activity (Activity(..))
import Domain.Id (EntityId(..))
import App.Monad (App, getConn)
import Infra.Db.Activity (DbActivity(..))

-- Get last notification timestamp
getLastNotificationTime :: App (Maybe UTCTime)
getLastNotificationTime = do
  conn <- getConn
  results <- liftIO $ query_ conn
    "SELECT last_notification_at FROM notification_state WHERE id = 'singleton'"
  pure $ case results of
    [Only t] -> t
    _ -> Nothing

-- Update last notification timestamp
updateLastNotificationTime :: UTCTime -> App ()
updateLastNotificationTime t = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "UPDATE notification_state SET last_notification_at = ? WHERE id = 'singleton'"
    (Only t)
  pure ()

-- Get activities that should be notified:
-- - Status = Surfaced OR urgency = 'high' OR sender in VIP list
-- - AND notified_at IS NULL
getNotifiableActivities :: [Text] -> App [Activity]
getNotifiableActivities vipEmails = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id, \
    \tags, parent_id \
    \FROM activities \
    \WHERE notified_at IS NULL \
    \  AND (status = 'surfaced' \
    \       OR urgency = 'high' \
    \       OR sender_email = ANY(?)) \
    \ORDER BY created_at DESC \
    \LIMIT 100"
    (Only (PGArray vipEmails))
  pure $ map unDbActivity results

-- Mark activities as notified
markActivitiesNotified :: [EntityId] -> App ()
markActivitiesNotified [] = pure ()
markActivitiesNotified ids = do
  conn <- getConn
  let idTexts = map unEntityId ids
  _ <- liftIO $ execute conn
    "UPDATE activities SET notified_at = NOW() WHERE id = ANY(?)"
    (Only (PGArray idTexts))
  pure ()
