-- src/Infra/Db/Activity.hs
module Infra.Db.Activity
  ( insertActivity
  , insertConversation
  , activityExists
  , activityExistsForAccount
  , getActivity
  , getActivitiesByStatus
  , getActivitiesForToday
  , getRecentActivities
  , getTodaysCalendarEvents
  , getPendingEmails
  , updateActivityStatus
  , updateActivityClassification
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson ()
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Domain.Id (EntityId(..), newEntityId)
import Domain.Activity
import Domain.Classification (Classification(..), ActivityType(..), Urgency(..))
import App.Monad (App, getConn)

instance FromRow Activity where
  fromRow = Activity
    <$> (EntityId <$> field)          -- id
    <*> (EntityId <$> field)          -- account_id
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
    <*> (fmap fromPGArray <$> field)  -- personas
    <*> field                          -- activity_type
    <*> field                          -- urgency
    <*> field                          -- autonomy_tier
    <*> field                          -- confidence
    <*> (fmap EntityId <$> field)     -- person_id
    where
      parseSource :: Text -> ActivitySource
      parseSource "email" = Email
      parseSource "calendar" = Calendar
      parseSource "conversation" = Conversation
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
        Conversation -> "conversation"
  n <- liftIO $ execute conn
    "insert into activities \
    \(id, account_id, source, source_id, raw, title, sender_email, starts_at, ends_at) \
    \values (?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \on conflict (account_id, source, source_id) do nothing"
    ( unEntityId aid
    , unEntityId (newActivityAccountId new)
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
        Conversation -> "conversation"
  results <- liftIO $ query conn
    "select 1 from activities where source = ? and source_id = ? limit 1"
    (srcText, srcId)
  pure $ not (null (results :: [Only Int]))

-- Get activity by ID
getActivity :: EntityId -> App (Maybe Activity)
getActivity aid = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities where id = ?"
    (Only $ unEntityId aid)
  pure $ case results of
    [a] -> Just a
    _ -> Nothing

-- Get activities for "today" view: surfaced, high-urgency pending, and quarantined
getActivitiesForToday :: Int -> App [Activity]
getActivitiesForToday limit = do
  conn <- getConn
  liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where status = 'surfaced' \
    \   or status = 'quarantined' \
    \   or (status = 'pending' and urgency = 'high') \
    \order by \
    \  case status \
    \    when 'quarantined' then 1 \
    \    when 'surfaced' then 2 \
    \    else 3 \
    \  end, \
    \  case urgency \
    \    when 'high' then 1 \
    \    when 'normal' then 2 \
    \    else 3 \
    \  end, \
    \  created_at desc \
    \limit ?"
    (Only limit)

-- Get activities from the last N hours
getRecentActivities :: Int -> App [Activity]
getRecentActivities hours = do
  conn <- getConn
  liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where created_at > now() - interval '1 hour' * ? \
    \order by created_at desc \
    \limit 50"
    (Only hours)

-- Get today's calendar events
getTodaysCalendarEvents :: App [Activity]
getTodaysCalendarEvents = do
  conn <- getConn
  liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where source = 'calendar' \
    \  and starts_at >= date_trunc('day', now()) \
    \  and starts_at < date_trunc('day', now()) + interval '1 day' \
    \order by starts_at"
    ()

-- Get pending emails (for chat context)
getPendingEmails :: Int -> App [Activity]
getPendingEmails limit = do
  conn <- getConn
  liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where source = 'email' and status = 'pending' \
    \order by created_at desc \
    \limit ?"
    (Only limit)

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
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
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

-- Check if activity exists for an account
activityExistsForAccount :: EntityId -> ActivitySource -> Text -> App Bool
activityExistsForAccount accountId src srcId = do
  conn <- getConn
  let srcText = case src of
        Email -> "email" :: Text
        Calendar -> "calendar"
        Conversation -> "conversation"
  results <- liftIO $ query conn
    "select 1 from activities where account_id = ? and source = ? and source_id = ? limit 1"
    (unEntityId accountId, srcText, srcId)
  pure $ not (null (results :: [Only Int]))

-- Update activity with classification data
updateActivityClassification :: EntityId -> Classification -> Maybe EntityId -> App ()
updateActivityClassification aid classification mPersonId = do
  conn <- getConn
  let typeText = case classificationActivityType classification of
        Request -> "request" :: Text
        Information -> "information"
        ActionRequired -> "action_required"
        FYI -> "fyi"
        Event -> "event"
  let urgencyText = case classificationUrgency classification of
        High -> "high" :: Text
        Normal -> "normal"
        Low -> "low"
  _ <- liftIO $ execute conn
    "update activities set \
    \  personas = ?, activity_type = ?, urgency = ?, \
    \  autonomy_tier = ?, confidence = ?, summary = ?, \
    \  person_id = ?, updated_at = now() \
    \where id = ?"
    ( PGArray (classificationPersonas classification)
    , typeText
    , urgencyText
    , classificationAutonomyTier classification
    , classificationConfidence classification
    , classificationSummary classification
    , fmap unEntityId mPersonId
    , unEntityId aid
    )
  pure ()

-- Insert a conversation log
insertConversation :: Text -> Text -> App EntityId
insertConversation chatQuery response = do
  conn <- getConn
  aid <- liftIO newEntityId
  _ <- liftIO $ execute conn
    "insert into activities \
    \(id, account_id, source, source_id, raw, title, summary, status) \
    \values (?, (select id from accounts limit 1), 'conversation', ?, '{}', ?, ?, 'processed')"
    ( unEntityId aid
    , "chat-" <> unEntityId aid
    , "Chat: " <> T.take 50 chatQuery
    , response
    )
  pure aid
