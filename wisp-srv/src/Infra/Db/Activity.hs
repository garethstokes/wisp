-- src/Infra/Db/Activity.hs
module Infra.Db.Activity
  ( insertActivity
  , insertConversation
  , activityExists
  , activityExistsForAccount
  , getActivity
  , getActivitiesByStatus
  , getActivitiesFiltered
  , countActivitiesByStatus
  , getActivitiesForToday
  , getRecentActivities
  , getTodaysCalendarEvents
  , getCalendarEventsInRange
  , getUpcomingCalendarEvents
  , getPendingEmails
  , updateActivityStatus
  , updateActivityClassification
  , searchActivities
  , getActivitySummaryStats
  , DbActivity(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson ()
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (PGArray(..), Query(..))
import Domain.Id (EntityId(..), newEntityId)
import Domain.Activity
import Domain.Classification (Classification(..), ActivityType(..), Urgency(..))
import App.Monad (App, getConn)

-- Newtype wrapper to define FromRow instance without orphan warning
newtype DbActivity = DbActivity { unDbActivity :: Activity }

instance FromRow DbActivity where
  fromRow = fmap DbActivity $ Activity
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
      parseStatus "needs_review" = NeedsReview
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
    [a] -> Just (unDbActivity a)
    _ -> Nothing

-- Get activities for "today" view: surfaced, high-urgency pending, and quarantined
getActivitiesForToday :: Int -> App [Activity]
getActivitiesForToday limit = do
  conn <- getConn
  results <- liftIO $ query conn
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
  pure $ map unDbActivity results

-- Get activities from the last N hours
getRecentActivities :: Int -> App [Activity]
getRecentActivities hours = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where created_at > now() - interval '1 hour' * ? \
    \order by created_at desc \
    \limit 50"
    (Only hours)
  pure $ map unDbActivity results

-- Get today's calendar events
getTodaysCalendarEvents :: App [Activity]
getTodaysCalendarEvents = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where source = 'calendar' \
    \  and starts_at >= date_trunc('day', now()) \
    \  and starts_at < date_trunc('day', now()) + interval '1 day' \
    \order by starts_at"
    ()
  pure $ map unDbActivity results

-- Get calendar events within a date range
getCalendarEventsInRange :: UTCTime -> UTCTime -> App [Activity]
getCalendarEventsInRange startTime endTime = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where source = 'calendar' \
    \  and starts_at >= ? \
    \  and starts_at < ? \
    \order by starts_at"
    (startTime, endTime)
  pure $ map unDbActivity results

-- Get upcoming calendar events (next N days)
getUpcomingCalendarEvents :: Int -> App [Activity]
getUpcomingCalendarEvents days = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where source = 'calendar' \
    \  and starts_at >= now() \
    \  and starts_at < now() + interval '1 day' * ? \
    \order by starts_at"
    (Only days)
  pure $ map unDbActivity results

-- Search activities by title/summary text
searchActivities :: Text -> Int -> App [Activity]
searchActivities searchTerm limit = do
  conn <- getConn
  let pattern = "%" <> searchTerm <> "%"
  results <- liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where title ilike ? or summary ilike ? or sender_email ilike ? \
    \order by created_at desc \
    \limit ?"
    (pattern, pattern, pattern, limit)
  pure $ map unDbActivity results

-- Get activity counts grouped by status and source
getActivitySummaryStats :: App [(Text, Text, Int)]
getActivitySummaryStats = do
  conn <- getConn
  liftIO $ query_ conn
    "select source, status, count(*)::int \
    \from activities \
    \group by source, status \
    \order by source, status"

-- Get pending emails (for chat context)
getPendingEmails :: Int -> App [Activity]
getPendingEmails limit = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where source = 'email' and status = 'pending' \
    \order by created_at desc \
    \limit ?"
    (Only limit)
  pure $ map unDbActivity results

-- Get activities by status
getActivitiesByStatus :: ActivityStatus -> Int -> App [Activity]
getActivitiesByStatus st limit = do
  conn <- getConn
  let statusText = case st of
        Pending -> "pending" :: Text
        NeedsReview -> "needs_review"
        Quarantined -> "quarantined"
        Processed -> "processed"
        Surfaced -> "surfaced"
        Archived -> "archived"
  results <- liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities where status = ? \
    \order by created_at desc limit ?"
    (statusText, limit)
  pure $ map unDbActivity results

-- Get activities with flexible date filters
getActivitiesFiltered :: Maybe ActivityStatus -> Maybe UTCTime -> Maybe UTCTime -> Int -> App [Activity]
getActivitiesFiltered mStatus mSince mBefore limit = do
  conn <- getConn
  let baseQuery = "select id, account_id, source, source_id, raw, status, title, summary, \
                  \sender_email, starts_at, ends_at, created_at, \
                  \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
                  \from activities where 1=1"
      statusClause = case mStatus of
        Just st -> " and status = '" <> statusToText st <> "'"
        Nothing -> ""
      sinceClause = case mSince of
        Just _ -> " and created_at >= ?"
        Nothing -> ""
      beforeClause = case mBefore of
        Just _ -> " and created_at < ?"
        Nothing -> ""
      orderClause = " order by created_at desc limit ?"
      fullQuery = baseQuery <> statusClause <> sinceClause <> beforeClause <> orderClause
  -- Build params based on which filters are present
  results <- case (mSince, mBefore) of
    (Just since, Just before) ->
      liftIO $ query conn (Query $ encodeUtf8 fullQuery) (since, before, limit)
    (Just since, Nothing) ->
      liftIO $ query conn (Query $ encodeUtf8 fullQuery) (since, limit)
    (Nothing, Just before) ->
      liftIO $ query conn (Query $ encodeUtf8 fullQuery) (before, limit)
    (Nothing, Nothing) ->
      liftIO $ query conn (Query $ encodeUtf8 fullQuery) (Only limit)
  pure $ map unDbActivity results
  where
    statusToText :: ActivityStatus -> Text
    statusToText Pending = "pending"
    statusToText NeedsReview = "needs_review"
    statusToText Quarantined = "quarantined"
    statusToText Processed = "processed"
    statusToText Surfaced = "surfaced"
    statusToText Archived = "archived"

-- Count activities by status (no limit)
countActivitiesByStatus :: ActivityStatus -> App Int
countActivitiesByStatus status = do
  conn <- getConn
  let statusText = case status of
        Pending -> "pending" :: Text
        NeedsReview -> "needs_review"
        Quarantined -> "quarantined"
        Processed -> "processed"
        Surfaced -> "surfaced"
        Archived -> "archived"
  results <- liftIO $ query conn
    "select count(*) from activities where status = ?"
    (Only statusText)
  pure $ case results of
    [Only n] -> n
    _ -> 0

-- Update activity status
updateActivityStatus :: EntityId -> ActivityStatus -> App ()
updateActivityStatus aid status = do
  conn <- getConn
  let statusText = case status of
        Pending -> "pending" :: Text
        NeedsReview -> "needs_review"
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
