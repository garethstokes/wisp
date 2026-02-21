-- src/Http/Handlers/Activities.hs
module Http.Handlers.Activities
  ( getActivities
  , getActivityStats
  , getActivityById
  , getActivityLogs
  , getInbox
  , getReview
  , approveActivity
  , dismissActivity
  , triggerPoll
  , backfillGitHubDiffs
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=), Value)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Network.HTTP.Types.Status (status404)
import Web.Scotty.Trans (ActionT, json, status, pathParam, queryParamMaybe)
import App.Monad (Env)
import Domain.Id (EntityId(..))
import Domain.Activity (Activity(..), ActivityStatus(..))
import Infra.Db.Activity (getActivitiesByStatus, countActivitiesByStatus, getActivity, getActivitiesForToday, getTodaysCalendarEvents, updateActivityStatus, getActivitiesPaginated, getActivityCountsBySource)
import Infra.Db.Receipt (getReceiptsForActivity)
import Services.Scheduler (runPollCycle)

-- Convert Activity to JSON (full details including classification)
activityToJson :: Activity -> Value
activityToJson a = object
  [ "id" .= unEntityId (activityId a)
  , "account_id" .= unEntityId (activityAccountId a)
  , "source" .= activitySource a
  , "source_id" .= activitySourceId a
  , "raw" .= activityRaw a
  , "status" .= activityStatus a
  , "title" .= activityTitle a
  , "summary" .= activitySummary a
  , "sender_email" .= activitySenderEmail a
  , "starts_at" .= activityStartsAt a
  , "ends_at" .= activityEndsAt a
  , "created_at" .= activityCreatedAt a
  , "personas" .= activityPersonas a
  , "activity_type" .= activityType a
  , "urgency" .= activityUrgency a
  , "autonomy_tier" .= activityAutonomyTier a
  , "confidence" .= activityConfidence a
  , "person_id" .= fmap unEntityId (activityPersonId a)
  , "tags" .= activityTags a
  ]

-- GET /activities?limit=50&offset=0
getActivities :: ActionT (ReaderT Env IO) ()
getActivities = do
  -- Parse pagination params
  mLimit <- queryParamMaybe "limit"
  mOffset <- queryParamMaybe "offset"
  let limit = fromMaybe 50 (mLimit >>= parseIntParam)
      offset = fromMaybe 0 (mOffset >>= parseIntParam)
  -- Get paginated activities
  activities <- lift $ getActivitiesPaginated limit offset
  -- Get counts by source (total and last 24 hours) - only on first page
  sourceCounts <- if offset == 0
    then lift getActivityCountsBySource
    else pure []
  let metrics = if offset == 0
        then Just $ object
          [ "by_source" .= [ object
              [ "source" .= src
              , "total" .= total
              , "recent" .= recent
              ]
            | (src, total, recent) <- sourceCounts
            ]
          , "total" .= sum [total | (_, total, _) <- sourceCounts]
          , "recent" .= sum [recent | (_, _, recent) <- sourceCounts]
          ]
        else Nothing
  json $ object $
    [ "activities" .= map activityToJson activities
    , "count" .= length activities
    , "offset" .= offset
    , "has_more" .= (length activities == limit)
    ] ++ maybe [] (\m -> ["metrics" .= m]) metrics

-- Parse an integer from Text
parseIntParam :: Text -> Maybe Int
parseIntParam t = case TR.decimal t of
  Right (n, rest) | T.null rest -> Just n
  _ -> Nothing

-- GET /activities/stats - Get counts by status
getActivityStats :: ActionT (ReaderT Env IO) ()
getActivityStats = do
  pending <- lift $ countActivitiesByStatus Pending
  needsReview <- lift $ countActivitiesByStatus NeedsReview
  quarantined <- lift $ countActivitiesByStatus Quarantined
  surfaced <- lift $ countActivitiesByStatus Surfaced
  processed <- lift $ countActivitiesByStatus Processed
  json $ object
    [ "pending" .= pending
    , "needs_review" .= needsReview
    , "quarantined" .= quarantined
    , "surfaced" .= surfaced
    , "processed" .= processed
    ]

-- GET /activities/:id
getActivityById :: ActionT (ReaderT Env IO) ()
getActivityById = do
  aid <- pathParam "id"
  mactivity <- lift $ getActivity (EntityId aid)
  case mactivity of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Activity not found" :: Text)]
    Just activity -> json $ activityToJson activity

-- GET /activities/:id/logs - Get processing history for an activity
getActivityLogs :: ActionT (ReaderT Env IO) ()
getActivityLogs = do
  aid <- pathParam "id"
  mactivity <- lift $ getActivity (EntityId aid)
  case mactivity of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Activity not found" :: Text)]
    Just activity -> do
      receipts <- lift $ getReceiptsForActivity (activityId activity)
      json $ object
        [ "activity_id" .= unEntityId (activityId activity)
        , "logs" .= receipts
        , "count" .= length receipts
        ]

-- GET /inbox - Activities requiring attention + today's schedule
getInbox :: ActionT (ReaderT Env IO) ()
getInbox = do
  activities <- lift $ getActivitiesForToday 50
  calendar <- lift $ getTodaysCalendarEvents
  -- Group by status for the CLI
  let surfaced = [a | a <- activities, activityStatus a == Surfaced]
  let quarantined = [a | a <- activities, activityStatus a == Quarantined]
  let highUrgency = [a | a <- activities, activityStatus a == Pending]
  json $ object
    [ "surfaced" .= map activityToJson surfaced
    , "quarantined" .= map activityToJson quarantined
    , "high_urgency" .= map activityToJson highUrgency
    , "calendar" .= map activityToJson calendar
    , "total" .= length activities
    ]

-- GET /review - Activities needing review (tier 3)
getReview :: ActionT (ReaderT Env IO) ()
getReview = do
  activities <- lift $ getActivitiesByStatus NeedsReview 100
  totalCount <- lift $ countActivitiesByStatus NeedsReview
  json $ object
    [ "activities" .= map activityToJson activities
    , "count" .= length activities
    , "total" .= totalCount
    ]

-- POST /activities/:id/approve - Move quarantined to surfaced
approveActivity :: ActionT (ReaderT Env IO) ()
approveActivity = do
  aid <- pathParam "id"
  mactivity <- lift $ getActivity (EntityId aid)
  case mactivity of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Activity not found" :: Text)]
    Just activity -> do
      lift $ updateActivityStatus (activityId activity) Surfaced
      json $ object
        [ "status" .= ("approved" :: Text)
        , "id" .= unEntityId (activityId activity)
        , "new_status" .= ("surfaced" :: Text)
        ]

-- POST /activities/:id/dismiss - Archive an activity
dismissActivity :: ActionT (ReaderT Env IO) ()
dismissActivity = do
  aid <- pathParam "id"
  mactivity <- lift $ getActivity (EntityId aid)
  case mactivity of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Activity not found" :: Text)]
    Just activity -> do
      lift $ updateActivityStatus (activityId activity) Archived
      json $ object
        [ "status" .= ("dismissed" :: Text)
        , "id" .= unEntityId (activityId activity)
        , "new_status" .= ("archived" :: Text)
        ]

-- POST /poll
triggerPoll :: ActionT (ReaderT Env IO) ()
triggerPoll = do
  lift runPollCycle
  json $ object ["status" .= ("poll triggered" :: Text)]

-- POST /admin/backfill-github-diffs - Backfill diffs for existing PushEvents
backfillGitHubDiffs :: ActionT (ReaderT Env IO) ()
backfillGitHubDiffs = do
  -- This will call backfillPushEventDiffs from GitHubPoller (implemented in Task 5)
  json $ object
    [ "status" .= ("backfill endpoint ready" :: Text)
    , "message" .= ("Implementation pending in Task 5" :: Text)
    ]
