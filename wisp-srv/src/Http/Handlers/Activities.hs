-- src/Http/Handlers/Activities.hs
module Http.Handlers.Activities
  ( getActivities
  , getActivityById
  , getToday
  , triggerPoll
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=), Value)
import Data.Text (Text)
import Network.HTTP.Types.Status (status404)
import Web.Scotty.Trans (ActionT, json, status, pathParam)
import App.Monad (Env)
import Domain.Id (EntityId(..))
import Domain.Activity (Activity(..), ActivityStatus(..))
import Infra.Db.Activity (getActivitiesByStatus, getActivity, getActivitiesForToday)
import Services.Scheduler (runPollCycle)

-- Convert Activity to JSON (full details including classification)
activityToJson :: Activity -> Value
activityToJson a = object
  [ "id" .= unEntityId (activityId a)
  , "account_id" .= unEntityId (activityAccountId a)
  , "source" .= activitySource a
  , "source_id" .= activitySourceId a
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
  ]

-- GET /activities
getActivities :: ActionT (ReaderT Env IO) ()
getActivities = do
  -- Get recent pending activities
  activities <- lift $ getActivitiesByStatus Pending 50
  json $ object
    [ "activities" .= map activityToJson activities
    , "count" .= length activities
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

-- GET /today - Activities requiring attention today
getToday :: ActionT (ReaderT Env IO) ()
getToday = do
  activities <- lift $ getActivitiesForToday 50
  -- Group by status for the CLI
  let surfaced = [a | a <- activities, activityStatus a == Surfaced]
  let quarantined = [a | a <- activities, activityStatus a == Quarantined]
  let highUrgency = [a | a <- activities, activityStatus a == Pending]
  json $ object
    [ "surfaced" .= map activityToJson surfaced
    , "quarantined" .= map activityToJson quarantined
    , "high_urgency" .= map activityToJson highUrgency
    , "total" .= length activities
    ]

-- POST /poll
triggerPoll :: ActionT (ReaderT Env IO) ()
triggerPoll = do
  lift runPollCycle
  json $ object ["status" .= ("poll triggered" :: Text)]
