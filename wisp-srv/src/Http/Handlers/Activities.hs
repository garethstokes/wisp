-- src/Http/Handlers/Activities.hs
module Http.Handlers.Activities
  ( getActivities
  , getActivityById
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
import Infra.Db.Activity (getActivitiesByStatus, getActivity)

-- Convert Activity to JSON
activityToJson :: Activity -> Value
activityToJson a = object
  [ "id" .= unEntityId (activityId a)
  , "source" .= activitySource a
  , "source_id" .= activitySourceId a
  , "status" .= activityStatus a
  , "title" .= activityTitle a
  , "summary" .= activitySummary a
  , "sender_email" .= activitySenderEmail a
  , "starts_at" .= activityStartsAt a
  , "ends_at" .= activityEndsAt a
  , "created_at" .= activityCreatedAt a
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
