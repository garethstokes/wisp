module Http.Handlers.Pipeline
  ( postRunPipeline
  , postClassifyActivity
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Network.HTTP.Types.Status (status404, status500)
import Web.Scotty.Trans (ActionT, json, status, captureParam)
import App.Env (Env)
import Domain.Id (EntityId(..))
import Infra.Db.Activity (getActivity)
import Services.Pipeline (processPendingActivities, processActivity)

-- POST /pipeline/run - Run pipeline on pending activities
postRunPipeline :: ActionT (ReaderT Env IO) ()
postRunPipeline = do
  results <- lift $ processPendingActivities 100
  let successes = length [() | (_, Right _) <- results]
  let failures = length [() | (_, Left _) <- results]
  json $ object
    [ "processed" .= successes
    , "failed" .= failures
    , "total" .= length results
    ]

-- POST /activities/:id/classify - Manually classify a single activity
postClassifyActivity :: ActionT (ReaderT Env IO) ()
postClassifyActivity = do
  actId <- captureParam "id"
  mActivity <- lift $ getActivity (EntityId actId)
  case mActivity of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Activity not found" :: Text)]
    Just activity -> do
      result <- lift $ processActivity activity
      case result of
        Left err -> do
          status status500
          json $ object ["error" .= err]
        Right newStatus -> json $ object
          [ "status" .= ("classified" :: Text)
          , "new_status" .= show newStatus
          ]
