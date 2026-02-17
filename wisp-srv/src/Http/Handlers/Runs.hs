module Http.Handlers.Runs
  ( getRuns
  , getRunById
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Network.HTTP.Types.Status (status404)
import Web.Scotty.Trans (ActionT, json, status, pathParam)
import App.Monad (Env)
import Domain.Run (RunId(..))
import Infra.Db.Run (getRecentRuns, getRun)

-- GET /runs - List recent runs
getRuns :: ActionT (ReaderT Env IO) ()
getRuns = do
  runs <- lift $ getRecentRuns 20
  json $ object
    [ "runs" .= runs
    , "count" .= length runs
    ]

-- GET /runs/:id - Get full run with events
getRunById :: ActionT (ReaderT Env IO) ()
getRunById = do
  rid <- pathParam "id"
  mrun <- lift $ getRun (RunId rid)
  case mrun of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Run not found" :: Text)]
    Just run -> json run
