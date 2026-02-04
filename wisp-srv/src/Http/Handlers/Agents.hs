module Http.Handlers.Agents
  ( getAgents
  ) where

import Control.Monad.Reader (ReaderT)
import Data.Aeson (object, (.=))
import Web.Scotty.Trans (ActionT, json)
import App.Monad (Env)
import Agents.Dispatcher (allAgents)

-- GET /agents
getAgents :: ActionT (ReaderT Env IO) ()
getAgents = json $ object ["agents" .= allAgents]
