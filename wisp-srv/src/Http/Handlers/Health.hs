module Http.Handlers.Health
  ( getHealth
  ) where

import Control.Monad.Reader (ReaderT)
import Data.Aeson (object, (.=))
import Web.Scotty.Trans (ActionT, json)
import App.Monad (Env)

getHealth :: ActionT (ReaderT Env IO) ()
getHealth = json $ object
  [ "status" .= ("ok" :: String)
  , "service" .= ("wisp-srv" :: String)
  ]
