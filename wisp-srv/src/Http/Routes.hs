module Http.Routes
  ( routes
  ) where

import Control.Monad.Reader (ReaderT)
import Web.Scotty.Trans (ScottyT, get)
import Http.Handlers.Health (getHealth)
import App.Monad (Env)

routes :: ScottyT (ReaderT Env IO) ()
routes = do
  get "/health" getHealth
