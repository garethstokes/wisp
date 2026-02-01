module Http.Routes
  ( routes
  ) where

import Control.Monad.Reader (ReaderT)
import Web.Scotty.Trans (ScottyT, get)
import Http.Handlers.Health (getHealth)
import Http.Handlers.Auth (getGoogleAuth, getGoogleCallback)
import App.Monad (Env)

routes :: ScottyT (ReaderT Env IO) ()
routes = do
  -- Health
  get "/health" getHealth

  -- Auth
  get "/auth/google" getGoogleAuth
  get "/auth/google/callback" getGoogleCallback
