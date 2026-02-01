module Http.Routes
  ( routes
  ) where

import Control.Monad.Reader (ReaderT)
import Web.Scotty.Trans (ScottyT, get, post)
import Http.Handlers.Health (getHealth)
import Http.Handlers.Auth (getGoogleAuth, getGoogleCallback, getAuthStatus)
import Http.Handlers.Activities (getActivities, getActivityById, triggerPoll)
import App.Monad (Env)

routes :: ScottyT (ReaderT Env IO) ()
routes = do
  -- Health
  get "/health" getHealth

  -- Auth
  get "/auth/google" getGoogleAuth
  get "/auth/google/callback" getGoogleCallback
  get "/auth/status" getAuthStatus

  -- Activities
  get "/activities" getActivities
  get "/activities/:id" getActivityById

  -- Poll trigger
  post "/poll" triggerPoll
