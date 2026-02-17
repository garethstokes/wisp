module Http.Routes
  ( routes
  ) where

import Control.Monad.Reader (ReaderT)
import Web.Scotty.Trans (ScottyT, get, post)
import Http.Handlers.Agents (getAgents)
import Http.Handlers.Auth (getGoogleAuth, getGoogleCallback, getAuthStatus)
import Http.Handlers.Health (getHealth)
import Http.Handlers.Activities (getActivities, getActivityStats, getActivityById, getActivityLogs, getInbox, getReview, approveActivity, dismissActivity, triggerPoll)
import Http.Handlers.Chat (postChat)
import Http.Handlers.People (getPeople, getPersonById)
import Http.Handlers.Pipeline (postRunPipeline, postClassifyActivity)
import Http.Handlers.Runs (getRuns, getRunById)
import App.Monad (Env)

routes :: ScottyT (ReaderT Env IO) ()
routes = do
  -- Health
  get "/health" getHealth

  -- Agents
  get "/agents" getAgents

  -- Auth
  get "/auth/google" getGoogleAuth
  get "/auth/google/callback" getGoogleCallback
  get "/auth/status" getAuthStatus

  -- Activities
  get "/activities" getActivities
  get "/activities/stats" getActivityStats
  get "/activities/:id" getActivityById
  get "/activities/:id/logs" getActivityLogs
  get "/inbox" getInbox
  get "/review" getReview

  -- Poll trigger
  post "/poll" triggerPoll

  -- Activity actions
  post "/activities/:id/approve" approveActivity
  post "/activities/:id/dismiss" dismissActivity

  -- People
  get "/people" getPeople
  get "/people/:id" getPersonById

  -- Chat
  post "/chat" postChat

  -- Classification pipeline
  post "/pipeline/run" postRunPipeline
  post "/activities/:id/classify" postClassifyActivity

  -- Agent runs
  get "/runs" getRuns
  get "/runs/:id" getRunById
