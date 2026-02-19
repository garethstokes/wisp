module Http.Routes
  ( routes
  ) where

import Control.Monad.Reader (ReaderT)
import Web.Scotty.Trans (ScottyT, get, post, put)
import Http.Handlers.Agents (getAgentsList, getAgentByName, postAgentActivateSkill, postAgentDeactivate)
import Http.Handlers.Auth (getGoogleAuth, getGoogleCallback, getGitHubAuth, getGitHubCallback, getAuthStatus)
import Http.Handlers.Health (getHealth)
import Http.Handlers.Activities (getActivities, getActivityStats, getActivityById, getActivityLogs, getInbox, getReview, approveActivity, dismissActivity, triggerPoll)
import Http.Handlers.Chat (postChat)
import Http.Handlers.People (getPeople, getPersonById)
import Http.Handlers.Pipeline (postRunPipeline, postClassifyActivity)
import Http.Handlers.Runs (getRuns, getRunById)
import Http.Handlers.Skills (getSkillsList, getSkillByName, putSkillPrompt)
import Http.Handlers.Tenants (getTenantsList, postTenant, getTenantById)
import Http.Handlers.Documents (getProjectsList, postProject, postProjectArchive, getNotesList, postNote, getPrefsList, postPref, getDocumentById, getDocumentLogHandler)
import App.Monad (Env)

routes :: ScottyT (ReaderT Env IO) ()
routes = do
  -- Health
  get "/health" getHealth

  -- Agents
  get "/api/agents" getAgentsList
  get "/api/agents/:name" getAgentByName
  post "/api/agents/:name/activate/:skill" postAgentActivateSkill
  post "/api/agents/:name/deactivate" postAgentDeactivate

  -- Skills (new API)
  get "/api/skills" getSkillsList
  get "/api/skills/:name" getSkillByName
  put "/api/skills/:name" putSkillPrompt

  -- Tenants
  get "/api/tenants" getTenantsList
  post "/api/tenants" postTenant
  get "/api/tenants/:id" getTenantById

  -- Auth
  get "/auth/google" getGoogleAuth
  get "/auth/google/callback" getGoogleCallback
  get "/auth/github" getGitHubAuth
  get "/auth/github/callback" getGitHubCallback
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

  -- Documents
  get "/api/projects" getProjectsList
  post "/api/projects" postProject
  post "/api/projects/:id/archive" postProjectArchive
  get "/api/notes" getNotesList
  post "/api/notes" postNote
  get "/api/preferences" getPrefsList
  post "/api/preferences" postPref
  get "/api/documents/:id" getDocumentById
  get "/api/documents/:id/log" getDocumentLogHandler
