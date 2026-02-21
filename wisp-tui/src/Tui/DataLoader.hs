module Tui.DataLoader
  ( loadActivities
  , loadMoreActivities
  , loadKnowledge
  , loadSkills
  , loadAgents
  , loadAgentSessions
  , loadApprovals
  , DataLoadResult(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Wisp.Client
  ( ClientConfig
  , ClientError(..)
  , Activity
  , ActivityMetrics
  , Document
  , Skill
  , AgentInfo
  , SessionSummary
  , ApprovalItem(..)
  , ActivitiesResponse(..)
  , getActivities
  , getActivitiesPage
  , getNotes
  , getPreferences
  , getApprovals
  , getSkills
  , getAgents
  , getAgent
  , getAgentSessions
  )

-- | Result of a data load operation
data DataLoadResult
  = ActivitiesLoaded [Activity] (Maybe ActivityMetrics) Bool  -- activities, metrics, hasMore
  | ActivitiesAppended [Activity] Bool  -- more activities, hasMore
  | KnowledgeLoaded [Document] [Document]  -- notes, prefs
  | SkillsLoaded [Skill]
  | AgentsLoaded [AgentInfo]
  | AgentSessionsLoaded Text [SessionSummary]
  | ApprovalsLoaded [(Activity, Text, Text)]
  | LoadError Text
  deriving (Show, Eq)

-- | Load activities from server (first page)
loadActivities :: ClientConfig -> IO DataLoadResult
loadActivities cfg = do
  result <- getActivities cfg
  pure $ case result of
    Left err -> LoadError $ "Failed to load activities: " <> showError err
    Right resp -> ActivitiesLoaded (arActivities resp) (arMetrics resp) (arHasMore resp)

-- | Load more activities (pagination)
loadMoreActivities :: ClientConfig -> Int -> IO DataLoadResult
loadMoreActivities cfg offset = do
  result <- getActivitiesPage cfg 50 offset
  pure $ case result of
    Left err -> LoadError $ "Failed to load more activities: " <> showError err
    Right resp -> ActivitiesAppended (arActivities resp) (arHasMore resp)

-- | Load knowledge (notes, prefs) from server
loadKnowledge :: ClientConfig -> IO DataLoadResult
loadKnowledge cfg = do
  notesResult <- getNotes cfg
  prefsResult <- getPreferences cfg

  let notes = either (const []) id notesResult
      prefs = either (const []) id prefsResult

  pure $ KnowledgeLoaded notes prefs

-- | Load skills from server
loadSkills :: ClientConfig -> IO DataLoadResult
loadSkills cfg = do
  result <- getSkills cfg
  pure $ case result of
    Left err -> LoadError $ "Failed to load skills: " <> showError err
    Right skills -> SkillsLoaded skills

-- | Load agents from server
loadAgents :: ClientConfig -> IO DataLoadResult
loadAgents cfg = do
  result <- getAgents cfg
  case result of
    Left err -> pure $ LoadError $ "Failed to load agents: " <> showError err
    Right agentNames -> do
      -- Fetch full info for each agent
      agents <- mapM (getAgent cfg) agentNames
      pure $ AgentsLoaded [a | Right a <- agents]

-- | Load sessions for a specific agent
loadAgentSessions :: ClientConfig -> Text -> IO DataLoadResult
loadAgentSessions cfg agentName = do
  result <- getAgentSessions cfg agentName
  pure $ case result of
    Left err -> LoadError $ "Failed to load sessions: " <> showError err
    Right sessions -> AgentSessionsLoaded agentName sessions

-- | Load approvals from server
loadApprovals :: ClientConfig -> IO DataLoadResult
loadApprovals cfg = do
  result <- getApprovals cfg
  pure $ case result of
    Left err -> LoadError $ "Failed to load approvals: " <> showError err
    Right items -> ApprovalsLoaded
      [ (approvalActivity item, approvalType item, approvalReason item)
      | item <- items
      ]

showError :: ClientError -> Text
showError (HttpError t) = t
showError (ParseError t) = t
showError (ServerError code t) = "Server error " <> T.pack (show code) <> ": " <> t
