module Tui.DataLoader
  ( loadActivities
  , loadMoreActivities
  , loadKnowledge
  , loadSkills
  , loadAgents
  , loadAgentSessions
  , loadChatSession
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
  , ActiveSession(..)
  , ChatMessage(..)
  , ApprovalItem(..)
  , ProjectSuggestion
  , ActivitiesResponse(..)
  , getActivities
  , getActivitiesPage
  , getProjects
  , getNotes
  , getPreferences
  , getApprovals
  , getProjectSuggestions
  , getSkills
  , getAgents
  , getAgent
  , getAgentSessions
  , getActiveSession
  )

-- | Result of a data load operation
data DataLoadResult
  = ActivitiesLoaded [Activity] (Maybe ActivityMetrics) Bool  -- activities, metrics, hasMore
  | ActivitiesAppended [Activity] Bool  -- more activities, hasMore
  | KnowledgeLoaded [Document] [Document] [Document]  -- projects, notes, prefs
  | SkillsLoaded [Skill]
  | AgentsLoaded [AgentInfo]
  | AgentSessionsLoaded Text [SessionSummary]
  | ChatSessionLoaded (Maybe (Text, [ChatMessage]))  -- sessionId, messages
  | ApprovalsLoaded [(Activity, Text, Text)] [ProjectSuggestion]
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

-- | Load knowledge (projects, notes, prefs) from server
loadKnowledge :: ClientConfig -> IO DataLoadResult
loadKnowledge cfg = do
  projectsResult <- getProjects cfg
  notesResult <- getNotes cfg
  prefsResult <- getPreferences cfg

  let projects = either (const []) id projectsResult
      notes = either (const []) id notesResult
      prefs = either (const []) id prefsResult

  pure $ KnowledgeLoaded projects notes prefs

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

-- | Load active chat session for an agent (for TUI resume)
loadChatSession :: ClientConfig -> Text -> IO DataLoadResult
loadChatSession cfg agentName = do
  result <- getActiveSession cfg agentName
  pure $ case result of
    Left err -> LoadError $ "Failed to load session: " <> showError err
    Right Nothing -> ChatSessionLoaded Nothing
    Right (Just session) -> ChatSessionLoaded $ Just (asId session, asMessages session)

-- | Load approvals from server (also fetches project suggestions)
loadApprovals :: ClientConfig -> IO DataLoadResult
loadApprovals cfg = do
  approvalsResult <- getApprovals cfg
  suggestionsResult <- getProjectSuggestions cfg
  pure $ case approvalsResult of
    Left err -> LoadError $ "Failed to load approvals: " <> showError err
    Right items ->
      let approvalItems =
            [ (approvalActivity item, approvalType item, approvalReason item)
            | item <- items
            ]
          suggestions = either (const []) id suggestionsResult
      in ApprovalsLoaded approvalItems suggestions

showError :: ClientError -> Text
showError (HttpError t) = t
showError (ParseError t) = t
showError (ServerError code t) = "Server error " <> T.pack (show code) <> ": " <> t
