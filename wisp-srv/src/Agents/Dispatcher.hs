module Agents.Dispatcher
  ( -- Legacy skill-based routing
    allAgents
  , getAgent
  , dispatchChat
    -- New agent-based routing
  , listAgentNames
  ) where

import Data.Text (Text)
import Domain.Agent (AgentInfo(..), AgentName, parseAgentTag)
import Domain.Chat (ChatMessage, ChatResponse)
import Domain.Id (EntityId)
import App.Monad (App)
import qualified Skills.Concierge as Concierge
import qualified Skills.Scheduler as Scheduler
import qualified Skills.Housekeeper as Housekeeper
import qualified Skills.Insights as Insights
import Agents.Run (withRunLogging)
import Infra.Db.Activity (searchTags)

-- | All skill-based agent infos (legacy)
allAgents :: [AgentInfo]
allAgents =
  [ Concierge.agentInfo
  , Scheduler.agentInfo
  , Housekeeper.agentInfo
  , Insights.agentInfo
  ]

-- | Get skill-based agent info by ID (legacy)
getAgent :: Text -> Maybe AgentInfo
getAgent aid = case [a | a <- allAgents, agentId a == aid] of
  [a] -> Just a
  _ -> Nothing

-- | Dispatch chat to the appropriate agent
-- Supports both legacy "wisp/skill" format and new agent names
-- timezone: Optional IANA timezone for converting dates to local time in agent context
dispatchChat :: Text -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
dispatchChat agent msgs tz = withRunLogging agent Nothing msgs $ \ctx messages ->
  case agent of
    -- Legacy skill-based routing
    "wisp/concierge" -> Concierge.handleChatWithContext ctx messages tz
    "wisp/scheduler" -> Scheduler.handleChatWithContext ctx messages tz
    "wisp/housekeeper" -> pure $ Left "Agent 'wisp/housekeeper' not yet implemented"
    "wisp/insights" -> Insights.handleChatWithContext ctx messages tz
    -- New agent routing will be handled here once agent chat is implemented
    _ -> pure $ Left $ "Unknown agent: " <> agent

-- | List available agent names from knowledge
-- Finds all tags matching "agent:*" pattern and extracts agent names
listAgentNames :: EntityId -> App [AgentName]
listAgentNames accountId = do
  -- Search for tags starting with "agent:"
  tags <- searchTags accountId "agent:" 100
  pure $ extractAgentNames tags
  where
    extractAgentNames :: [Text] -> [AgentName]
    extractAgentNames = foldr addIfAgent []

    addIfAgent :: Text -> [AgentName] -> [AgentName]
    addIfAgent tag acc = case parseAgentTag tag of
      Just name -> name : acc
      Nothing -> acc
