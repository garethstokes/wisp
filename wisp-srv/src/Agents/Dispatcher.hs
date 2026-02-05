module Agents.Dispatcher
  ( allAgents
  , getAgent
  , dispatchChat
  ) where

import Data.Text (Text)
import Domain.Agent (AgentInfo(..))
import Domain.Chat (ChatMessage, ChatResponse)
import App.Monad (App)
import qualified Agents.Concierge as Concierge
import qualified Agents.Scheduler as Scheduler
import qualified Agents.Housekeeper as Housekeeper
import qualified Agents.Insights as Insights

allAgents :: [AgentInfo]
allAgents =
  [ Concierge.agentInfo
  , Scheduler.agentInfo
  , Housekeeper.agentInfo
  , Insights.agentInfo
  ]

getAgent :: Text -> Maybe AgentInfo
getAgent aid = case [a | a <- allAgents, agentId a == aid] of
  [a] -> Just a
  _ -> Nothing

-- | Dispatch chat to the appropriate agent
-- timezone: Optional IANA timezone for converting dates to local time in agent context
dispatchChat :: Text -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
dispatchChat "wisp/concierge" msgs tz = Concierge.handleChat msgs tz
dispatchChat "wisp/scheduler" msgs tz = Scheduler.handleChat msgs tz
dispatchChat "wisp/housekeeper" _ _ = pure $ Left "Agent 'wisp/housekeeper' not yet implemented"
dispatchChat "wisp/insights" msgs tz = Insights.handleChat msgs tz
dispatchChat agent _ _ = pure $ Left $ "Unknown agent: " <> agent
