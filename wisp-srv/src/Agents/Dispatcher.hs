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
import Agents.Run (withRunLogging)

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
dispatchChat agent msgs tz = withRunLogging agent Nothing msgs $ \messages ->
  case agent of
    "wisp/concierge" -> Concierge.handleChat messages tz
    "wisp/scheduler" -> Scheduler.handleChat messages tz
    "wisp/housekeeper" -> pure $ Left "Agent 'wisp/housekeeper' not yet implemented"
    "wisp/insights" -> Insights.handleChat messages tz
    _ -> pure $ Left $ "Unknown agent: " <> agent
