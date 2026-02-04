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

dispatchChat :: Text -> [ChatMessage] -> App (Either Text ChatResponse)
dispatchChat "wisp/concierge" msgs = Concierge.handleChat msgs
dispatchChat "wisp/scheduler" _ = pure $ Left "Agent 'wisp/scheduler' not yet implemented"
dispatchChat "wisp/housekeeper" _ = pure $ Left "Agent 'wisp/housekeeper' not yet implemented"
dispatchChat "wisp/insights" _ = pure $ Left "Agent 'wisp/insights' not yet implemented"
dispatchChat agent _ = pure $ Left $ "Unknown agent: " <> agent
