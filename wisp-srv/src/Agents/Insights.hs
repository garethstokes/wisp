module Agents.Insights
  ( agentInfo
  ) where

import Domain.Agent (AgentInfo(..))

agentInfo :: AgentInfo
agentInfo = AgentInfo
  { agentId = "wisp/insights"
  , agentDescription = "Retrieval, summaries, feedback clustering"
  , agentTools = []
  , agentWorkflows = ["feedback-cluster", "generate-summary"]
  , agentImplemented = False
  }
