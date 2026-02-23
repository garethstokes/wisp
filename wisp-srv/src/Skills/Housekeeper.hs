module Skills.Housekeeper
  ( agentInfo
  ) where

import Domain.Agent (AgentInfo(..))

agentInfo :: AgentInfo
agentInfo = AgentInfo
  { agentId = "housekeeper"
  , agentDescription = "Admin hygiene, receipts, anomaly detection"
  , agentTools = []
  , agentWorkflows = ["create-receipt", "cleanup-archived", "anomaly-triage"]
  , agentImplemented = False
  }
