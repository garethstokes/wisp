module Agents.Scheduler
  ( agentInfo
  ) where

import Domain.Agent (AgentInfo(..))

agentInfo :: AgentInfo
agentInfo = AgentInfo
  { agentId = "wisp/scheduler"
  , agentDescription = "Calendar reasoning and communication drafts"
  , agentTools = []  -- Not yet defined
  , agentWorkflows = ["schedule-negotiation", "draft-response"]
  , agentImplemented = False
  }
