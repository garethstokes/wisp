# Agents

## Overview

Agents are autonomous components that handle specific domains. Each agent:
- Lives in its own module under `Agents/`
- Exports `agentInfo :: AgentInfo` describing its capabilities
- Can have deterministic flows (Haskell orchestrates, LLM returns structured data)
- Can have decision flows (LLM outputs tool calls to express intent)

## Agent Registry

Use `wisp agents` to list all agents with their tools and implementation status.

Use `GET /agents` endpoint to retrieve agent metadata programmatically.

## Agent Definitions

### wisp/concierge [IMPLEMENTED]

Intake, classification, routing, quarantine.

**Deterministic flows:**
- `classify-pending` - Classify incoming activities
- `route-activity` - Route based on classification

**Decision flows:**
- `quarantine-interview` - Interactive chat for quarantined items

**Tools:**
- `update_activities` (decision) - Update activity status/classification
- `query_activities` (decision) - Fetch activities by filter
- `query_people` (decision) - Look up contacts

---

### wisp/scheduler [NOT IMPLEMENTED]

Calendar reasoning and communication drafts.

**Deterministic flows:** (none)

**Decision flows:**
- `schedule-negotiation` - Negotiate meeting times
- `draft-response` - Draft calendar-related responses

---

### wisp/housekeeper [NOT IMPLEMENTED]

Admin hygiene, receipts, anomaly detection.

**Deterministic flows:**
- `create-receipt` - Create audit receipt
- `cleanup-archived` - Clean up old archived items

**Decision flows:**
- `anomaly-triage` - Investigate anomalies

---

### wisp/insights [NOT IMPLEMENTED]

Retrieval, summaries, feedback clustering.

**Deterministic flows:** (none)

**Decision flows:**
- `feedback-cluster` - Cluster user feedback
- `generate-summary` - Generate activity summaries

## Adding a New Agent

1. Create `Agents/NewAgent.hs` with `agentInfo :: AgentInfo`
2. Add module to `wisp-srv.cabal`
3. Register in `Agents.Dispatcher`:
   - Add to `allAgents` list
   - Add dispatch case for `"wisp/newagent"`
4. Implement `handleChat` if the agent supports chat

```haskell
module Agents.NewAgent (agentInfo) where

import Domain.Agent (AgentInfo(..), ToolInfo(..), ToolType(..))

agentInfo :: AgentInfo
agentInfo = AgentInfo
  { agentId = "wisp/newagent"
  , agentDescription = "Description of what this agent does"
  , agentTools = []
  , agentWorkflows = []
  , agentImplemented = False
  }
```
