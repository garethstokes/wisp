# Agents (Option A)

## Flow Types

**Deterministic flow**: Haskell orchestrates, LLM returns structured data. No choices.

**Decision flow**: LLM outputs tool calls to express intent. Reducer pattern applies.

## Agent Definitions

concierge:
- Intake, classification, routing, quarantine
- Deterministic: classify-pending, route-activity
- Decision: quarantine-interview

scheduler:
- Calendar reasoning and communication drafts
- Deterministic: (none)
- Decision: schedule-negotiation, draft-response

housekeeper:
- Admin hygiene, receipts, anomaly detection
- Deterministic: create-receipt, cleanup-archived
- Decision: anomaly-triage

insights:
- Retrieval, summaries, feedback clustering
- Deterministic: (none)
- Decision: feedback-cluster, generate-summary
