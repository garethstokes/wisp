# Wisp Architecture

This document explains how Wisp is built and why it's designed this way.

## System Overview

```
┌─────────────────────────────────────────────────────────────┐
│                      User Interface                          │
├─────────────────────────────────────────────────────────────┤
│  CLI (wisp-cli)              HTTP API (wisp-srv)            │
│  - Commands                   - /chat, /inbox, /runs        │
│  - Session management         - /pipeline, /activities      │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                    Agent Dispatcher                          │
├─────────────────────────────────────────────────────────────┤
│  Routes requests to: Concierge | Scheduler | Insights       │
│  Each agent: assembles context → calls LLM → executes tools │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                    Services Layer                            │
├─────────────────────────────────────────────────────────────┤
│  CalendarPoller | GmailPoller | ClassificationQueue         │
│  Router | PeopleResolver | NotificationLoop                 │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                  Infrastructure Layer                        │
├─────────────────────────────────────────────────────────────┤
│  PostgreSQL | Google APIs | Claude API | Logger             │
└─────────────────────────────────────────────────────────────┘
```

## Project Structure

```
wisp/
├── wisp-cli/                    # Command-line interface
│   └── app/Main.hs              # All CLI commands
├── wisp-srv/                    # HTTP backend
│   ├── app/Main.hs              # Entry point, startup sequence
│   ├── src/
│   │   ├── Domain/              # Core business models
│   │   │   ├── Activity.hs      # Email/event entity
│   │   │   ├── Run.hs           # Agent run + events
│   │   │   └── Person.hs        # Contact entity
│   │   ├── Agents/              # AI agent implementations
│   │   │   ├── Concierge.hs     # Activity management
│   │   │   ├── Scheduler.hs     # Calendar coordination
│   │   │   ├── Insights.hs      # Search and analytics
│   │   │   ├── Run.hs           # Event logging framework
│   │   │   └── Dispatcher.hs    # Routes to agents
│   │   ├── Http/                # API layer
│   │   │   ├── Routes.hs        # Endpoint definitions
│   │   │   └── Handlers/        # Request handlers
│   │   ├── Services/            # Background processes
│   │   │   ├── Scheduler.hs     # Polling orchestration
│   │   │   ├── GmailPoller.hs   # Gmail sync
│   │   │   ├── CalendarPoller.hs# Calendar sync
│   │   │   └── Router.hs        # Classification routing
│   │   ├── Infra/               # External integrations
│   │   │   ├── Db/              # PostgreSQL access
│   │   │   ├── Claude/          # Anthropic API
│   │   │   └── Google/          # Gmail, Calendar, OAuth
│   │   └── App/                 # Configuration, environment
│   ├── migrations/              # Database schema
│   └── test/                    # Test suites
└── docs/                        # Documentation
```

## Core Concepts

### Activities

An **Activity** is the central entity—an email, calendar event, or conversation item.

```haskell
data Activity = Activity
  { activityId :: EntityId
  , activitySource :: ActivitySource      -- Email | Calendar | Conversation
  , activityStatus :: ActivityStatus      -- Pending | Quarantined | NeedsReview | Processed | Surfaced | Archived
  , activityRaw :: Value                  -- Original JSON from source
  , activityTitle :: Maybe Text
  , activitySummary :: Maybe Text
  -- Classification (populated by Concierge):
  , activityType :: Maybe Text            -- request | information | action_required | fyi | event
  , activityUrgency :: Maybe Text         -- low | normal | high
  , activityAutonomyTier :: Maybe Int     -- 1-4
  , activityConfidence :: Maybe Double    -- 0.0-1.0
  }
```

**Status Lifecycle:**

```
Pending (ingested)
    ↓
[Classification]
    ↓
┌───┼───────────┬────────────┐
↓   ↓           ↓            ↓
Processed  Quarantined  NeedsReview  Surfaced
(Tier 1-2) (Low conf)   (Tier 3)     (Tier 4)
    │           │           │            │
    └───────────┴───────────┴────────────┘
                    ↓
                Archived (dismissed)
```

### Agent Runs and Events

Every agent interaction is captured as a **Run** with immutable **Events**:

```haskell
data Run = Run
  { runId :: RunId
  , runAgent :: Text                -- "wisp/concierge"
  , runStatus :: RunStatus          -- Running | Waiting | Completed | Failed
  , runEvents :: [RunEvent]         -- Append-only log
  }

data RunEvent
  = InputEvent { tool :: Text, data_ :: Value }
  | ContextAssembled { context :: Value }
  | LlmCalled { model, systemPrompt, userPrompt, rawResponse :: Text, inputTokens, outputTokens :: Maybe Int }
  | ToolRequested { toolName :: Text, toolArgs :: Value }
  | ToolSucceeded { toolName :: Text, result :: Value }
  | ToolFailed { toolName :: Text, error :: Text }
```

This enables:
- **Full audit trail**: See exactly what happened and why
- **Debugging**: Inspect LLM prompts and responses
- **Replay**: Re-run decisions with different parameters

### Autonomy Tiers

The core philosophy made concrete:

| Tier | Name | User Interruption | Example |
|------|------|-------------------|---------|
| 1 | Silent | None | Spam filtered, newsletters archived |
| 2 | Noted | None | Low-priority FYI logged but not surfaced |
| 3 | Review | Required | Ambiguous item needs human judgment |
| 4 | Surfaced | Immediate | Important item presented for action |

### Confidence Threshold

When classification confidence < threshold (default 0.7):
- Activity goes to **Quarantined** status
- User can review and approve/dismiss
- Builds feedback loop for improving classification

## Data Flow

### Email Classification Pipeline

```
1. Poll Cycle (every N minutes)
   └─→ Gmail API: listMessages(since: historyId)

2. Ingestion
   └─→ Insert Activity(status: Pending)
   └─→ Enqueue to ClassificationQueue

3. Classification Worker
   └─→ Dequeue activity
   └─→ Create Run
   └─→ Log InputEvent
   └─→ Call Claude with system prompt + activity
   └─→ Log LlmCalled event
   └─→ Parse tool call: classify_activity
   └─→ Log ToolRequested
   └─→ Execute classification
   └─→ Log ToolSucceeded
   └─→ Route based on confidence + tier
   └─→ Update activity status
   └─→ Insert Receipt
   └─→ Complete Run
```

### Interactive Chat Flow

```
1. User: wisp chat -a wisp/scheduler -m "Free time tomorrow?"

2. CLI
   └─→ Load session (prior messages)
   └─→ POST /chat { agent, messages, timezone }

3. HTTP Handler
   └─→ Dispatch to Scheduler agent
   └─→ Create RunContext
   └─→ Assemble context (calendar events)
   └─→ Build prompts
   └─→ Call Claude (logged)
   └─→ Parse tool request: find_free_slots
   └─→ Execute tool
   └─→ Loop back to Claude with results
   └─→ Parse response: respond_to_user
   └─→ Return response

4. CLI
   └─→ Display message
   └─→ Save to session
```

## Design Decisions

### Why Haskell?

- **Type safety**: Domain models are precise and self-documenting
- **Purity**: Side effects are explicit in the type system
- **Composition**: ReaderT monad stacks compose cleanly
- **Performance**: Compiled, concurrent, efficient

### Why PostgreSQL?

- **JSONB**: Flexible schema for raw activity data
- **ACID**: Reliable transactions for event sourcing
- **Mature**: Well-supported, well-understood
- **Simple**: postgresql-simple library, no heavy ORM

### Why Event Sourcing for Runs?

- **Auditability**: Every decision is traceable
- **Debugging**: See exact prompts and responses
- **Replay**: Re-run with different parameters
- **Trust**: Users can verify agent behavior

### Why Tool-based Agent Interface?

Agents don't call functions directly—they request tools:

```
LLM Response: {"tool": "query_calendar", "args": {"date": "2026-02-18"}}
```

Benefits:
- **Logging**: Every tool call is captured
- **Validation**: Args are validated before execution
- **Security**: Tools are whitelisted
- **Inspection**: Users can see exactly what agents do

## Configuration

### wisp.yaml

```yaml
server:
  host: 127.0.0.1
  port: 5812

database:
  url: postgresql://localhost/wisp

google:
  clientId: "..."
  clientSecret: "..."

claude:
  apiKey: "..."
  model: "claude-sonnet-4-20250514"

polling:
  intervalMinutes: 5

classification:
  confidenceThreshold: 0.7
  workerCount: null          # null = auto-detect CPUs

notifications:
  enabled: true
  default_interval_hours: 4
  quiet_hours_start: "22:00"
  quiet_hours_end: "08:00"
```

### Environment Variables

Override any config value:
- `DATABASE_URL`
- `PORT`
- `GOOGLE_CLIENT_ID`
- `GOOGLE_CLIENT_SECRET`
- `ANTHROPIC_API_KEY`

## Database Schema

Key tables:

| Table | Purpose |
|-------|---------|
| `accounts` | Linked Gmail/Calendar accounts |
| `activities` | Emails, events, conversations |
| `agent_runs` | Run metadata (agent, status, timestamps) |
| `run_events` | Append-only event log (JSONB) |
| `receipts` | Action audit trail |
| `people` | Extracted contacts |
| `poll_state` | Sync cursors |

## Testing

```bash
cd wisp-srv
cabal test
```

Test categories:
- **Domain**: Model serialization, validation
- **Database**: CRUD operations, migrations
- **Integration**: Full workflows
- **Handlers**: HTTP endpoint behavior

## Key Files

| File | Purpose |
|------|---------|
| `wisp-srv/app/Main.hs` | Server entry point, startup sequence |
| `wisp-cli/app/Main.hs` | CLI commands |
| `wisp-srv/src/Agents/Run.hs` | Event logging framework |
| `wisp-srv/src/Agents/Dispatcher.hs` | Agent routing |
| `wisp-srv/src/Services/Router.hs` | Classification routing logic |
| `wisp-srv/src/Infra/Claude/Client.hs` | Claude API wrapper |
