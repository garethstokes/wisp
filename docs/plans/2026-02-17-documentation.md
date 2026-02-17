# Wisp Documentation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Create comprehensive documentation that explains why Wisp exists, what it does, and how it works.

**Architecture:** Documentation split into README (quick overview), architecture guide (technical deep-dive), and roadmap (future direction). Each document targets different audiences and purposes.

**Tech Stack:** Markdown documentation, Mermaid diagrams for visual architecture

---

## Task 1: Create README.md

**Files:**
- Create: `README.md`

**Step 1: Write README.md**

```markdown
# Wisp

An autonomy-preserving personal assistant that manages your emails and calendar while respecting your agency.

## Why Wisp?

Information overload is real. You get hundreds of emails, calendar invites, and notifications daily. Most AI assistants solve this by taking over—making decisions for you, auto-responding, auto-scheduling.

Wisp takes a different approach: **autonomy preservation**. It classifies, filters, and surfaces information intelligently, but keeps you in control of decisions that matter.

### Core Philosophy

**Autonomy Tiers** govern how much authority Wisp has:

| Tier | Authority | Example |
|------|-----------|---------|
| 1 | Silent processing | Spam filtering, newsletter categorization |
| 2 | Noted | Low-priority FYIs, recorded but not surfaced |
| 3 | Needs review | Ambiguous items requiring human judgment |
| 4 | Surfaced | Important items presented for your attention |

**Confidence-based quarantine**: When Wisp isn't sure about a classification, it quarantines the item for your review rather than guessing wrong.

**Full audit trail**: Every decision is logged with reasoning. You can inspect exactly why Wisp classified something a certain way.

## What Wisp Does

- **Email Management**: Connects to Gmail, classifies incoming messages by type and urgency
- **Calendar Coordination**: Tracks your schedule, finds free time, helps with scheduling decisions
- **Intelligent Routing**: Surfaces what needs attention, archives what doesn't
- **Interactive Agents**: Chat with specialized agents to query, search, and manage your information

## Quick Start

### Prerequisites

- GHC 9.6+ and Cabal 3.8+
- PostgreSQL 14+
- Google Cloud Console OAuth credentials
- Anthropic API key

### Setup

1. Clone and build:
   ```bash
   cd wisp-srv
   cabal build
   ```

2. Create `wisp.yaml`:
   ```yaml
   server:
     port: 5812

   database:
     url: postgresql://localhost/wisp

   google:
     clientId: "your-client-id"
     clientSecret: "your-client-secret"

   claude:
     apiKey: "your-anthropic-key"
   ```

3. Start the server:
   ```bash
   cabal run wisp-srv
   ```

4. Connect a Google account:
   ```bash
   wisp auth
   ```

5. Check your inbox:
   ```bash
   wisp inbox
   ```

## CLI Commands

```bash
# Status and overview
wisp status              # Server health and activity counts
wisp inbox               # Activities needing attention
wisp review              # Items needing human review

# Activity management
wisp activity <ID>       # Full details for one activity
wisp logs <ID>           # Audit trail for an activity
wisp approve <ID>        # Move from quarantined to surfaced
wisp dismiss <ID>        # Archive an activity

# Agent interaction
wisp chat -a wisp/concierge -m "Show quarantined items"
wisp chat -a wisp/scheduler -m "What's my schedule today?"
wisp chat -a wisp/insights -m "Emails from alice@example.com"

# System operations
wisp poll                # Trigger email/calendar sync
wisp classify            # Run classification pipeline
wisp auth                # Add a Google account

# Debugging
wisp runs                # List recent agent runs
wisp run <ID>            # Full event log for a run
```

## Agents

### Concierge (`wisp/concierge`)
Manages activities—queries, updates status, handles classification refinement.

### Scheduler (`wisp/scheduler`)
Calendar coordination—schedule queries, free slot finding, meeting assistance.

### Insights (`wisp/insights`)
Historical search—full-text search, statistics, relationship analysis.

## Documentation

- [Architecture Guide](docs/architecture.md) - Technical deep-dive
- [Roadmap](docs/roadmap.md) - Future plans
- [Vocabulary](docs/vocab.md) - Domain terminology
- [Tools](docs/tools.md) - Agent tool specifications

## License

MIT
```

**Step 2: Verify file was created**

Run: `ls -la README.md`
Expected: File exists with reasonable size

**Step 3: Commit**

```bash
git add README.md
git commit -m "docs: add README with quick start and overview"
```

---

## Task 2: Create Architecture Guide

**Files:**
- Create: `docs/architecture.md`

**Step 1: Write architecture.md**

```markdown
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
```

**Step 2: Verify file was created**

Run: `ls -la docs/architecture.md`
Expected: File exists

**Step 3: Commit**

```bash
git add docs/architecture.md
git commit -m "docs: add architecture guide"
```

---

## Task 3: Create Roadmap

**Files:**
- Create: `docs/roadmap.md`

**Step 1: Write roadmap.md**

```markdown
# Wisp Roadmap

This document outlines the current state and future direction of Wisp.

## Current State (v0.1)

### Working Features

- **Gmail Integration**: Polling, message ingestion, incremental sync
- **Calendar Integration**: Event polling, schedule queries
- **Multi-account Support**: Multiple Gmail/Calendar accounts
- **Classification Pipeline**: Automatic classification with confidence-based routing
- **Three Agents**: Concierge (activities), Scheduler (calendar), Insights (search)
- **Event Sourcing**: Full audit trail for agent runs
- **CLI**: Comprehensive command-line interface
- **HTTP API**: RESTful endpoints for all operations

### Known Limitations

- No web UI (CLI only)
- Single-user design
- No push notifications (polling only)
- Classification accuracy depends on prompt tuning
- No email sending (read-only Gmail access)

## Short-term Goals

### Improved Classification

- [ ] User feedback loop for classification corrections
- [ ] Per-user classification preferences
- [ ] Domain-specific classifiers (work vs personal)

### Better Calendar Integration

- [ ] Event creation (not just reading)
- [ ] Conflict detection and resolution
- [ ] Meeting preparation summaries

### CLI Enhancements

- [ ] Interactive mode (REPL-style)
- [ ] Rich terminal UI with colors and tables
- [ ] Autocompletion for commands

## Medium-term Goals

### Web Interface

- [ ] Dashboard showing inbox, calendar, insights
- [ ] Activity detail view with classification explanation
- [ ] Agent chat interface
- [ ] Settings and configuration UI

### Housekeeper Agent

- [ ] Automatic cleanup of old activities
- [ ] Archive management
- [ ] Storage optimization
- [ ] Scheduled maintenance tasks

### Smart Notifications

- [ ] Priority-based notification batching
- [ ] VIP sender alerts
- [ ] Configurable notification channels (email, webhook)
- [ ] Quiet hours enforcement

## Long-term Vision

### Multi-user Support

- [ ] User accounts and authentication
- [ ] Per-user configuration
- [ ] Shared calendars and activities

### Additional Integrations

- [ ] Slack/Teams messages
- [ ] Task managers (Todoist, Things)
- [ ] Note-taking apps (Notion, Obsidian)
- [ ] CRM integration

### Advanced AI Features

- [ ] Learning from user corrections
- [ ] Predictive scheduling
- [ ] Automated email drafts (with approval)
- [ ] Relationship insights and reminders

### Self-hosting Improvements

- [ ] Docker deployment
- [ ] Helm charts for Kubernetes
- [ ] SQLite option for single-user setups
- [ ] Reduced external dependencies

## Non-goals

Things Wisp explicitly won't do:

- **Auto-respond to emails**: Maintains user agency
- **Make decisions without consent**: Tier system ensures review gates
- **Collect telemetry**: No data leaves your server
- **Require cloud services**: Self-hosted by design

## Contributing

Contributions welcome! Areas where help is needed:

1. **Classification prompts**: Improving accuracy
2. **Test coverage**: More integration tests
3. **Documentation**: Examples and tutorials
4. **Agents**: New agent capabilities

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.
```

**Step 2: Verify file was created**

Run: `ls -la docs/roadmap.md`
Expected: File exists

**Step 3: Commit**

```bash
git add docs/roadmap.md
git commit -m "docs: add roadmap"
```

---

## Task 4: Final Verification and Summary Commit

**Step 1: Verify all documentation exists**

Run: `ls -la README.md docs/architecture.md docs/roadmap.md`
Expected: All three files exist

**Step 2: Review git status**

Run: `git status`
Expected: Clean working tree (all committed)

**Step 3: Show commit log**

Run: `git log --oneline -5`
Expected: Three new documentation commits
