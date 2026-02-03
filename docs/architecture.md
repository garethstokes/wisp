# Wisp Architecture

Wisp is an autonomy-preserving personal assistant that processes emails and calendar events, classifies them by urgency and type, and surfaces what needs attention.

## System Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                         wisp-srv                                     │
│                                                                      │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌────────────┐ │
│  │   Pollers   │  │ Classific-  │  │  Notific-   │  │  REST API  │ │
│  │  Gmail +    │─▶│   ation     │  │   ation     │  │            │ │
│  │  Calendar   │  │   Queue     │  │   Loop      │  │            │ │
│  └─────────────┘  └──────┬──────┘  └─────────────┘  └─────────────┘ │
│                          │                                ▲          │
│                          ▼                                │          │
│                   ┌─────────────┐                         │          │
│                   │   Workers   │                         │          │
│                   │ N parallel  │──▶ Claude API           │          │
│                   │ classifiers │                         │          │
│                   └──────┬──────┘                         │          │
│                          │                                │          │
│                          ▼                                │          │
│                   ┌─────────────┐                         │          │
│                   │ PostgreSQL  │◀────────────────────────┘          │
│                   └─────────────┘                                    │
└─────────────────────────────────────────────────────────────────────┘
         ▲                                      ▲
         │                                      │
    wisp-cli                               wisp-web (future)
```

## Components

### wisp-srv (Backend Server)

Long-running Haskell service with:

- **Pollers** - Fetch Gmail messages and Calendar events on interval
- **Classification Queue** - STM-based queue feeding N parallel workers
- **Workers** - Call Claude API to classify activities by type, urgency, autonomy tier
- **Notification Loop** - Adaptive batch notifications via desktop (notify-send)
- **REST API** - HTTP endpoints for CLI and future web UI
- **Chat** - 12-factor style conversational agent with action execution

### wisp-cli (Command Line Interface)

Haskell CLI that talks to wisp-srv over HTTP:

- `wisp status` - Overview with activity counts by status
- `wisp inbox` - Activities requiring attention (surfaced, quarantined, high urgency)
- `wisp people` - Known contacts linked to activities
- `wisp chat "..."` - Natural language queries and commands
- `wisp approve <id>` / `wisp dismiss <id>` - Manage activities

## Code Organization

```
wisp-srv/
├── app/Main.hs              # Entry point, starts all background threads
├── src/
│   ├── App/                 # Application foundation
│   │   ├── Config.hs        # YAML config parsing
│   │   ├── Env.hs           # Environment setup
│   │   └── Monad.hs         # App monad (ReaderT Env IO)
│   ├── Domain/              # Core types (no IO)
│   │   ├── Activity.hs      # Activity, ActivityStatus, ActivitySource
│   │   ├── Classification.hs
│   │   ├── Person.hs
│   │   ├── ChatAction.hs    # Actions chat can execute
│   │   └── ...
│   ├── Infra/               # External integrations
│   │   ├── Db/              # PostgreSQL queries
│   │   ├── Google/          # Gmail, Calendar, OAuth
│   │   └── Claude/          # Claude API client
│   ├── Services/            # Business logic
│   │   ├── GmailPoller.hs
│   │   ├── CalendarPoller.hs
│   │   ├── Classifier.hs
│   │   ├── Router.hs        # Determines activity status from classification
│   │   ├── Pipeline.hs      # Orchestrates classify → route → store
│   │   ├── Chat.hs          # Conversational agent
│   │   ├── Notification.hs
│   │   └── NotificationLoop.hs
│   └── Http/                # REST API
│       ├── Routes.hs
│       ├── Server.hs
│       └── Handlers/
└── migrations/              # SQL migrations (run in alphabetical order)
```

## Data Flow

### Activity Lifecycle

```
Email/Event arrives
       │
       ▼
┌─────────────┐
│   CAPTURE   │  Poller stores raw data
│   Pending   │  status: pending
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  CLASSIFY   │  Worker calls Claude API
│             │  Extracts: type, urgency, personas, autonomy tier
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   ROUTE     │  Deterministic routing based on classification
│             │
│  Tier 1-2 ──────▶ Processed (silent)
│  Tier 3   ──────▶ NeedsReview
│  Tier 4   ──────▶ Surfaced (notify user)
│  Low conf ──────▶ Quarantined (human review)
└─────────────┘
```

### Activity Statuses

| Status | Meaning |
|--------|---------|
| Pending | Newly captured, awaiting classification |
| NeedsReview | Tier 3 - Requires human review before action |
| Quarantined | Low confidence classification, needs human verification |
| Surfaced | Tier 4 - Ready for user attention |
| Processed | Tier 1-2 - Handled silently |
| Archived | Dismissed by user |

### Autonomy Tiers

| Tier | Description | Action |
|------|-------------|--------|
| 1 | Noise/spam | Auto-archive silently |
| 2 | FYI/informational | Log and continue |
| 3 | Needs human input | Surface for review |
| 4 | Requires immediate attention | Notify and surface |

## Key Design Decisions

### 12-Factor Chat Actions

Chat uses structured outputs instead of tool loops:
1. LLM returns JSON with action intent
2. Server executes action deterministically
3. Result returned to user

This keeps control flow in your code, not the LLM.

### Adaptive Notifications

- Default: notify every 4 hours
- If 3+ urgent items: notify every 2 hours
- Quiet hours configurable (default 10pm-8am)
- LLM generates natural language summaries (PDA-friendly)

### Multi-Account Support

- Multiple Google accounts stored in `accounts` table
- Each account has its own OAuth tokens
- Pollers iterate through all accounts each cycle

## Configuration

`wisp.yaml`:

```yaml
server:
  host: "127.0.0.1"
  port: 8080

database:
  url: "postgres://localhost:5432/wisp"

google:
  clientId: "set-via-env"      # GOOGLE_CLIENT_ID
  clientSecret: "set-via-env"  # GOOGLE_CLIENT_SECRET

polling:
  intervalMinutes: 5

classification:
  confidenceThreshold: 0.5
  workerCount: null  # null = auto-detect CPU count

claude:
  apiKey: "set-via-env"  # ANTHROPIC_API_KEY
  model: "claude-sonnet-4-20250514"

notifications:
  enabled: true
  default_interval_hours: 4
  urgent_interval_hours: 2
  urgent_threshold_count: 3
  quiet_hours_start: "22:00"
  quiet_hours_end: "08:00"
  vip_emails: []
```

## Database Schema

Core tables:

- `accounts` - Google accounts with OAuth tokens
- `activities` - Emails and calendar events with classification
- `people` - Contacts extracted from activities
- `receipts` - Processing audit log
- `notification_state` - Tracks last notification time

See `wisp-srv/migrations/` for full schema.
