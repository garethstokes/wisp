# Wisp Technical Design

## Overview

This document describes the technical architecture for Wisp, an autonomy-preserving personal assistant. It covers the system components, data flow, database schema, and build sequence for the MVP.

---

## Architecture

### Components

```
┌─────────────────────────────────────────────────────────────────────┐
│                         wisp-srv                                     │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐                 │
│  │   Poller    │  │  Job Queue  │  │  REST API   │                 │
│  │  (Gmail +   │──▶│   (PG)      │◀─│  (HTTP)     │◀── wisp-cli    │
│  │  Calendar)  │  │             │  │             │◀── wisp-web    │
│  └─────────────┘  └──────┬──────┘  └─────────────┘                 │
│                          │                                          │
│                          ▼                                          │
│                   ┌─────────────┐                                   │
│                   │   Workers   │                                   │
│                   │ - Classify  │──▶ LLM API (Claude/OpenAI)       │
│                   │ - Route     │                                   │
│                   │ - Act       │                                   │
│                   └──────┬──────┘                                   │
│                          │                                          │
│                          ▼                                          │
│                   ┌─────────────┐                                   │
│                   │ PostgreSQL  │                                   │
│                   │ - Activities│                                   │
│                   │ - People    │                                   │
│                   │ - Receipts  │                                   │
│                   │ - Jobs      │                                   │
│                   │ - Auth      │                                   │
│                   └─────────────┘                                   │
└─────────────────────────────────────────────────────────────────────┘
```

**wisp-srv**: Long-running service containing:
- Poller (Gmail + Calendar on fixed interval)
- PostgreSQL-backed job queue
- Workers that process jobs
- REST API for CLI and web clients

**wisp-cli**: Command-line client that talks to wisp-srv over HTTP.

**wisp-web**: Web UI that talks to wisp-srv over HTTP.

### Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Concurrency | Job queue (PostgreSQL-backed) | Jobs survive restarts, retries for free, easy to inspect |
| LLM | External API (Claude/OpenAI) | Simpler setup, reliable |
| API style | REST | Simple, well-understood, sufficient for MVP |
| Agent architecture | Pipeline with LLM steps | Deterministic flow, LLM at decision points only |
| OAuth | Built-in flow | Auto-refresh, no manual re-auth |
| Classification config | Hardcoded | Simple, version controlled |
| People matching | Email-based | One email = one person, simple |
| Gmail actions | Local shadow only | Build trust before modifying real inbox |
| Polling | Fixed interval | Simple, configurable |

---

## Processing Pipeline

The pipeline is deterministic with LLM called at specific decision points:

```
    Email/Event arrives
           │
           ▼
    ┌─────────────┐
    │   CAPTURE   │  Store raw in activities table
    │             │  status: pending
    └──────┬──────┘
           │
           ▼
    ┌─────────────┐
    │  CLASSIFY   │  ◀── LLM CALL
    │             │
    │  Output:    │  - personas (work/home/personal)
    │             │  - type (request/information/action_required/fyi/event)
    │             │  - urgency (high/normal/low)
    │             │  - autonomy_tier (1-4)
    │             │  - confidence (0-1)
    │             │  - summary
    └──────┬──────┘
           │
           ▼
    ┌─────────────┐
    │   RESOLVE   │  Match sender email to People table
    │   PEOPLE    │  If new: create person, LLM generates display name
    └──────┬──────┘
           │
           ▼
    ┌─────────────┐
    │    ROUTE    │  Deterministic based on confidence + tier
    │             │
    │  if confidence < 0.5:      → status: quarantined
    │  elif tier == 1:           → status: processed (silent)
    │  elif tier == 2:           → status: processed (would notify)
    │  elif tier == 3:           → status: pending_review (draft)
    │  elif tier == 4:           → status: surfaced
    └──────┬──────┘
           │
           ▼
    ┌─────────────┐
    │   RECEIPT   │  Log: input, classification, action, confidence
    └─────────────┘
```

**Key point:** LLM is called exactly once per item (at CLASSIFY). Routing is pure business logic.

---

## wisp-srv Components

### Poller

Runs on fixed interval (configurable, default 5 minutes).

**Gmail Poller:**
- Fetches emails since last poll using Gmail API
- Creates `process_email` job for each new email
- Stores raw email in activities with `status: pending`

**Calendar Poller:**
- Fetches events modified since last poll
- Creates `process_calendar_event` job for each new/changed event
- Stores raw event in activities with `status: pending`

### Job Queue

PostgreSQL-backed queue. Workers poll the jobs table, claim with `SELECT ... FOR UPDATE SKIP LOCKED`, process, update status.

Job types:
- `process_email` → Extract metadata, create `classify` job
- `process_calendar_event` → Extract metadata, create `classify` job
- `classify` → Call LLM, update activity, create `route` job
- `route` → Set status based on tier/confidence, log receipt

### REST API

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/auth/google` | GET | Start OAuth flow |
| `/auth/google/callback` | GET | OAuth callback |
| `/activities` | GET | List activities (with filters) |
| `/activities/:id` | GET | Get single activity |
| `/activities/:id/reclassify` | POST | Manually reclassify |
| `/quarantine` | GET | List quarantined items |
| `/quarantine/:id/approve` | POST | Approve item |
| `/quarantine/:id/dismiss` | POST | Dismiss item |
| `/people` | GET | List people |
| `/people/:id` | GET | Get person with history |
| `/schedule/today` | GET | Today's calendar + gaps |
| `/chat` | POST | Conversational query |

---

## Database Schema

### activities

```sql
id              uuid PRIMARY KEY
source          text        -- 'email', 'calendar', 'conversation', 'manual'
source_id       text        -- Gmail message ID, Calendar event ID
raw             jsonb       -- Original API response
created_at      timestamptz
updated_at      timestamptz

-- Classification (populated by LLM)
personas        text[]      -- ['work'], ['home', 'personal'], etc.
activity_type   text        -- 'request', 'information', 'action_required', 'fyi', 'event'
urgency         text        -- 'high', 'normal', 'low'
autonomy_tier   int         -- 1-4
confidence      float       -- 0.0-1.0
status          text        -- 'pending', 'quarantined', 'processed', 'surfaced', 'archived'

-- Content
title           text        -- Subject line or summary
summary         text        -- LLM-generated summary
sender_email    text        -- For quick filtering
person_id       uuid        -- FK to people

-- Calendar-specific
starts_at       timestamptz
ends_at         timestamptz
```

### people

```sql
id              uuid PRIMARY KEY
email           text UNIQUE
display_name    text        -- LLM-generated from email context
personas        text[]      -- ['work'], ['home'], etc.
relationship    text        -- 'investor', 'spouse', etc.
organisation    text
notes           text
first_contact   timestamptz
last_contact    timestamptz
contact_count   int
created_at      timestamptz
updated_at      timestamptz
```

### receipts

```sql
id              uuid PRIMARY KEY
activity_id     uuid        -- FK to activities
action_taken    text        -- 'classified', 'quarantined', 'processed', 'surfaced'
action_detail   text        -- Human-readable description
confidence      float
created_at      timestamptz
```

### jobs

```sql
id              uuid PRIMARY KEY
type            text        -- 'process_email', 'classify', 'route', etc.
payload         jsonb
status          text        -- 'pending', 'running', 'completed', 'failed'
scheduled_at    timestamptz
attempts        int DEFAULT 0
max_attempts    int DEFAULT 3
last_error      text
created_at      timestamptz
updated_at      timestamptz
```

### auth_tokens

```sql
id              uuid PRIMARY KEY
provider        text        -- 'google'
access_token    text
refresh_token   text
expires_at      timestamptz
scopes          text[]
created_at      timestamptz
updated_at      timestamptz
```

### poll_state

```sql
id              uuid PRIMARY KEY
source          text UNIQUE -- 'gmail', 'calendar'
last_poll_at    timestamptz
cursor          text        -- Gmail history ID or Calendar sync token
```

---

## Conversational Agent

### Context Assembly

Before calling LLM for `/chat`:

1. **Immediate context** (always loaded):
   - Today's calendar events
   - Recent activities (24h)
   - Quarantine count
   - Pending surfaced items

2. **Query analysis** (keyword/pattern matching, not LLM):
   - Person mentions → fetch person record
   - Time references → fetch relevant date range
   - "quarantine" → fetch quarantined items

3. **Retrieved context** (query-specific):
   - People records if mentioned
   - Historical activities if asked
   - Receipts if "what did Wisp do"

### System Prompt

```
You are Wisp, a personal assistant. You help by providing information
and options, never pressure or demands.

Rules:
- Never say "you should" or "don't forget" - offer observations instead
- Frame gaps in schedule as possibilities, not obligations
- When discussing progress, celebrate what happened, don't mention what didn't
- Be concise and conversational
```

### Conversation Logging

After each chat, extract and store as activity with `source: 'conversation'`:
- Key decisions mentioned
- New information shared

---

## CLI Commands

```
wisp auth          # Start OAuth flow (opens browser)
wisp status        # Quick overview: pending items, quarantine count, today's events
wisp today         # Today's schedule with gaps highlighted
wisp chat "..."    # Ask a question (also supports interactive mode)
wisp quarantine    # List quarantined items with context
wisp approve <id>  # Approve a quarantined item
wisp dismiss <id>  # Dismiss a quarantined item
wisp people        # List known people
wisp person <id>   # Detail on one person with recent activity
wisp activity <id> # Full detail on an activity with receipts
wisp logs          # Recent receipts (what Wisp has done)
```

---

## Web UI Views

| View | Description |
|------|-------------|
| Dashboard | Today's schedule, recent activity, quarantine count, surfaced items |
| Quarantine | List of items needing review, approve/dismiss actions |
| Activity feed | Chronological list with filters (persona, type, status) |
| People | List of people, click through for activity history |
| Chat | Text input for conversational queries |
| Receipts | Audit log of what Wisp has done |

---

## Authentication

### OAuth Flow

1. User runs `wisp auth`
2. CLI calls `GET /auth/google`
3. Server generates OAuth URL with scopes: `gmail.readonly`, `calendar.readonly`
4. CLI opens URL in browser
5. User consents, Google redirects to callback
6. Server exchanges code for tokens, stores in `auth_tokens`
7. Polling starts immediately

### Token Refresh

Server checks `expires_at` before each API call. If expired or within 5 minutes, uses refresh token automatically.

---

## Configuration

**Environment variables:**
- `ANTHROPIC_API_KEY` or `OPENAI_API_KEY` - LLM provider key

**Config file** (`wisp.toml`):

```toml
[server]
host = "127.0.0.1"
port = 8080

[database]
url = "postgres://localhost:5432/wisp"

[google]
client_id = "..."
client_secret = "..."

[polling]
interval_minutes = 5

[classification]
confidence_threshold = 0.5
```

---

## Deployment

```
┌─────────────────────────────────────────────────────────────────────┐
│                     LOCAL MACHINE                                    │
│                                                                      │
│   ┌──────────┐     ┌──────────┐     ┌──────────┐                   │
│   │ wisp-srv │     │ wisp-cli │     │ wisp-web │                   │
│   │ :8080    │◀────│          │     │ :3000    │                   │
│   │          │◀────────────────────│          │                   │
│   └────┬─────┘     └──────────┘     └──────────┘                   │
│        │                                                            │
│        ▼                                                            │
│   ┌──────────┐                                                      │
│   │ Postgres │                                                      │
│   │ :5432    │                                                      │
│   └──────────┘                                                      │
└─────────────────────────────────────────────────────────────────────┘
         │
         │ HTTPS
         ▼
    ┌──────────┐     ┌──────────┐
    │  Google  │     │  Claude  │
    │  APIs    │     │  API     │
    └──────────┘     └──────────┘
```

---

## MVP Scope

### Included

| Component | Details |
|-----------|---------|
| wisp-srv | Poller, job queue, workers, REST API, OAuth flow |
| wisp-cli | All commands |
| wisp-web | All views |
| Integrations | Gmail (read-only), Google Calendar (read-only) |
| Pipeline | Capture → Classify → Route → Receipt |
| Storage | PostgreSQL |
| Actions | Local shadow only |
| Tiers | All classified, Tier 1 & 4 behaviors active |

### Deferred

| Item | Reason |
|------|--------|
| Tier 2 actions | Needs trust built first |
| Tier 3 drafting | Needs prompt tuning |
| Gmail modifications | Start with shadow mode |
| Goal tracking | Needs fitness integrations |
| Weekly summaries | Needs history first |
| Learning from corrections | Needs baseline data |
| WhatsApp / SMS | Integration complexity |

---

## Build Sequence

### Phase 1: Foundation
1. Database schema + migrations
2. wisp-srv skeleton with config loading
3. Job queue implementation
4. REST API skeleton with health endpoint

### Phase 2: Authentication
5. Google OAuth flow
6. Token storage and refresh logic
7. `wisp auth` CLI command

### Phase 3: Polling & Capture
8. Gmail poller
9. Calendar poller
10. Poll state tracking
11. `wisp status` CLI command

### Phase 4: Classification Pipeline
12. Classify worker with LLM call
13. People resolution
14. Route worker
15. Receipt logging

### Phase 5: CLI Completeness
16. `wisp today`
17. `wisp quarantine`, `wisp approve`, `wisp dismiss`
18. `wisp people`, `wisp person <id>`
19. `wisp activity <id>`, `wisp logs`

### Phase 6: Conversational Agent
20. Context assembly logic
21. `/chat` endpoint
22. `wisp chat` CLI command
23. Conversation logging

### Phase 7: Web UI
24. wisp-web with dashboard
25. Remaining views
