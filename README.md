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
