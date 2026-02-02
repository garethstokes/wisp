# Notification System Design

## Overview

A batch notification system that periodically summarizes pending items and delivers via desktop notification. Designed for PDA-friendliness: no demands, just natural observations at adaptive intervals.

## Key Principles

- **Batch, not interrupt** - Periodic summaries, not per-item alerts
- **Adaptive frequency** - Infrequent by default, more frequent only when genuinely needed
- **Natural language** - LLM-generated summaries that read like a friend mentioning something, not a todo list
- **No pressure** - Observations only, never "you should" or demands

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                    wisp-srv                          │
│                                                      │
│  ┌──────────────────┐     ┌──────────────────────┐  │
│  │ NotificationLoop │────▶│ NotificationService  │  │
│  │ (background)     │     │ - gatherPending      │  │
│  │                  │     │ - generateSummary    │  │
│  │ Checks every     │     │ - deliver            │  │
│  │ 15 min, sends    │     │   (notify-send)      │  │
│  │ per adaptive     │     └──────────────────────┘  │
│  │ interval         │                               │
│  └──────────────────┘                               │
└─────────────────────────────────────────────────────┘
```

## Notification Criteria

Items included in notifications:
- Status = Surfaced (Tier 4 items)
- OR urgency = High
- OR sender email is in VIP list
- AND `notified_at` is NULL (not already notified)

## Adaptive Timing

**Default interval:** 4 hours

**Shorten to 2 hours when:**
- 3+ items matching criteria are pending
- Calendar event from VIP or high-urgency within next 2 hours

**Hard floor:** Never more frequent than 2 hours

**Quiet hours:** Configurable window (default 10pm-8am) where notifications queue until morning

**Loop behavior:**
1. Runs every 15 minutes
2. Checks if adaptive interval has elapsed since last notification
3. If yes, gathers items and sends
4. Updates `last_notification_at` timestamp

## Content Generation

Gather pending items and ask Claude to write a natural summary.

**Prompt:**
```
Write a brief, casual observation about what's waiting.
No pressure, no "you should" - just friendly info.
Keep it under 3 sentences.

Items waiting:
- [list of titles/senders/types]
```

**Example outputs:**
- "A few things came in - Sarah sent something about the workshop, and there's a calendar reminder for tomorrow's dentist appointment."
- "Quiet day so far. One GitHub notification about a failed build."
- "3 new things since this morning, including something from your accountant."

## Delivery

Desktop only (for now):
- Shell out to `notify-send "Wisp" "<summary>"`
- 30 second timeout
- Informational only, no action required

Future: Phone notifications via Ntfy when idle detection shows user is away.

## Database Changes

**Add to activities table:**
```sql
ALTER TABLE activities ADD COLUMN notified_at TIMESTAMPTZ;
```

**New table for state:**
```sql
CREATE TABLE notification_state (
  id TEXT PRIMARY KEY DEFAULT 'singleton',
  last_notification_at TIMESTAMPTZ
);
```

## Configuration

```yaml
notifications:
  enabled: true
  default_interval_hours: 4
  urgent_interval_hours: 2
  urgent_threshold_count: 3
  quiet_hours_start: "22:00"
  quiet_hours_end: "08:00"
  vip_emails: []
```

## Implementation Files

- `Services/Notification.hs` - Core logic: gather, summarize, deliver
- `Services/NotificationLoop.hs` - Background thread with adaptive timer
- `App/Config.hs` - NotificationConfig type
- `Infra/Db/Notification.hs` - Database queries for state and marking notified

## Startup

Main.hs spawns notification loop thread alongside classification workers:

```haskell
when (notificationsEnabled config) $ do
  notifyThread <- async $ notificationLoop env
  link notifyThread
```

## Future Extensions

- Phone delivery via Ntfy when idle
- `wisp notify` CLI command for immediate summary
- Acknowledge/snooze via notification actions
