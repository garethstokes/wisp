# Wisp: Personal Assistant Specification

## Overview

Wisp is an autonomy-preserving personal assistant that acts as a buffer between you and life's demands. It absorbs, filters, and handles routine obligations so fewer things require your direct attention. When your input is needed, it offers options without pressure.

## Problem Statement

### Root Problem: Fragmentation

Life data is scattered across multiple systems (calendars, emails, files, messaging), causing:

- **Missed information** - Important messages/events slip through
- **Mental exhaustion** - Context switching between apps drains energy
- **No full picture** - Hard to plan when domains are separated
- **Painful search** - "Where did I see that thing?" is a daily frustration

### Downstream Problem 1: Planning Fails

Planning rhythms (yearly → monthly → weekly → daily) don't stick because:

- Missing inputs when planning (data is fragmented)
- Can't adapt to ad-hoc events from all domains (work, home, personal)

### Downstream Problem 2: Motivation Fades

No feedback loop showing progress toward goals. Effort feels invisible, so momentum fades.

### Key Constraint: PDA (Pathological Demand Avoidance)

Traditional productivity systems with schedules, to-do lists, and "you should do X" framing create pressure that triggers avoidance. The system must reduce demands, not create them.

---

## Design Principles

1. **Reduce demands, never add them** - Every feature must decrease things requiring your action
2. **Options, not instructions** - Never "you should do X" - always "here are things you could do"
3. **Progress without pressure** - Show accomplishments without framing gaps as failures
4. **Autonomous by default, within boundaries** - Act on routine matters; you define boundaries
5. **Graceful degradation** - If ignored for days, nothing breaks, no guilt - it keeps buffering

---

## Life Domains

Wisp sees your whole life across three domains:

### Work (Co-founder)

- **Incoming:** Investor emails, customer queries, team coordination, meeting requests
- **Outgoing:** Follow-ups, scheduling, status updates
- **Goals:** Revenue milestones, product launches, fundraising

### Home (Husband & Father)

- **Incoming:** School communications, family logistics, spouse coordination, bills
- **Outgoing:** RSVPs, appointment bookings, family communication
- **Goals:** Planned birthdays/holidays, school commitments covered, relationship quality

### Personal (Self)

- **Incoming:** Fitness tracking, doctor reminders, friend invitations
- **Outgoing:** Social coordination, booking appointments
- **Goals:** Weight/strength targets, health maintenance, social connection

---

## Autonomy Tiers

Actions are categorised by how much autonomy Wisp has:

### Tier 1: Fully Autonomous

AI handles silently, you never see it.

| Task | Example |
|------|---------|
| Acknowledge message receipt | "Got it, will look at this" to non-urgent email |
| Send greetings | "Happy birthday!" to contacts |
| File/label incoming mail | Receipts to receipts folder, newsletters to read-later |
| Detect calendar conflicts | Flag when two events overlap |

### Tier 2: Autonomous with Undo Window

AI acts, notifies you, reversible within window.

| Task | Example |
|------|---------|
| Schedule within constraints | Accept meeting that fits availability rules |
| Send templated replies | "Yes, I can make that work" |
| Pay recurring bills | Pay electricity matching expected amount |
| Reschedule conflicts | Decline conflict, propose alternative time |

### Tier 3: Draft and Suggest

AI prepares, you approve when ready.

| Task | Example |
|------|---------|
| Compose non-trivial replies | Draft response to investor question |
| Research for decisions | "Son's birthday - here are 3 activity options nearby" |
| Summarise what happened | "This week: 4 workouts, 12 emails handled" |

### Tier 4: Surface Only

AI makes you aware, takes no action.

| Task | Example |
|------|---------|
| New significant commitments | "Investor wants to schedule funding call" |
| Emotionally sensitive | "Email from son's therapist" |
| Unusual/high-stakes | "Bill for £2,400 - 3x normal amount" |

---

## System Concepts

### Inbox

Single entry point. All inputs land here first (emails, calendar events, etc.).

### Classification

Multi-label tagging applied to each item:

- **Persona:** work / home / personal (can have multiple)
- **Type:** request / information / action_required / fyi / event
- **Urgency:** high / normal / low
- **People:** Who's involved, relationship context
- **Autonomy tier:** 1-4
- **Confidence:** 0-1 score

### Storage

Persistent store for activities, people, conversations, and history.

### Receipts

Audit trail for every item:

- What arrived
- What Wisp did with it
- Where it went
- Confidence score (0-1)

### Quarantine

Items below confidence threshold stay in inbox as "needs review". No nagging - surfaced only when you ask.

---

## Core Flows

### Flow 1: Incoming Email Processing

```
Email arrives
    ↓
  INBOX
  Capture raw input + metadata
    ↓
  CLASSIFICATION
  Assign: persona, type, urgency, people, tier, confidence
    ↓
  CONFIDENCE CHECK (threshold: 0.5)
    ↓
  Below threshold → QUARANTINE ("needs review")
  Above threshold → Process by tier:
    Tier 1 → Handle silently
    Tier 2 → Handle + notify with undo
    Tier 3 → Draft, queue for review
    Tier 4 → Surface with context
    ↓
  STORAGE
  Write activity with schema
    ↓
  RECEIPT
  Log: input, classification, action, destination, confidence
```

### Flow 2: Calendar Conflict Detection

```
New event detected
    ↓
  INBOX → CLASSIFICATION
    ↓
  CONFLICT CHECK against existing calendar
    ↓
  CONFIDENCE SCORE
    ↓
  Below threshold → QUARANTINE
  Above threshold →
    No conflict → Accept (Tier 2)
    Conflict, reschedulable → Propose alternative (Tier 2)
    Conflict with protected time → Surface only (Tier 4)
    ↓
  STORAGE + RECEIPT
```

### Flow 3: Daily Rhythm Visibility

```
Trigger: morning / on-demand
    ↓
  Query STORAGE for today's calendar
    ↓
  Identify open windows (30+ mins)
    ↓
  Cross-reference with daily goals (train / walk dog)
    ↓
  Surface observations:
    "You have 90 minutes clear between 2pm and 3:30pm"
    ↓
  RECEIPT (log summary generated)
```

### Flow 4: Conversational Query

```
You ask a question
    ↓
  Query: STORAGE (calendar, emails, people, history)
    ↓
  Synthesise answer with sources
    ↓
  Respond conversationally
    ↓
  STORAGE: log key items from conversation
    ↓
  RECEIPT: log query + response summary
```

---

## Data Schemas

### Activity Schema

**Core Fields**

| Field | Type | Description |
|-------|------|-------------|
| `id` | uuid | Unique identifier |
| `source` | enum | `email`, `calendar`, `conversation`, `manual` |
| `source_id` | string | Original ID in source system |
| `raw` | text/blob | Original unprocessed content |
| `created_at` | timestamp | When captured |
| `updated_at` | timestamp | Last modification |

**Classification Fields**

| Field | Type | Description |
|-------|------|-------------|
| `personas` | array | `["work", "home", "personal"]` |
| `activity_type` | enum | `request`, `information`, `action_required`, `fyi`, `event` |
| `urgency` | enum | `high`, `normal`, `low` |
| `people` | array | `[{name, email, relationship}]` |
| `autonomy_tier` | int | 1-4 |
| `confidence` | float | 0-1 |
| `status` | enum | `quarantined`, `pending`, `processed`, `archived` |

**Content Fields**

| Field | Type | Description |
|-------|------|-------------|
| `title` | string | Short summary / subject |
| `summary` | text | AI-generated summary |
| `key_items` | array | Extracted dates, amounts, actions |
| `due_date` | timestamp | If time-sensitive |
| `related_activities` | array | Linked activity IDs |

**Receipt Fields**

| Field | Type | Description |
|-------|------|-------------|
| `action_taken` | enum | `acknowledged`, `filed`, `replied`, `scheduled`, `surfaced`, `quarantined`, `none` |
| `action_detail` | text | What specifically happened |
| `action_at` | timestamp | When action taken |
| `destination` | string | Where it went |
| `receipt_confidence` | float | 0-1 |
| `reversible` | boolean | Can be undone? |
| `reversed` | boolean | Was it undone? |

### People Schema

**Core Fields**

| Field | Type | Description |
|-------|------|-------------|
| `id` | uuid | Unique identifier |
| `name` | string | Display name |
| `emails` | array | Known email addresses |
| `personas` | array | `["work"]`, `["home"]`, etc. |

**Relationship Context**

| Field | Type | Description |
|-------|------|-------------|
| `relationship` | string | "investor", "son's teacher", "spouse" |
| `organisation` | string | Company, school, etc. |
| `notes` | text | Anything shared with Wisp |

**Communication Patterns**

| Field | Type | Description |
|-------|------|-------------|
| `first_contact` | timestamp | First interaction |
| `last_contact` | timestamp | Most recent |
| `contact_count` | int | Total interactions |
| `avg_response_time` | duration | Typical reply speed |
| `typical_urgency` | enum | Their usual urgency level |

**Interaction Summary**

| Field | Type | Description |
|-------|------|-------------|
| `recent_topics` | array | Recent discussion subjects |
| `pending_items` | array | Waiting on them / awaiting your response |
| `key_dates` | array | Birthday, recurring meetings |

---

## Conversational Agent

### Context Assembly

**Immediate Context (loaded per conversation)**

| Source | What it provides |
|--------|------------------|
| Today's calendar | Scheduled, upcoming, gaps |
| Recent activities | Last 24-48 hours |
| Quarantined items | Awaiting review |
| Pending drafts | Tier 3 items ready |

**Retrieved Context (on demand)**

| Source | When retrieved |
|--------|----------------|
| People records | Asking about someone |
| Historical activities | "Have I heard from..." |
| Receipts | "What did Wisp do with..." |
| Previous conversations | Referencing past discussions |

**Persistent Memory**

| Type | Examples |
|------|----------|
| Preferences | "Morning meetings preferred" |
| Goals | "3mm ARR", "110kg bodyweight" |
| Key dates | Birthdays, school terms |
| Corrections | Past reversals and reclassifications |

### Conversation Logging

After each conversation, extract and store:

- Key decisions made
- New information shared
- Action items emerged
- Questions left unanswered

---

## Confidence & Quarantine

### Confidence Thresholds

| Score | Meaning | Behaviour |
|-------|---------|-----------|
| 0.9 - 1.0 | Very confident | Act autonomously (Tier 1-2) |
| 0.7 - 0.89 | Confident | Act but notify (Tier 2) |
| 0.5 - 0.69 | Uncertain | Draft/suggest only (Tier 3) |
| Below 0.5 | Low confidence | Quarantine |

### Factors Affecting Confidence

| Factor | Impact |
|--------|--------|
| Seen sender before | ↑ Higher |
| Similar to approved items | ↑ Higher |
| New sender/pattern | ↓ Lower |
| Ambiguous content | ↓ Lower |
| Multiple valid interpretations | ↓ Lower |
| Past corrections | Adjusts scoring |

### Quarantine Behaviour

- Items stay with "needs review" status
- No expiry, no nagging
- Surfaced only when asked
- Process anytime, or never

### Learning from Corrections

- Reverse Tier 2 action → Lower confidence for similar
- Approve quarantined item → Raise confidence for similar
- Reclassify → Adjust classification model

---

## Progress Visibility

### Principle

Celebrate what happened, don't highlight what didn't.

### Weekly Summary (on request)

```
This week:
- 23 emails handled automatically
- 4 training sessions logged
- School trip RSVP sent

Toward your goals:
- Weight: 112kg → 111.5kg (↓0.5kg)
- Squat: Hit 140kg
```

Never mentions: missed workouts, unanswered emails, "should have" framing.

### Goal Tracking

Surfaces progress only:

- When you ask
- In weekly summaries (positive framing)
- When milestones hit

Never: "You haven't...", "You're behind...", "Don't forget..."

### Accomplishment Log

Every Wisp action is an accomplishment:

- Bills paid
- Emails acknowledged
- Conflicts resolved
- Birthdays remembered

Visible proof: "Things are getting handled."

---

## Service Blueprint

```
┌──────────────────────────────────────────────────────────────────────────────┐
│  USER ACTIONS                                                                 │
├──────────────────────────────────────────────────────────────────────────────┤
│  Check email     Attend meeting    Ask Wisp      Review         Train        │
│  (normal day)    (calendar)        a question    quarantine     (daily goal) │
└───────┬────────────────┬───────────────┬─────────────┬─────────────┬─────────┘
        ▼                ▼               ▼             ▼             ▼
┌──────────────────────────────────────────────────────────────────────────────┐
│  FRONTSTAGE (what user sees)                                                  │
├──────────────────────────────────────────────────────────────────────────────┤
│  Fewer emails    Calendar just     Conversational  Items with    "90 mins    │
│  to deal with    works             response        context       free 2-3pm" │
└───────┬────────────────┬───────────────┬─────────────┬─────────────┬─────────┘
        ▼                ▼               ▼             ▼             ▼
┌──────────────────────────────────────────────────────────────────────────────┐
│  BACKSTAGE (Wisp processes)                                                   │
├──────────────────────────────────────────────────────────────────────────────┤
│  Inbox →         Conflict          Query storage   Fetch         Scan cal,   │
│  Classify →      detection →       + people +      quarantine    find gaps,  │
│  Route by tier   resolve/surface   history         by status     check goals │
└───────┬────────────────┬───────────────┬─────────────┬─────────────┬─────────┘
        ▼                ▼               ▼             ▼             ▼
┌──────────────────────────────────────────────────────────────────────────────┐
│  SUPPORT PROCESSES                                                            │
├──────────────────────────────────────────────────────────────────────────────┤
│  Gmail API       Calendar API      LLM inference   Storage       Calendar    │
│  polling         polling           Context build   queries       + Goals     │
└───────┬────────────────┬───────────────┬─────────────┬─────────────┬─────────┘
        ▼                ▼               ▼             ▼             ▼
┌──────────────────────────────────────────────────────────────────────────────┐
│  DATA / STORAGE                                                               │
├──────────────────────────────────────────────────────────────────────────────┤
│  Activities      Activities        Activities      Activities    Goals       │
│  People          People            People          Receipts      Rhythm      │
│  Receipts        Receipts          Conversations   Quarantine               │
└──────────────────────────────────────────────────────────────────────────────┘
```

---

## MVP Scope

### Included

| Component | What's included |
|-----------|-----------------|
| **Integrations** | Gmail + Google Calendar |
| **Inbox** | Capture emails and calendar events |
| **Classification** | Persona, urgency, tier, confidence |
| **Tier 1 actions** | Acknowledge receipt, file/label |
| **Tier 4 surfacing** | High-stakes items with context |
| **Quarantine** | Low-confidence items held |
| **Receipts** | Full audit trail |
| **People** | Basic tracking |
| **Conversational agent** | Query schedule, people, activity, quarantine |
| **Daily rhythm** | Surface open windows |
| **Storage** | Local database |
| **Interface** | CLI |

### Deferred

| Component | Reason |
|-----------|--------|
| Tier 2 actions | Needs trust built first |
| Tier 3 drafting | Needs prompt tuning |
| WhatsApp / SMS | Integration complexity |
| File storage | Lower urgency |
| Goal tracking with metrics | Needs fitness integrations |
| Weekly summaries | Needs history first |
| Learning from corrections | Needs baseline first |

---

## Daily Goals

The assistant knows your daily rhythm goals:

- Eat within diet macros
- Train or walk the dog

It identifies windows where these *could* happen and surfaces as observations:

> "You have 90 minutes clear between 2pm and 3:30pm - no meetings, nothing pending."

No "you should" - just visibility into when it's possible.
