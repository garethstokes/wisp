# Project Knowledge Extraction from Activities

## Overview

Generate knowledge (specifically Project documents) from the activities table through:
1. **Automatic extraction** - Classifier assigns activities to projects during classification
2. **Batch reflection** - Periodic job synthesizes project state from accumulated activities

## Goals

- Bootstrap with known projects: Wisp, Lune, Super IT, PaidRight
- Classify activities into one of these projects (or "other")
- Accumulate project state from activity signals
- Surface "new project" suggestions when unassigned activities cluster

## Design

### Project Document Structure

```json
{
  "name": "Super IT",
  "type": "work",
  "summary": "Database migration project with PaidRight team. Currently in review phase.",
  "status": "active",
  "participants": ["alice@example.com", "bob@example.com"],
  "activity_count": 23,
  "last_activity_at": "2026-02-20T14:30:00Z"
}
```

### Activity-Document Relationships

Many-to-many relationship - an activity can relate to multiple projects.

```sql
CREATE TABLE activity_documents (
  activity_id TEXT NOT NULL REFERENCES activities(id) ON DELETE CASCADE,
  document_id TEXT NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
  relationship TEXT NOT NULL,  -- 'project', 'mentions', etc.
  confidence FLOAT,
  source TEXT NOT NULL,        -- 'classifier', 'user', 'reflection'
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (activity_id, document_id, relationship)
);

CREATE INDEX idx_activity_documents_document ON activity_documents(document_id);
```

### Classifier Extension

Extended output format:

```json
{
  "personas": ["work"],
  "activity_type": "request",
  "urgency": "normal",
  "autonomy_tier": 2,
  "summary": "...",
  "projects": [
    {"name": "super-it", "confidence": 0.9},
    {"name": "wisp", "confidence": 0.6}
  ]
}
```

Classifier prompt includes list of known projects (fetched from documents where `type = 'project'`).

### Hybrid Update Model

**Immediate updates (per-activity):**
- Increment `activity_count`
- Update `last_activity_at`
- Append new emails to `participants` (deduplicated)
- Record link in `activity_documents`
- Low confidence assignments (< 0.7) → `status = 'needs_review'`

**Batch updates (reflection job):**
- Synthesize/refine `summary` from recent activity summaries
- Infer `status`: "active" / "stalled" / "completed"
- Detect "other" clusters → surface new project suggestion
- Prune stale participants

### Batch Reflection Job

**Trigger:** Scheduled (daily) or on-demand via CLI

**Process:**
```
For each active project:
  1. Fetch activities linked since last_reflected_at
  2. If no new activities → skip
  3. Build context: project doc + activity summaries
  4. LLM call: synthesize into updated project state
  5. Update project document
  6. Log changes to document_log
```

**LLM prompt structure:**
```
You are updating a project's knowledge state based on recent activities.

Current project state:
{project.data as JSON}

Recent activities (since last reflection):
- [2026-02-20] Email: "Migration review scheduled for Friday"
- [2026-02-21] Calendar: "SuperIT migration review" with alice@, bob@

Output updated project state as JSON:
{
  "summary": "...",
  "status": "active|stalled|completed",
  "participants": [...]
}
```

### New Project Suggestions

**Detection:** During batch reflection, find unassigned activities and cluster by:
- Sender email patterns
- Subject/title patterns
- Tag co-occurrence

**When cluster exceeds threshold (e.g., 5 activities):**
- Create synthetic activity with `source = 'reflection'`, `status = 'needs_review'`
- Surfaces in Approvals view
- On approve: Create project document, link activities
- On dismiss: Record in `dismissed_suggestions` to prevent re-surfacing

```sql
CREATE TABLE dismissed_suggestions (
  id TEXT PRIMARY KEY,
  cluster_key TEXT NOT NULL,   -- e.g., "subject:phoenix"
  dismissed_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
```

## Implementation Components

### Database

| Object | Purpose |
|--------|---------|
| `activity_documents` table | Many-to-many activity-document links |
| `dismissed_suggestions` table | Track dismissed project suggestions |
| Seed/migration | Bootstrap 4 initial projects |

### Modules

| Module | Purpose |
|--------|---------|
| `Infra/Db/ActivityDocument.hs` | CRUD for join table |
| `Services/ProjectUpdater.hs` | Immediate updates from classified activities |
| `Skills/Reflection.hs` | Batch reflection job |
| `Skills/Concierge/Classifier.hs` | Extended to output `projects` array |
| `Domain/ProjectSuggestion.hs` | Suggestion data type |

### Entry Points

- Classifier runs → `ProjectUpdater.updateImmediate`
- Cron/CLI command → `Reflection.runProjectReflection`
- Reflection detects clusters → creates synthetic review activities

## Bootstrap Data

Initial projects (created via migration):

| Name | Type |
|------|------|
| Wisp | work |
| Lune | work |
| Super IT | work |
| PaidRight | work |
