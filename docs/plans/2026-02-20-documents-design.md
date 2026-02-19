# Documents Design

**Goal:** Add a projection layer for derived state (projects, notes, preferences) that is built from activities via a log of transforms.

**Architecture:** Activities are immutable input events. Documents are mutable derived state. The Log connects them with an audit trail of how documents evolved from activities.

---

## Core Model

```
Activities (input events)
     │
     ▼
    Log (transforms)
     │
     ▼
Documents (derived state)
     │
     └──► (as Note activity) ──► Activities
```

Documents can feed back into the activity stream as Note activities, enabling documents to influence other documents through the standard pipeline.

---

## Document Types

### Project

Track ongoing work across different life areas.

```json
{
  "type": "project",
  "data": {
    "name": "Gym",
    "type": "health"
  },
  "tags": ["gym", "fitness", "training"],
  "active": true,
  "last_activity_at": "2026-02-20T10:00:00Z"
}
```

Project types: `work`, `personal`, `family`, `health`, `spiritual`

### Note

Freeform knowledge with metadata.

```json
{
  "type": "note",
  "data": {
    "title": "Alice is my sister",
    "content": "Works at Google, lives in Sydney"
  },
  "tags": ["alice", "family"],
  "confidence": 0.95,
  "source": "user"
}
```

### Preference

User preferences as key/value pairs.

```json
{
  "type": "preference",
  "data": {
    "key": "meeting_time_preference",
    "value": "late morning (10am-12pm)",
    "context": "scheduling"
  },
  "tags": ["scheduling", "calendar"],
  "confidence": 0.85,
  "source": "reflection"
}
```

---

## Database Schema

### Documents Table

```sql
CREATE TABLE documents (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  tenant_id UUID REFERENCES tenants(id),
  type TEXT NOT NULL CHECK (type IN ('project', 'note', 'preference')),
  data JSONB NOT NULL,
  tags TEXT[] NOT NULL DEFAULT '{}',
  confidence FLOAT,
  source TEXT,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  archived_at TIMESTAMPTZ,
  supersedes_id UUID REFERENCES documents(id),
  last_activity_at TIMESTAMPTZ
);

CREATE INDEX idx_documents_tenant ON documents(tenant_id);
CREATE INDEX idx_documents_type ON documents(type);
CREATE INDEX idx_documents_tags ON documents USING GIN(tags);
CREATE INDEX idx_documents_active ON documents(active) WHERE active = TRUE;
CREATE INDEX idx_documents_supersedes ON documents(supersedes_id);
```

### Document Log Table

```sql
CREATE TABLE document_log (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  document_id UUID NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
  activity_id UUID REFERENCES activities(id) ON DELETE SET NULL,
  description TEXT,
  source TEXT NOT NULL CHECK (source IN ('agent', 'user', 'classifier', 'reflection')),
  agent_id TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_document_log_document ON document_log(document_id);
CREATE INDEX idx_document_log_activity ON document_log(activity_id);
CREATE INDEX idx_document_log_created ON document_log(created_at DESC);
```

---

## Document Lifecycle

### Creation

Documents can be created:

1. **By user** - Explicit command (`wisp project create "Gym" --type health`)
2. **By agent** - During conversation ("Note: Alice works at Google")
3. **By classifier** - Extracting facts from activities
4. **By reflection** - Periodic analysis of activity patterns

### Updates

When a document is updated:
- A new log entry records the change
- `last_activity_at` updated for projects
- Original document modified in place (not immutable)

### Supersession

When information is corrected:
- New document created with `supersedes_id` pointing to old
- Old document remains (audit trail)
- Queries filter to latest version

### Archival

- `active` set to false
- `archived_at` timestamp recorded
- Document remains queryable but excluded from default views

---

## Log Entry Examples

**User creates a project:**
```json
{
  "document_id": "...",
  "activity_id": null,
  "description": "Created project",
  "source": "user",
  "agent_id": null
}
```

**Agent extracts note from email:**
```json
{
  "document_id": "...",
  "activity_id": "email-123",
  "description": "Extracted contact info from email signature",
  "source": "agent",
  "agent_id": "wisp/concierge"
}
```

**Classifier links GitHub activity to project:**
```json
{
  "document_id": "project-lune",
  "activity_id": "github-push-456",
  "description": "Matched via #lune tag",
  "source": "classifier",
  "agent_id": null
}
```

---

## Context Assembly

When agents handle requests, they can query relevant documents:

```sql
-- Get active projects for context
SELECT * FROM documents
WHERE tenant_id = ? AND type = 'project' AND active = TRUE
ORDER BY last_activity_at DESC;

-- Get notes matching conversation tags
SELECT * FROM documents
WHERE tenant_id = ? AND type = 'note' AND active = TRUE
AND tags && ARRAY['alice', 'meeting']
AND supersedes_id IS NULL;

-- Get all preferences
SELECT * FROM documents
WHERE tenant_id = ? AND type = 'preference' AND active = TRUE
AND supersedes_id IS NULL;
```

---

## Relationship to Existing Entities

| Entity | Role | Relationship to Documents |
|--------|------|---------------------------|
| Activity | Input event | Source for document creation/updates via Log |
| Person | Contact record | Could be migrated to Document type `person`, or kept separate |
| Soul | Agent personality | Remains separate (agent-specific, not shared) |
| Session | Conversation state | Remains separate (ephemeral) |
| Summary | Compressed sessions | Could become Document type, or remain separate |

**Decision:** People remain a separate entity for now. Can revisit if document model proves flexible enough.

---

## CLI Commands (Proposed)

```bash
# Projects
wisp project create "Gym" --type health --tags gym,fitness
wisp project list
wisp project archive <id>

# Notes
wisp note "Alice works at Google" --tags alice,contacts
wisp note list --tag alice

# Preferences
wisp pref set meeting_time "10am-12pm" --context scheduling
wisp pref list
```

---

## Open Questions

1. **Full-text search** - Should documents have FTS index on data/content?
2. **Tag normalization** - Enforce lowercase? Separate tags table for vocabulary?
3. **Person migration** - Move People to documents, or keep as separate entity?
4. **Retention** - How long to keep superseded documents?
