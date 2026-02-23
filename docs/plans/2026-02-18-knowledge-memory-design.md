# Knowledge + Memory System Design

**Goal:** Give Wisp agents shared knowledge and individual memory, enabling continuity across conversations and accumulated wisdom over time.

**Architecture:** Knowledge stored as tagged Activities (source: Note), Memory stored per-agent as sessions/summaries/souls. Notes flow through existing classification pipeline. Souls evolve via periodic reflection with user approval.

---

## Core Concepts

### Knowledge (Shared Across Agents)

General information available to all agents:
- **People** - contacts, relationships, how you know them
- **Projects** - things you work on (SuperIT, PaidRight, home)
- **Facts** - preferences, constraints, personal details
- **Daily log** - events, decisions, what happened when

Knowledge types emerge from **tags**, not predefined categories. A note tagged `#alice #family` is person knowledge. A note tagged `#superit #architecture` is project knowledge.

### Memory (Agent-Specific)

Each agent maintains its own:
- **Sessions** - pages of conversation messages
- **Summaries** - compressed versions of past sessions
- **Soul** - personality + accumulated wisdom

Memory stays isolated per-agent. Concierge doesn't see Scheduler's conversation history, but both access shared knowledge.

---

## Data Model

### Knowledge as Tagged Notes

Notes are Activities with `source: Note`:

```
Activity (source: Note)
├── title: "Alice is my sister, works at Google"
├── content: (optional longer form)
├── tags: ["alice", "family", "google"]
├── confidence: 0.85
├── status: Processed | Quarantined
├── parent_id: (if superseding an earlier note)
├── raw: {
│     origin: "chat" | "manual" | "reflection" | "import"
│     session_id: "..."
│     agent_id: "wisp"
│     source_message: "Note: Alice is my sister..."
│     created_by: "user" | "agent_proposed"
│   }
└── created_at: timestamp
```

### Tags

Normalized vocabulary with suggestion from existing:

```
Tag
├── id: UUID
├── name: "alice" (lowercase, normalized)
└── created_at: timestamp
```

Classifier suggests existing tags to prevent sprawl ("Alice" → "alice"). New tags can be created anytime.

### Sessions

Raw conversation pages:

```
Session
├── id: UUID
├── agent_id: "wisp"
├── messages: [ChatMessage]
├── created_at: timestamp
├── ended_at: timestamp | null
└── summarized: boolean
```

### Summaries

Compressed sessions:

```
Summary
├── id: UUID
├── agent_id: "wisp"
├── session_ids: [UUID] (which sessions this covers)
├── content: "Discussed email classification. Decided to..."
└── created_at: timestamp
```

### Soul

Agent personality and wisdom:

```
Soul
├── agent_id: "wisp" (primary key)
├── personality: "Concise, uses bullet points, slightly formal"
├── insights: [
│     "Prefers late morning meetings (10am-12pm)",
│     "Dislikes lengthy preambles",
│     "Wants calendar queries to include travel buffer"
│   ]
└── updated_at: timestamp
```

---

## Note Capture Flow

### Triggers

**Explicit** (user-initiated):
- "Note: ..."
- "Remember: ..."
- "/note ..."

**Agent-proposed** (user confirms):
- Agent detects decision or fact during conversation
- Asks: "Should I note that you decided on Redis for caching?"

### Pipeline

```
1. User says "Note: Alice is my sister, works at Google"
   or agent proposes and user confirms

2. Agent creates Activity:
   { source: Note
   , title: "Alice is my sister, works at Google"
   , status: Pending
   , raw: { origin: "chat", session_id: "...", agent: "wisp", created_by: "user" }
   }

3. Activity enters classification queue (existing pipeline)

4. Classifier:
   - Infers tags from content
   - Suggests from existing tags (normalizes case/spelling)
   - Checks for conflicts with existing notes
   - Assigns confidence score
   - Links to Person record if detected

5. Conflict check:
   - If new note conflicts with existing (e.g., "Alice works at Meta" vs "Alice works at Google")
   - Both flagged for review in Quarantine

6. Router:
   - confidence >= 0.7 → Processed (knowledge captured)
   - confidence < 0.7 → Quarantined (user reviews tags/content)
```

---

## Session Lifecycle

```
1. Conversation starts → new Session created

2. Messages accumulate in session

3. Session ends:
   - Explicit: /end command
   - Timeout: no messages for N minutes
   - Natural: agent detects conversation concluded

4. Background process summarizes ended sessions:
   - LLM compresses session into key points
   - Summary linked to session_ids it covers

5. Raw session retained (archived)
   - Summaries used for context assembly
   - Full sessions available for search/audit
```

---

## Soul Evolution (Periodic Reflection)

```
1. Trigger: After N sessions (configurable, e.g., 5)

2. Reflection agent reviews recent sessions:
   - What patterns emerged?
   - What preferences did user express?
   - What worked well / poorly?

3. Agent proposes soul updates:

   "Based on recent conversations, I'd like to update my approach:

   Personality:
   - [keep] Concise, uses bullet points
   - [add] Avoids scheduling suggestions before 10am

   Insights:
   - [add] When asking about 'free time', means focused work blocks
   - [update] Prefers morning meetings → Prefers late morning (10am-12pm)
   - [promote to knowledge?] Late morning preference (useful for all agents)"

4. User approves/rejects/edits each change

5. If insight promoted to knowledge:
   - Creates Note with tags #preference, #scheduling
   - Available to all agents

6. Updated soul persists, injected into future system prompts
```

---

## Context Assembly

When an agent handles a chat, it assembles:

### System Prompt

```
Base agent instructions (existing)
├── Soul injection:
│     Personality: "Concise, uses bullet points..."
│     Insights: ["Prefers late morning meetings", ...]
└── Agent-specific instructions
```

### Context

```
Current session messages (full)
├── Recent session summaries (last 3-5, this agent)
├── Relevant knowledge:
│     - Tag-matched notes (if "SuperIT" mentioned → #superit notes)
│     - Mentioned people + their notes
│     - Today's daily log notes
├── Calendar/activities (existing)
└── Mentioned people records (existing)
```

### Retrieval Strategy

Current (simple):
- Tag matching for knowledge
- Recency for summaries
- Existing activity/calendar queries

Future (planned):
- FTS + LLM encoding
- Reranking for relevance
- Semantic similarity beyond exact tag match

---

## Provenance

Every piece of knowledge tracks its origin:

```
raw: {
  origin: "chat" | "manual" | "reflection" | "import"
  session_id: "..." (if from chat)
  agent_id: "wisp" (which agent captured it)
  source_message: "Note: Alice works at Google"
  created_by: "user" | "agent_proposed"
}
```

Questions always answerable:
- Where did this knowledge come from?
- When was it captured?
- What was the context?
- Did I say it or did an agent propose it?

---

## Conflict Resolution

When classifier detects conflicting notes:

```
New note: "Alice works at Meta"
Existing: "Alice works at Google" (tagged #alice)

Both notes → Quarantine with conflict link:
  "Conflicting notes about Alice's workplace - review which is current"

User resolves:
  - Archive old note (set parent_id on new note)
  - Keep both (if both valid in different contexts)
  - Merge (edit content to reconcile)
```

---

## Database Schema

### New Tables

```sql
-- Tags (normalized vocabulary)
CREATE TABLE tags (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name TEXT UNIQUE NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Note-to-tag mapping
CREATE TABLE activity_tags (
  activity_id UUID REFERENCES activities(id) ON DELETE CASCADE,
  tag_id UUID REFERENCES tags(id) ON DELETE CASCADE,
  PRIMARY KEY (activity_id, tag_id)
);

-- Sessions (conversation pages)
CREATE TABLE sessions (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  agent_id TEXT NOT NULL,
  messages JSONB NOT NULL DEFAULT '[]',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  ended_at TIMESTAMPTZ,
  summarized BOOLEAN NOT NULL DEFAULT FALSE
);

-- Summaries (compressed sessions)
CREATE TABLE summaries (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  agent_id TEXT NOT NULL,
  session_ids UUID[] NOT NULL,
  content TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Souls (agent personality + wisdom)
CREATE TABLE souls (
  agent_id TEXT PRIMARY KEY,
  personality TEXT NOT NULL DEFAULT '',
  insights JSONB NOT NULL DEFAULT '[]',
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Indexes
CREATE INDEX idx_activity_tags_activity ON activity_tags(activity_id);
CREATE INDEX idx_activity_tags_tag ON activity_tags(tag_id);
CREATE INDEX idx_sessions_agent ON sessions(agent_id);
CREATE INDEX idx_sessions_created ON sessions(created_at DESC);
CREATE INDEX idx_summaries_agent ON summaries(agent_id);
```

### Modifications to Existing

```sql
-- Add parent_id for note supersession/conflict resolution
ALTER TABLE activities ADD COLUMN parent_id UUID REFERENCES activities(id);

-- Add source type for notes
-- (activities.source already supports extensible values)
```

---

## Open Questions (Future)

1. **Knowledge decay** - Should old notes fade in relevance? Options: manual archive, timestamp weighting, evergreen flag

2. **Retrieval sophistication** - FTS + LLM encoding + reranking planned for later

3. **Session timeout** - What's the right idle timeout before auto-ending a session?

4. **Reflection frequency** - Is 5 sessions the right trigger, or should it be time-based?

---

## Summary

| Concept | Storage | Shared? | Evolution |
|---------|---------|---------|-----------|
| Knowledge | Activities (source: Note) + Tags | Yes, all agents | User notes, agent proposals |
| Sessions | sessions table | No, per-agent | Accumulate messages, then summarize |
| Summaries | summaries table | No, per-agent | Background compression |
| Soul | souls table | No, per-agent | Periodic reflection, user approval |
