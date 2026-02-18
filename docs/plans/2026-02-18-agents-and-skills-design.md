# Agents and Skills Architecture

**Goal:** Separate agent identity (persona + memory) from capabilities (skills), enabling named agents like Jarvis/Friday that can dynamically activate skills.

---

## Core Concepts

### Skills

Capability modules that provide specialized prompts, tools, and context:

- **Prompt**: Stored in knowledge as a note tagged `#skill:<name> #prompt`
- **Tools**: Declared in the prompt, implementations in Haskell (`Skills/<Name>.hs`)
- **Context**: Skill code fetches its own context when active

Skills are stateless - they add capabilities but don't have memory.

### Agents

Named personas with identity and memory:

- **Definition**: A note tagged `#agent:<name>` with personality seed and state
- **Memory**: Sessions, summaries, souls keyed by agent name
- **State**: Current active skill stored in the agent note

Agents are persistent - they accumulate memory and evolve over time.

---

## Data Model

### Agent Definitions (in Knowledge)

Note tagged `#agent:jarvis`:
```json
{
  "personality_seed": "Formal, concise, proactive",
  "active_skill": null
}
```

No separate agents table - agents exist as notes. Memory tables key on agent name.

### Skills (in Knowledge + Code)

Note tagged `#skill:concierge #prompt`:
```
You are a concierge assistant helping manage email and communications.

## Tools

- classify_email: Classify an email's urgency and type
- archive_email: Archive an email
- reply_email: Draft a reply to an email
...
```

Tool implementations in `Skills/Concierge.hs`.

### Memory Tables (existing)

```sql
sessions.agent_id    -- "jarvis" (agent name, not skill path)
summaries.agent_id   -- "jarvis"
souls.agent_id       -- "jarvis"
```

---

## Runtime Flow

### Base Agent State (no skill active)

**System prompt:**
- Agent's soul (personality + insights from `souls` table)
- Agent's personality seed (from `#agent:<name>` note)

**Context:**
- Recent session summaries
- Relevant knowledge notes

**Tools:**
- `search_knowledge` - Search notes by tags or text
- `read_note` - Read a specific note
- `add_note` - Create a new note with tags
- `activate_skill` - Request permission to activate a skill

### Skill Activation

1. Agent recognizes need for a skill (e.g., user asks about calendar)
2. Agent calls `activate_skill("scheduler")`
3. System asks user: "Activate scheduler skill?"
4. User confirms
5. Agent note updated: `active_skill = "scheduler"`
6. Skill prompt + tools + context added to conversation

### Activated Skill State

**System prompt:**
- Agent soul + skill prompt (from `#skill:<name> #prompt` note)

**Context:**
- Base context + skill-specific context (skill fetches what it needs)

**Tools:**
- Base tools + skill's declared tools
- `deactivate_skill` - Return to base state

### Session Continuity

- Sessions belong to the agent, not the skill
- Skill activation/deactivation happens within the same session
- Messages flow continuously; skill change is just a context update

---

## Code Structure

### Directory Layout

```
wisp-srv/src/
  Agents/
    Core.hs         -- Unified agent logic (all agents share this)
  Skills/
    Concierge.hs    -- Concierge skill (tools, context fetching)
    Scheduler.hs    -- Scheduler skill
    Insights.hs     -- Insights skill
    Base.hs         -- Base tools (search_knowledge, add_note, etc.)
```

### Skill Module Interface

```haskell
data Skill = Skill
  { skillName :: Text
  , skillTools :: [Tool]
  , skillFetchContext :: App SkillContext
  }

-- Each skill module exports:
conciergeSkill :: Skill
schedulerSkill :: Skill
insightsSkill :: Skill
```

### Agent Core

```haskell
-- Agents/Core.hs handles:
-- 1. Load agent definition from knowledge
-- 2. Load soul from souls table
-- 3. Check active_skill, load skill prompt if set
-- 4. Assemble system prompt (soul + skill prompt)
-- 5. Dispatch tool calls (base tools + skill tools)
-- 6. Handle activate_skill / deactivate_skill
```

---

## Migration Path

### Phase 1: Restructure Code

- Rename `Agents/*.hs` → `Skills/*.hs`
- Extract prompts from code into seed notes
- Create `Agents/Core.hs` with unified logic
- Create `Skills/Base.hs` with base tools

### Phase 2: Create Initial Agents

- Seed default agent note (`#agent:wisp`)
- Ensure memory tables use agent names
- Update HTTP routes to use agent names

### Phase 3: Skill Activation Flow

- Implement `activate_skill` / `deactivate_skill` tools
- Add permission prompt (agent asks, user confirms)
- Wire up skill context fetching

### Phase 4: Base Tools

- Implement `search_knowledge`, `read_note`, `add_note`
- These leverage Phase 1's tag/note infrastructure

---

## Open Questions (deferred)

1. **Skill chaining** - Can an agent have multiple skills active? (Current answer: no, one at a time)
2. **Skill parameters** - Can skills be parameterized? (Deferred)
3. **Agent creation UI** - How do users create new agents? (Deferred - predefined for now)

---

## Summary

| Concept | Storage | Contains |
|---------|---------|----------|
| Skill | Note (`#skill:<name> #prompt`) + Code | Prompt, tools, context logic |
| Agent | Note (`#agent:<name>`) | Personality seed, active_skill |
| Memory | sessions/summaries/souls tables | Keyed by agent name |
| Soul | souls table | Personality, insights (evolves) |
