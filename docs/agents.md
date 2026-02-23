# Agents and Skills

## Overview

Wisp uses a **knowledge-based agent architecture** where agents are personas defined by notes in the knowledge system, not hardcoded modules. Each agent:

- Is defined by a note tagged with `agent:NAME` containing its personality configuration
- Has a **soul** that evolves over time (stored in the database)
- Can **activate skills** to gain specialized tools
- Always has access to **base tools** for knowledge management

### Key Concepts

| Concept | Description |
|---------|-------------|
| **Agent** | A named persona (e.g., `wisp`) loaded from knowledge |
| **Soul** | Persistent personality insights that evolve through interactions |
| **Skill** | A set of specialized tools an agent can activate at runtime |
| **Base Tools** | Tools available to all agents (knowledge, notes, skill activation) |

**Important:** An agent is NOT the same as a skill. The agent `wisp` can activate different skills (concierge, scheduler, insights) at runtime. There is no `wisp/concierge` agent — there is a `wisp` agent that can use the `concierge` skill.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Agent: wisp                            │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐ │
│  │ Personality │  │    Soul     │  │   Active Skill      │ │
│  │  (from note)│  │ (evolving)  │  │ (optional, dynamic) │ │
│  └─────────────┘  └─────────────┘  └─────────────────────┘ │
│                                                             │
│  ┌─────────────────────────────────────────────────────┐   │
│  │                    Base Tools                        │   │
│  │  search_knowledge | read_note | add_note | activate  │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

## Base Tools

All agents have access to these tools without needing a skill:

| Tool | Description |
|------|-------------|
| `search_knowledge` | Search notes and documents by query or tags |
| `read_note` | Read full content of a specific note |
| `add_note` | Create a new note with optional tags |
| `activate_skill` | Activate a skill to gain its tools |
| `deactivate_skill` | Deactivate the current skill (when one is active) |
| `spawn_agent` | Spawn a background sub-agent for a specific task |

### spawn_agent

The `spawn_agent` tool allows an agent to delegate work to a background sub-agent. This is useful for:
- Long-running research tasks
- Parallel execution of independent subtasks
- Work that shouldn't block the main conversation

**Example:**
```json
{
  "tool": "spawn_agent",
  "task": "Research the latest Haskell web frameworks",
  "tools": ["research"],
  "context": {"focus": "production readiness"}
}
```

The spawned agent runs asynchronously and stores its findings in the knowledge system with appropriate tags.

## Skills

Skills provide specialized tools. An agent can have **one active skill at a time**. Skills are activated at runtime — they are not baked into agent identity.

### concierge

Intake, classification, routing, and quarantine management.

**Tools:**
| Tool | Description |
|------|-------------|
| `update_activities` | Update activity status or classification |
| `query_activities` | Fetch activities by status, limit, etc. |
| `query_people` | Look up contacts by name or email |

**Example:**
```json
{"tool": "query_activities", "status": "pending", "limit": 10}
{"tool": "update_activities", "activity_ids": ["id1"], "status": "processed"}
{"tool": "query_people", "search": "alice", "limit": 5}
```

### scheduler

Calendar reasoning, schedule queries, finding free time.

**Tools:**
| Tool | Description |
|------|-------------|
| `query_calendar` | Get calendar events for a date range |
| `find_free_slots` | Find available time slots |
| `list_connected_accounts` | List connected calendar accounts |

**Example:**
```json
{"tool": "query_calendar", "days": 7}
{"tool": "find_free_slots", "days": 7, "duration_minutes": 60, "start_hour": 9, "end_hour": 17}
{"tool": "list_connected_accounts"}
```

### insights

Activity search, pattern analysis, and people frequency insights.

**Tools:**
| Tool | Description |
|------|-------------|
| `search_activities` | Full-text search across activities |
| `get_summary` | Statistical overview of recent activities |
| `get_people_insights` | Analyze frequent contacts and interaction patterns |

**Example:**
```json
{"tool": "search_activities", "query": "project update", "limit": 20}
{"tool": "get_summary", "hours": 24}
{"tool": "get_people_insights", "search": "bob", "important_only": true}
```

### github

Read-only access to GitHub repositories using connected OAuth accounts.

**Tools:**
| Tool | Description |
|------|-------------|
| `list_repos` | List repositories the user has access to |
| `list_commits` | Get commit history for a repository |
| `read_file` | Read file contents from a repository |
| `view_diff` | View commit diff or compare two refs |
| `list_prs` | List pull requests for a repository |
| `view_pr` | View detailed pull request information |

**Example:**
```json
{"tool": "list_repos", "limit": 10}
{"tool": "list_commits", "repo": "owner/repo", "branch": "main", "limit": 5}
{"tool": "read_file", "repo": "owner/repo", "path": "src/main.hs", "ref": "main"}
{"tool": "view_diff", "repo": "owner/repo", "commit": "abc123"}
{"tool": "list_prs", "repo": "owner/repo", "state": "open"}
{"tool": "view_pr", "repo": "owner/repo", "number": 42}
```

**Requires:** Connected GitHub OAuth account.

### research

Deep research with web search, planning, and finding synthesis.

**Tools:**
| Tool | Description |
|------|-------------|
| `create_research_plan` | Create a structured research plan |
| `web_search` | Search the web for information |
| `write_finding` | Record a research finding |
| `complete_research` | Finalize research and summarize findings |

**Example:**
```json
{"tool": "create_research_plan", "topic": "Haskell web frameworks", "questions": ["What are the options?", "Which is best for production?"]}
{"tool": "web_search", "query": "servant vs yesod comparison 2025", "max_results": 5}
{"tool": "write_finding", "session_id": "research-abc123", "finding": "Servant is the most popular choice for APIs"}
{"tool": "complete_research", "session_id": "research-abc123", "summary": "Research findings..."}
```

**Requires:** Tavily API key configured (TAVILY_API_KEY environment variable).

## Default Agent

The seed data creates one agent:

### wisp

The default chat agent. A helpful, proactive assistant that can activate any skill as needed.

- **Personality:** Helpful, concise, and proactive. Anticipates needs and offers actionable suggestions.
- **Default Skill:** None (starts without a skill; user or agent can activate one)

To use wisp with a specific skill:
```bash
# Chat with wisp (no skill active)
wisp chat -a wisp -m "Hello"

# Activate concierge skill
wisp activate wisp concierge

# Now chat uses concierge tools
wisp chat -a wisp -m "What's in my inbox?"

# Switch to scheduler
wisp activate wisp scheduler
```

## Creating a New Agent

Agents are created by adding a note with the `agent:NAME` tag:

```bash
# Via CLI
wisp notes add "My custom agent personality and instructions" \
  --tags "agent:jarvis"

# Via API
POST /api/notes
{
  "content": "You are a specialized agent for...",
  "tags": ["agent:jarvis"],
  "raw": {
    "personality_seed": "Be formal and precise.",
    "active_skill": null
  }
}
```

### Agent Configuration (raw field)

```json
{
  "personality_seed": "Your personality description",
  "active_skill": null
}
```

The `active_skill` field stores the currently active skill. It starts as `null` and is updated when you call `activate_skill` or use the API/CLI to activate.

## Implementation Details

### Source Files

| File | Purpose |
|------|---------|
| `Agents/Core.hs` | Agent loading, system prompt building |
| `Agents/Dispatcher.hs` | Routes chat to agents, handles tool loops |
| `Skills/Registry.hs` | Skill definitions and tool routing |
| `Skills/Base.hs` | Base tools (knowledge, notes, skill activation) |
| `Skills/Concierge.hs` | Concierge skill implementation |
| `Skills/Scheduler.hs` | Scheduler skill implementation |
| `Skills/Insights.hs` | Insights skill implementation |
| `Skills/GitHub.hs` | GitHub skill implementation |
| `Skills/Research.hs` | Research skill implementation |
| `Services/Tavily.hs` | Tavily web search API client |

### API Endpoints

| Endpoint | Description |
|----------|-------------|
| `GET /api/agents` | List all agent names |
| `GET /api/agents/:name` | Get agent info (personality, active skill, soul) |
| `POST /api/agents/:name/activate/:skill` | Activate a skill |
| `POST /api/agents/:name/deactivate` | Deactivate current skill |
| `GET /api/agents/:name/sessions` | List chat sessions |
| `GET /api/agents/:name/active-session` | Get active session (for TUI resume) |
| `GET /api/skills` | List all available skills |

### CLI Commands

```bash
# List agents
wisp agents

# Show agent details
wisp agents wisp

# Chat with an agent
wisp chat -a wisp -m "What's in my inbox?"

# Activate a skill
wisp activate wisp concierge

# Deactivate skill
wisp deactivate wisp
```
