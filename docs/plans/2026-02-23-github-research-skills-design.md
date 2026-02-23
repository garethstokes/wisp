# GitHub and Research Skills Design

**Goal:** Add two new skills (GitHub, Research) and a generic sub-agent spawning tool.

## Overview

| Component | Description |
|-----------|-------------|
| **GitHub Skill** | Read-only GitHub access via existing OAuth accounts |
| **Research Skill** | Structured research workflow with parallel sub-agents |
| **spawn_agent tool** | Generic base tool for spawning sub-agents |
| **Tavily service** | Web search API client |

## GitHub Skill

### Purpose

Read-only access to GitHub repositories using existing OAuth accounts from the `accounts` table.

### Tools

| Tool | Description |
|------|-------------|
| `list_repos` | List repositories accessible to connected GitHub account |
| `list_commits` | List recent commits (optional: branch, path, limit) |
| `read_file` | Read file contents at a ref (branch/commit/tag) |
| `view_diff` | View changes for a commit or compare two refs |
| `list_prs` | List pull requests (open/closed/all) |
| `view_pr` | View PR details including diff and comments |

### Tool Schemas

```json
{"tool": "list_repos", "limit": 20}

{"tool": "list_commits", "repo": "owner/repo", "branch": "main", "limit": 10}

{"tool": "read_file", "repo": "owner/repo", "path": "src/Main.hs", "ref": "main"}

{"tool": "view_diff", "repo": "owner/repo", "commit": "abc123"}
{"tool": "view_diff", "repo": "owner/repo", "base": "main", "head": "feature"}

{"tool": "list_prs", "repo": "owner/repo", "state": "open"}

{"tool": "view_pr", "repo": "owner/repo", "number": 42}
```

### Authentication

Query `accounts` table for `provider = 'github'`, use stored access token for GitHub API calls. Reuses existing OAuth infrastructure.

### Implementation

```haskell
-- Skills/GitHub.hs
data GitHubToolCall
  = ListRepos ListReposQuery
  | ListCommits ListCommitsQuery
  | ReadFile ReadFileQuery
  | ViewDiff ViewDiffQuery
  | ListPRs ListPRsQuery
  | ViewPR ViewPRQuery

executeToolCall :: GitHubToolCall -> App ToolResult
```

## Research Skill

### Purpose

Structured research workflow: create a plan, gather information in parallel via sub-agents, synthesize findings.

### Tools

| Tool | Description |
|------|-------------|
| `create_research_plan` | Create research session with N tasks, returns session_id |
| `web_search` | Search the web via Tavily API |
| `search_knowledge` | Search internal notes (reuses base tool) |
| `write_finding` | Write a finding as a note, tagged with session_id |
| `complete_research` | Summarize all findings into final document |

### Tool Schemas

```json
{"tool": "create_research_plan", "topic": "Effect systems in Haskell", "tasks": ["Survey libraries", "Compare approaches", "Gather opinions"]}

{"tool": "web_search", "query": "polysemy vs effectful performance"}

{"tool": "search_knowledge", "query": "effect systems"}

{"tool": "write_finding", "session_id": "research-20260223-a1b2c3", "title": "Library Comparison", "content": "..."}

{"tool": "complete_research", "session_id": "research-20260223-a1b2c3"}
```

### Workflow

```
1. User: "Research the pros and cons of Effect systems in Haskell"

2. Agent calls create_research_plan:
   → Creates session "research-20260223-a1b2c3"
   → Returns tasks: ["Survey libraries", "Compare approaches", "Gather opinions"]

3. Agent spawns sub-agents for each task (in parallel):
   → spawn_agent(task="Survey libraries", tools=["web_search", "search_knowledge", "write_finding"], context={session_id: "..."})
   → spawn_agent(task="Compare approaches", ...)
   → spawn_agent(task="Gather opinions", ...)

4. Each sub-agent:
   → web_search("polysemy haskell")
   → search_knowledge("effect systems")
   → write_finding(session_id, title, content)

5. Agent calls complete_research:
   → Reads all notes tagged "research:research-20260223-a1b2c3"
   → Produces summary note
```

### Storage

Research outputs stored as notes in the `activities` table:

- **Finding:** tags `["research:{session_id}", "research:finding"]`
- **Summary:** tags `["research:{session_id}", "research:summary"]`
- **Session ID format:** `research-{YYYYMMDD}-{short_uuid}` (e.g., `research-20260223-a1b2c3`)

## Generic Sub-Agent Tool

### Purpose

Allow any agent to spawn sub-agents for parallel work. Added to base tools (available to all agents).

### Schema

```json
{
  "tool": "spawn_agent",
  "task": "Research polysemy vs effectful performance",
  "tools": ["web_search", "search_knowledge", "write_finding"],
  "context": {"session_id": "research-20260223-a1b2c3"}
}
```

### Parameters

| Param | Required | Description |
|-------|----------|-------------|
| `task` | Yes | What the sub-agent should accomplish |
| `tools` | No | Restrict to these tools (default: inherit parent's tools) |
| `context` | No | Key-value data passed to sub-agent's system prompt |

### Behavior

- Runs async, returns immediately with `agent_id`
- Sub-agent uses same personality/soul as parent agent
- Results written via tools (e.g., `write_finding`) — no direct return value
- Parent can proceed without waiting, or poll for completion

### Implementation

```haskell
-- Skills/Base.hs
data BaseToolCall
  = SearchKnowledge ...
  | ReadNote ...
  | AddNote ...
  | ActivateSkill ...
  | DeactivateSkill
  | SpawnAgent SpawnAgentParams

data SpawnAgentParams = SpawnAgentParams
  { spawnTask    :: Text
  , spawnTools   :: Maybe [Text]  -- Nothing = inherit parent's tools
  , spawnContext :: Maybe Value   -- Extra context for sub-agent
  }
```

## Configuration

### wisp.yaml additions

```yaml
tavily:
  apiKey: "tvly-your-key-here"

# GitHub uses existing OAuth - no new config needed
```

### Dependencies

| Package | Purpose |
|---------|---------|
| `http-client` | Tavily API calls (simple REST) |

## File Structure

```
wisp-srv/src/Skills/GitHub.hs       -- GitHub skill implementation
wisp-srv/src/Skills/Research.hs     -- Research skill implementation
wisp-srv/src/Services/Tavily.hs     -- Tavily API client
wisp-srv/test/Skills/GitHubSpec.hs
wisp-srv/test/Skills/ResearchSpec.hs
```

## Registry Updates

```haskell
-- Skills/Registry.hs
allSkillNames = ["concierge", "scheduler", "insights", "github", "research"]

getSkill "github"   = Just githubSkill
getSkill "research" = Just researchSkill
```

## Tool Inventory

```
Base Tools (all agents):
  - search_knowledge
  - read_note
  - add_note
  - activate_skill
  - deactivate_skill
  - spawn_agent        ← NEW

GitHub Skill:
  - list_repos
  - list_commits
  - read_file
  - view_diff
  - list_prs
  - view_pr

Research Skill:
  - create_research_plan
  - web_search
  - search_knowledge   (delegates to base)
  - write_finding
  - complete_research
```

## Testing Strategy

### GitHub Skill
- Mock GitHub API responses
- Test token retrieval from accounts table
- Test each tool with sample responses

### Research Skill
- Mock Tavily API
- Test session ID generation
- Test note tagging for findings/summaries
- Test complete_research aggregation

### spawn_agent
- Test sub-agent receives correct tools restriction
- Test context passed to sub-agent system prompt
- Test async execution
