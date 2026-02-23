# GitHub and Research Skills - Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement GitHub skill, Research skill, spawn_agent base tool, and Tavily service.

**Design:** See `2026-02-23-github-research-skills-design.md`

---

## Task 1: Add Tavily config to App.Config

**Steps:**
1. Add `TavilyConfig` data type with `apiKey :: Text`
2. Add `tavily :: Maybe TavilyConfig` to `Config`
3. Support `TAVILY_API_KEY` env var override

**Files:** `wisp-srv/src/App/Config.hs`

**Verification:**
```bash
cabal build wisp-srv 2>&1 | grep -E "(error|warning:.*Config)" || echo "Build OK"
```

---

## Task 2: Create Tavily service

**Steps:**
1. Create `Services/Tavily.hs` with:
   - `TavilySearchResult` type (title, url, content, score)
   - `search :: Text -> Int -> App [TavilySearchResult]`
2. Use `http-client` for REST calls to `https://api.tavily.com/search`
3. Handle missing API key gracefully (return error, don't crash)

**Files:** `wisp-srv/src/Services/Tavily.hs`

**Verification:**
```bash
cabal build wisp-srv 2>&1 | grep -E "error" || echo "Build OK"
```

---

## Task 3: Create GitHub skill module (types only)

**Steps:**
1. Create `Skills/GitHub.hs` with:
   - `GitHubToolCall` ADT (ListRepos, ListCommits, ReadFile, ViewDiff, ListPRs, ViewPR)
   - Query types for each tool
   - `ToolResult` type
   - `agentInfo :: AgentInfo` (skill metadata)
2. Add JSON parsing instances

**Files:** `wisp-srv/src/Skills/GitHub.hs`

**Verification:**
```bash
cabal build wisp-srv 2>&1 | grep -E "error" || echo "Build OK"
```

---

## Task 4: Implement GitHub API client functions

**Steps:**
1. Add to `Skills/GitHub.hs`:
   - `getGitHubToken :: App (Maybe Text)` - fetch token from accounts table
   - `githubRequest :: Text -> App (Either Text Value)` - make authenticated API call
   - `executeToolCall :: GitHubToolCall -> App ToolResult` - route to implementations
2. Implement each tool:
   - `listRepos` - GET /user/repos
   - `listCommits` - GET /repos/:owner/:repo/commits
   - `readFile` - GET /repos/:owner/:repo/contents/:path
   - `viewDiff` - GET /repos/:owner/:repo/commits/:sha or compare
   - `listPRs` - GET /repos/:owner/:repo/pulls
   - `viewPR` - GET /repos/:owner/:repo/pulls/:number

**Files:** `wisp-srv/src/Skills/GitHub.hs`

**Verification:**
```bash
cabal build wisp-srv 2>&1 | grep -E "error" || echo "Build OK"
```

---

## Task 5: Create Research skill module (types only)

**Steps:**
1. Create `Skills/Research.hs` with:
   - `ResearchToolCall` ADT (CreateResearchPlan, WebSearch, WriteFinding, CompleteResearch)
   - Query types for each tool
   - `ToolResult` type
   - `agentInfo :: AgentInfo`
   - `generateSessionId :: App Text` - creates `research-YYYYMMDD-abc123`
2. Add JSON parsing instances

**Files:** `wisp-srv/src/Skills/Research.hs`

**Verification:**
```bash
cabal build wisp-srv 2>&1 | grep -E "error" || echo "Build OK"
```

---

## Task 6: Implement Research skill tool execution

**Steps:**
1. Implement each tool in `Skills/Research.hs`:
   - `createResearchPlan` - generates session ID, returns tasks
   - `webSearch` - delegates to Tavily service
   - `writeFinding` - creates note with `research:{session_id}` tag
   - `completeResearch` - reads all findings, creates summary note
2. Use `insertNote` from `Infra.Db.Activity` for writing notes

**Files:** `wisp-srv/src/Skills/Research.hs`

**Verification:**
```bash
cabal build wisp-srv 2>&1 | grep -E "error" || echo "Build OK"
```

---

## Task 7: Add spawn_agent to base tools

**Steps:**
1. Add to `Skills/Base.hs`:
   - `SpawnAgent SpawnAgentParams` constructor to `BaseToolCall`
   - `SpawnAgentParams` type (task, tools, context)
   - `parseSpawnAgent` function
   - Update `parseBaseToolCall` to handle "spawn_agent"
2. Add `"spawn_agent"` to `baseToolNames`
3. Stub `executeBaseTool` for SpawnAgent (return placeholder, impl in next task)

**Files:** `wisp-srv/src/Skills/Base.hs`

**Verification:**
```bash
cabal build wisp-srv 2>&1 | grep -E "error" || echo "Build OK"
```

---

## Task 8: Implement spawn_agent execution

**Steps:**
1. Create sub-agent spawning logic:
   - Build system prompt with task + context
   - Restrict available tools if specified
   - Run async via `Control.Concurrent.Async`
   - Return immediately with agent_id
2. Sub-agent uses parent's personality/soul
3. Sub-agent writes results via tools (no return value)

**Files:** `wisp-srv/src/Skills/Base.hs`, possibly `Agents/SubAgent.hs`

**Verification:**
```bash
cabal build wisp-srv 2>&1 | grep -E "error" || echo "Build OK"
```

---

## Task 9: Register new skills in Registry

**Steps:**
1. Update `Skills/Registry.hs`:
   - Import `Skills.GitHub` and `Skills.Research`
   - Add `"github"`, `"research"` to `allSkillNames`
   - Add `githubSkill` and `researchSkill` definitions
   - Add cases to `getSkill`
   - Add execution routing in `executeSkillTool`
2. Update `baseTools` to include `spawn_agent` tool definition

**Files:** `wisp-srv/src/Skills/Registry.hs`

**Verification:**
```bash
cabal build wisp-srv 2>&1 | grep -E "error" || echo "Build OK"
```

---

## Task 10: Add GitHub skill tests

**Steps:**
1. Create `test/Skills/GitHubSpec.hs`:
   - Test `agentInfo` has correct ID and tools
   - Test JSON parsing for each tool call type
   - Test query parameter parsing

**Files:** `wisp-srv/test/Skills/GitHubSpec.hs`

**Verification:**
```bash
cabal test --test-option="--match=/Skills.GitHub/" 2>&1 | tail -20
```

---

## Task 11: Add Research skill tests

**Steps:**
1. Create `test/Skills/ResearchSpec.hs`:
   - Test `agentInfo` has correct ID and tools
   - Test JSON parsing for each tool call type
   - Test session ID generation format
   - Test finding/summary tag generation

**Files:** `wisp-srv/test/Skills/ResearchSpec.hs`

**Verification:**
```bash
cabal test --test-option="--match=/Skills.Research/" 2>&1 | tail -20
```

---

## Task 12: Update documentation

**Steps:**
1. Update `docs/agents.md`:
   - Add GitHub skill to skills list with tools
   - Add Research skill to skills list with tools
   - Document spawn_agent base tool
2. Seed skill prompts (optional - can do later)

**Files:** `docs/agents.md`

**Verification:**
```bash
grep -c "github\|research\|spawn_agent" docs/agents.md
```

---

## Task 13: Final verification

**Steps:**
1. Run full test suite
2. Build all packages
3. Verify no warnings in new code

**Verification:**
```bash
cabal build all 2>&1 | grep -E "error" || echo "Build OK"
cabal test 2>&1 | tail -30
```
