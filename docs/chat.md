# Chat System

The chat system provides conversational interaction with Wisp agents.

## Agent Selection

Chat requires specifying an agent via `--agent` (or `-a`):

```bash
wisp chat -a wisp -m "Hello"
wisp chat -a wisp -m "What's on my calendar?"
```

Available agents can be listed with `wisp agents`.

## Skills

Agents can activate skills to gain specialized tools:

```bash
# Activate concierge skill for inbox management
wisp activate wisp concierge
wisp chat -a wisp -m "Show quarantined items"

# Switch to scheduler skill for calendar tasks
wisp activate wisp scheduler
wisp chat -a wisp -m "Find free time tomorrow"

# Deactivate skill (agent keeps base tools only)
wisp deactivate wisp
```

## Session Persistence

Chat sessions are stored in the database per agent.

### Session Management

```bash
# Use default session
wisp chat -a wisp -m "Hello"

# Use named session
wisp chat -a wisp -m "Hello" -s work

# Start fresh session (clears history)
wisp chat -a wisp -m "Fresh start" --new

# List sessions
wisp sessions

# Delete a session
wisp sessions -d work
```

### Session Behavior

- Sessions are bound to a specific agent
- If a session exists with a different agent, an error is returned
- Sessions are created on first message if they don't exist
- `--new` flag overwrites existing session with fresh one

## Server API

### POST /chat

Request:
```json
{
  "agent": "wisp",
  "messages": [
    {"role": "user", "content": "Hello"}
  ]
}
```

Response:
```json
{
  "message": "Response text",
  "tool_call": null
}
```

The server validates:
1. Agent exists in knowledge (note tagged `agent:NAME`)
2. Then dispatches to the agent handler

### GET /agents

Returns all registered agents:

```json
{
  "agents": ["wisp", "jarvis"],
  "skills": ["concierge", "scheduler", "insights"]
}
```

### GET /agents/:name

Returns agent details including active skill:

```json
{
  "name": "wisp",
  "personality": "Helpful, concise, and proactive.",
  "active_skill": "concierge",
  "soul": {
    "personality": "Prefers bullet points",
    "insights": ["User likes morning meetings"]
  }
}
```

### POST /agents/:name/activate/:skill

Activates a skill for an agent.

### POST /agents/:name/deactivate

Deactivates the current skill.

## Architecture

### Agent Loading

Agents are loaded from knowledge notes tagged with `agent:NAME`:

```haskell
loadAgentByTenant :: TenantId -> AgentName -> App (Maybe Agent)
```

### Skill Activation

Skills are stored in the agent's config and resolved at runtime:

```haskell
data Agent = Agent
  { agentName   :: AgentName
  , agentConfig :: AgentConfig
  , agentSoul   :: Soul
  , agentSkill  :: Maybe Skill  -- Currently active skill
  }
```

### Handler Signature

The dispatcher handles all chat requests:

```haskell
dispatchChat :: TenantId -> EntityId -> AgentName -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
```

The full message history is provided, allowing agents to maintain conversational context.
