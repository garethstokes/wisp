# Chat System

The chat system provides conversational interaction with Wisp agents.

## Agent Selection

Chat requires specifying an agent via `--agent` (or `-a`):

```bash
wisp chat -a wisp/concierge -m "Show quarantined items"
wisp chat -a wisp/concierge -m "Approve them"
```

Available agents can be listed with `wisp agents`.

## Session Persistence

Chat sessions are stored locally in `~/.wisp/sessions/<name>.json`.

### Session Management

```bash
# Use default session
wisp chat -a wisp/concierge -m "Hello"

# Use named session
wisp chat -a wisp/concierge -m "Hello" -s work

# Start fresh session (clears history)
wisp chat -a wisp/concierge -m "Fresh start" --new

# List sessions
wisp sessions

# Delete a session
wisp sessions -d work
```

### Session File Structure

```json
{
  "agent": "wisp/concierge",
  "created_at": "2026-02-04T12:00:00Z",
  "updated_at": "2026-02-04T12:05:00Z",
  "messages": [
    {"role": "user", "content": "Show me quarantined items"},
    {"role": "assistant", "content": "Here are 3 quarantined items..."}
  ]
}
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
  "agent": "wisp/concierge",
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
1. Agent exists in registry
2. Agent is implemented
3. Then dispatches to the agent's handler

### GET /agents

Returns all registered agents:

```json
{
  "agents": [
    {
      "id": "wisp/concierge",
      "description": "Intake, classification, routing, quarantine",
      "tools": [
        {"name": "update_activities", "type": "decision"},
        {"name": "query_activities", "type": "decision"},
        {"name": "query_people", "type": "decision"}
      ],
      "workflows": ["classify-pending", "route-activity", "quarantine-interview"],
      "implemented": true
    }
  ]
}
```

## Architecture

### Agent Registry

Each agent module exports `agentInfo :: AgentInfo`:

```haskell
data AgentInfo = AgentInfo
  { agentId          :: Text
  , agentDescription :: Text
  , agentTools       :: [ToolInfo]
  , agentWorkflows   :: [Text]
  , agentImplemented :: Bool
  }
```

### Agent Dispatcher

`Agents.Dispatcher` routes chat requests by agent path:

```haskell
dispatchChat :: Text -> [ChatMessage] -> App (Either Text ChatResponse)
dispatchChat "wisp/concierge" msgs = Concierge.handleChat msgs
dispatchChat "wisp/scheduler" _ = pure $ Left "Agent not implemented"
-- etc.
```

This pattern supports future namespaces like `custom/my-agent`.

### Handler Signature

Agents implement:

```haskell
handleChat :: [ChatMessage] -> App (Either Text ChatResponse)
```

The full message history is provided, allowing agents to maintain conversational context.
