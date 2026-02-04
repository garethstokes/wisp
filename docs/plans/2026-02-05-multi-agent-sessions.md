# Multi-Agent Session Design

**Goal:** Allow switching between agents within a single chat session, with shared conversation history.

## Data Model Changes

**Current session structure:**
```json
{
  "agent": "wisp/concierge",
  "messages": [{"role": "user", "content": "..."}]
}
```

**New session structure:**
```json
{
  "messages": [
    {"role": "user", "content": "hello what's on for today?"},
    {"role": "assistant", "agent": "wisp/concierge", "content": "You have 3 meetings..."},
    {"role": "user", "content": "can you clear my afternoon?"},
    {"role": "assistant", "agent": "wisp/scheduler", "content": "I'll help with that..."}
  ]
}
```

Key changes:
- Remove top-level `agent` field (sessions no longer bound to one agent)
- Add `agent` field to each assistant message (tracks who responded)
- User messages don't need an agent tag

## CLI Changes

The `-a` flag becomes required for `chat` (no default agent):

```bash
wisp chat -a wisp/concierge -m "what's on today?"
wisp chat -a wisp/scheduler -m "clear my afternoon"
```

If `-a` is omitted, show error:
```
Error: Please specify an agent with -a (e.g., -a wisp/concierge)
```

Remove the agent-mismatch validation that currently blocks using a different agent on an existing session.

The `--new` flag continues to work the same way - clears the session and starts fresh.

## Server-Side Prompt Building

When an agent receives conversation history, it builds a prompt showing who said what:

```
User: hello what's on for today?
wisp/concierge: You have 3 meetings today - a standup at 9am...
User: can you clear my afternoon?
```

The agent name prefix replaces the generic "Assistant:" label, letting the current agent understand context from other specialists.

## Implementation

**Files to modify:**

1. **`wisp-cli/app/Main.hs`**
   - Remove `sessionAgent` field from `Session` type
   - Add `smAgent` field to `SessionMessage` type (optional, only for assistant messages)
   - Remove the agent-mismatch validation check
   - Store the agent in each assistant message when saving
   - Make `-a` required (remove default value)

2. **`wisp-srv/src/Domain/Chat.hs`**
   - Add optional `messageAgent` field to `ChatMessage` type

3. **`wisp-srv/src/Agents/Concierge.hs` and `Agents/Scheduler.hs`**
   - Update `buildConversationPrompt` to use agent name as role label when present

**Migration:** Existing sessions continue to work - old messages won't have agent tags, new messages will.
