# wisp-tui Design

**Goal:** A terminal user interface for Wisp providing chat, activity browsing, document management, and approval workflows.

**Tech:** Haskell, [brick](https://hackage.haskell.org/package/brick), SSE for streaming

---

## Overview

wisp-tui is a navigation-centric TUI with four main views accessed via `Tab` cycling:

1. **Chat** - Converse with agents, streaming responses
2. **Activities** - Browse emails, calendar events, GitHub events
3. **Documents** - Manage projects, notes, preferences
4. **Approvals** - Review quarantined items and uncertain classifications

```
┌─────────────────────────────────────────────────────────────┐
│ [Chat] Activities  Documents  Approvals     wisp/concierge  │  ← Header
├─────────────────────────────────────────────────────────────┤
│                                                             │
│                      Main Content Area                      │  ← View
│                                                             │
├─────────────────────────────────────────────────────────────┤
│ Connected │ Press ? for help                                │  ← Status
└─────────────────────────────────────────────────────────────┘
```

---

## Project Structure

```
wisp/
├── wisp-core/                  -- Shared client library (NEW)
│   ├── src/
│   │   ├── Wisp/Client.hs      -- HTTP client
│   │   ├── Wisp/Client/SSE.hs  -- SSE streaming
│   │   └── Wisp/Client/Types.hs
│   └── wisp-core.cabal
├── wisp-cli/                   -- Refactored to use wisp-core
├── wisp-tui/                   -- New TUI package
│   ├── app/Main.hs
│   ├── src/
│   │   ├── Tui/App.hs          -- Brick app definition
│   │   ├── Tui/Types.hs        -- State, events, resources
│   │   ├── Tui/Views/
│   │   │   ├── Chat.hs
│   │   │   ├── Activities.hs
│   │   │   ├── Documents.hs
│   │   │   └── Approvals.hs
│   │   └── Tui/Widgets/        -- Reusable components
│   └── wisp-tui.cabal
└── wisp-srv/
```

---

## Server-Side: SSE for LLM Streaming

New endpoint for streaming chat responses:

```
POST /api/chat/stream
Content-Type: application/json
Accept: text/event-stream

{"agent": "wisp/concierge", "message": "...", "session": "default"}
```

**Event types:**

| Event | Data | Description |
|-------|------|-------------|
| `chunk` | `{"text": "..."}` | Partial response text |
| `tool_call_start` | `{"tool": "...", "args": {...}}` | Tool invocation begun |
| `tool_call_result` | `{"tool": "...", "result": ..., "duration_ms": N}` | Tool completed |
| `done` | `{"session_id": "...", "token_count": N}` | Response complete |
| `error` | `{"message": "...", "code": "..."}` | Error occurred |

**Implementation:**
- New module `Http/Handlers/ChatStream.hs`
- LLM runner emits events via `TChan` or `TQueue`
- Handler reads channel, formats as SSE, streams to client
- Existing `/api/chat` remains for CLI backward compatibility

---

## Views

### Chat View

```
┌─────────────────────────────────────────────────────────────┐
│ [Chat] Activities  Documents  Approvals     wisp/concierge  │
├─────────────────────────────────────────────────────────────┤
│ ┌─session: default────────────────────────────────────────┐ │
│ │                                                         │ │
│ │ [You] What meetings do I have tomorrow?                 │ │
│ │                                                         │ │
│ │ [Concierge] Let me check your calendar...               │ │
│ │ ⚙ Calling query_activities...                           │ │
│ │                                                         │ │
│ │ You have 2 meetings tomorrow:                           │ │
│ │ • 10:00 - Standup with team                             │ │
│ │ • 14:00 - 1:1 with Alice                                │ │
│ │                                                         │ │
│ └─────────────────────────────────────────────────────────┘ │
├─────────────────────────────────────────────────────────────┤
│ > _                                                         │
├─────────────────────────────────────────────────────────────┤
│ Connected │ a:agent s:session e:editor ?:help               │
└─────────────────────────────────────────────────────────────┘
```

**Keybindings:**
- `i` / `Enter` - Focus input
- `e` - Open `$EDITOR` for multi-line input
- `a` - Switch agent
- `s` - Switch session
- `n` - New session
- `j/k` - Scroll history
- `Esc` - Unfocus / cancel

**Streaming behavior:**
- `chunk` events append text in real-time
- `tool_call_start` shows "⚙ Calling {tool}..."
- `tool_call_result` updates to "✓ {tool} ({duration}ms)"
- `done` removes streaming indicator

### Activities View

```
┌─────────────────────────────────────────────────────────────┐
│  Chat [Activities] Documents  Approvals                     │
├─────────────────────────────────────────────────────────────┤
│ Filter: all                                        [/]search│
├─────────────────────────────────────────────────────────────┤
│ ▸ 📧 Meeting notes from Alice           10:32  surfaced    │
│   📅 Standup with team                  09:00  surfaced    │
│   🐙 PR #42 merged: fix auth bug        yesterday  stored  │
│   📧 Weekly report                      yesterday  stored  │
│   ...                                                       │
├─────────────────────────────────────────────────────────────┤
│ 156 activities │ j/k:navigate l:expand q:back /:search      │
└─────────────────────────────────────────────────────────────┘
```

**Expanded inline (press `l`):**
```
│ ▾ 📧 Meeting notes from Alice           10:32  surfaced    │
│   │ From: alice@example.com                                 │
│   │ Tags: meeting, alice, project-x                         │
│   │ ──────────────────────────────────────                  │
│   │ Hi, here are the notes from today's sync...             │
│   │ [Press l to collapse, o to open full detail]            │
```

**Keybindings:**
- `j/k` - Navigate
- `l` / `Enter` - Expand/collapse
- `o` - Full detail modal
- `h` - Collapse / back
- `/` - Search
- `f` - Filter by source
- `gg` - Top, `G` - Bottom

### Documents View

Tabbed sub-views for Projects, Notes, Preferences.

```
┌─────────────────────────────────────────────────────────────┐
│  Chat  Activities [Documents] Approvals                     │
├─────────────────────────────────────────────────────────────┤
│ [1:Projects]  2:Notes  3:Prefs                              │
├─────────────────────────────────────────────────────────────┤
│   Name                    Type        Last Activity         │
│ ──────────────────────────────────────────────────────────  │
│ ▸ Wisp                    work        2 hours ago           │
│   Gym                     health      3 days ago            │
│   Home renovation         personal    1 week ago            │
│   (archived: 2)                                             │
├─────────────────────────────────────────────────────────────┤
│ 3 active │ 1/2/3:tabs c:create a:archive j/k:nav            │
└─────────────────────────────────────────────────────────────┘
```

**Keybindings:**
- `1/2/3` - Switch sub-tab
- `j/k` - Navigate
- `c` - Create
- `a` - Archive (Projects)
- `e` - Edit
- `d` - Delete (with confirmation)

### Approvals View

Combined queue for quarantined items and uncertain classifications.

```
┌─────────────────────────────────────────────────────────────┐
│  Chat  Activities  Documents [Approvals]                    │
├─────────────────────────────────────────────────────────────┤
│ Review Queue (5 pending)                                    │
├─────────────────────────────────────────────────────────────┤
│   Type        Activity                        Reason        │
│ ──────────────────────────────────────────────────────────  │
│ ▸ quarantine  📧 Suspicious login alert       flagged       │
│   classify    📧 FW: Project update           uncertain 62% │
│   classify    📅 Blocked: Focus time          uncertain 58% │
│   quarantine  📧 Your account statement       flagged       │
│   classify    🐙 Issue opened: bug report     uncertain 51% │
├─────────────────────────────────────────────────────────────┤
│ 5 pending │ y:approve x:dismiss l:details c:set-category    │
└─────────────────────────────────────────────────────────────┘
```

**Keybindings:**
- `j/k` - Navigate
- `l` - Expand details
- `y` - Approve (accept suggestion)
- `x` - Dismiss (archive)
- `c` - Set category manually
- `Enter` - Approve and next

---

## Navigation & Keybindings

**Global:**
- `Tab` / `Shift+Tab` - Cycle views
- `?` - Help overlay
- `q` - Quit (with confirmation if mid-action)
- `Esc` - Cancel / back

**Style:** Vim-style throughout (`hjkl`, `/` search, `:` commands)

---

## State Management

```haskell
data AppState = AppState
  { currentView     :: View
  , chatState       :: ChatState
  , activitiesState :: ActivitiesState
  , documentsState  :: DocumentsState
  , approvalsState  :: ApprovalsState
  , config          :: ClientConfig
  , statusMessage   :: Maybe (Text, UTCTime)
  }

data ChatState = ChatState
  { messages       :: [ChatMessage]
  , inputBuffer    :: Text
  , currentAgent   :: Text
  , currentSession :: Text
  , streaming      :: Maybe StreamingState
  }
```

**Events:**
- Brick's `BChan` receives SSE events from background thread
- Custom: `ChatEventReceived ChatEvent | RefreshView View | Tick`
- Tick every second for timestamps and status dismissal

---

## Error Handling

- **Status bar** - Transient errors, auto-dismiss after 3-5 seconds
- **Inline** - Persistent issues shown in content area (e.g., "Failed to load activities")
- No modal dialogs for routine errors

---

## Dependencies

**wisp-core:**
```
aeson, http-client, http-types, text, time, bytestring, mtl
```

**wisp-tui:**
```
wisp-core, brick, vty, microlens, microlens-th, async, stm
```

**cabal.project:**
```
packages:
  wisp-srv
  wisp-cli
  wisp-tui
  wisp-core
```

---

## Open Questions

1. **Help overlay** - Full keybinding reference or context-sensitive hints?
2. **Theming** - Support custom colors via config file?
3. **Offline mode** - Cache last-known state when server unavailable?
