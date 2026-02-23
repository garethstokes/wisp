# TUI Markdown Rendering and Agent Events Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add markdown rendering to TUI chat messages and improve agent run visibility with real-time tool call display.

**Architecture:** Server emits SSE events for agent lifecycle (running, tool calls). TUI parses these events, displays tool progress inline in chat viewport, and renders all agent text as markdown with basic formatting (headers, bold, code, lists).

**Tech Stack:** Brick TUI, Vty, SSE streaming, custom markdown parser (no external deps).

---

## Task 1: Add AgentRunning event to server

**Files:**
- Modify: `wisp-srv/src/Domain/ChatEvent.hs`

**Step 1: Add AgentRunningEvent constructor**

In `Domain/ChatEvent.hs`, add to the `ChatEvent` data type:

```haskell
data ChatEvent
  = ChunkEvent Text
  | AgentRunningEvent           -- NEW: signals agent started processing
  | ToolCallStartEvent ToolCallInfo
  | ToolCallResultEvent ToolResultInfo
  | DoneEvent Text Int
  | ErrorEvent Text Text
  deriving (Show, Eq)
```

**Step 2: Update ToJSON instance**

```haskell
instance ToJSON ChatEvent where
  toJSON (ChunkEvent txt) = object ["text" .= txt]
  toJSON AgentRunningEvent = object []  -- NEW
  toJSON (ToolCallStartEvent info) = toJSON info
  -- ... rest unchanged
```

**Step 3: Update chatEventToSSE**

```haskell
chatEventToSSE :: ChatEvent -> BL.ByteString
chatEventToSSE evt =
  let (eventName, payload) = case evt of
        ChunkEvent _ -> ("chunk", encode evt)
        AgentRunningEvent -> ("agent_running", encode evt)  -- NEW
        ToolCallStartEvent _ -> ("tool_call_start", encode evt)
        -- ... rest unchanged
```

**Step 4: Build to verify**

```bash
cabal build wisp-srv 2>&1 | grep -E "error" || echo "Build OK"
```

**Step 5: Commit**

```bash
git add wisp-srv/src/Domain/ChatEvent.hs
git commit -m "feat(server): add AgentRunningEvent for TUI status"
```

---

## Task 2: Emit AgentRunning event in dispatcher

**Files:**
- Modify: `wisp-srv/src/Agents/Dispatcher.hs`

**Step 1: Emit event at start of agent run**

Find `runAgentLoop` and add emission at the start. In `dispatchChatNormal`, after loading the agent, emit the event:

```haskell
dispatchChatNormal tenantId accountId agentName msgs mTimezone _mSessionIdFromClient emit = do
  -- Load the agent from knowledge
  mAgent <- loadAgentByTenant tenantId agentName
  case mAgent of
    Nothing -> pure $ Left $ "Unknown agent: " <> agentName
    Just agent -> do
      -- Emit agent running event
      liftIO $ emit AgentRunningEvent  -- NEW: add this line

      -- Get or create session (rest unchanged)
```

**Step 2: Build to verify**

```bash
cabal build wisp-srv 2>&1 | grep -E "error" || echo "Build OK"
```

**Step 3: Commit**

```bash
git add wisp-srv/src/Agents/Dispatcher.hs
git commit -m "feat(server): emit AgentRunningEvent when agent starts"
```

---

## Task 3: Parse AgentRunning event in client SSE

**Files:**
- Modify: `wisp-core/src/Wisp/Client/SSE.hs`

**Step 1: Add AgentRunning constructor**

```haskell
data ChatEvent
  = ChunkEvent Text
  | AgentRunning              -- NEW
  | ToolCallStart Text
  | ToolCallResult Text Int
  | DoneEvent Text Int
  | ErrorEvent Text Text
  deriving (Show, Eq)
```

**Step 2: Add parsing case**

In `parseEventData`:

```haskell
parseEventData :: Text -> Text -> Maybe ChatEvent
parseEventData event dataJson = case event of
  "chunk" -> parseChunk dataJson
  "agent_running" -> Just AgentRunning  -- NEW
  "tool_call_start" -> parseToolStart dataJson
  "tool_call_result" -> parseToolResult dataJson
  "done" -> parseDone dataJson
  "error" -> parseError dataJson
  _ -> Nothing
```

**Step 3: Build to verify**

```bash
cabal build wisp-core 2>&1 | grep -E "error" || echo "Build OK"
```

**Step 4: Commit**

```bash
git add wisp-core/src/Wisp/Client/SSE.hs
git commit -m "feat(core): parse AgentRunning SSE event"
```

---

## Task 4: Add tool tracking fields to ChatState

**Files:**
- Modify: `wisp-tui/src/Tui/Types.hs`

**Step 1: Add new fields to ChatState**

```haskell
data ChatState = ChatState
  { _csMessages :: [ChatMessage]
  , _csInputBuffer :: Text
  , _csCurrentAgent :: Text
  , _csCurrentSession :: Text
  , _csStreaming :: Bool
  , _csStreamBuffer :: Text
  , _csToolCalls :: [(Text, Maybe Int)]  -- NEW: (name, duration_ms or Nothing if pending)
  } deriving (Show)
```

**Step 2: Add lens export**

Add to module exports:

```haskell
  , csToolCalls
```

**Step 3: Build to verify**

```bash
cabal build wisp-tui 2>&1 | grep -E "error" || echo "Build OK"
```

**Step 4: Commit**

```bash
git add wisp-tui/src/Tui/Types.hs
git commit -m "feat(tui): add csToolCalls field to ChatState"
```

---

## Task 5: Initialize tool tracking in Main.hs

**Files:**
- Modify: `wisp-tui/app/Main.hs`

**Step 1: Find initial ChatState and add field**

Search for `ChatState` initialization and add `_csToolCalls = []`:

```haskell
initialChatState = ChatState
  { _csMessages = []
  , _csInputBuffer = ""
  , _csCurrentAgent = "default"
  , _csCurrentSession = "default"
  , _csStreaming = False
  , _csStreamBuffer = ""
  , _csToolCalls = []  -- NEW
  }
```

**Step 2: Build to verify**

```bash
cabal build wisp-tui 2>&1 | grep -E "error" || echo "Build OK"
```

**Step 3: Commit**

```bash
git add wisp-tui/app/Main.hs
git commit -m "feat(tui): initialize csToolCalls in initial state"
```

---

## Task 6: Handle new events in handleSSEEvent

**Files:**
- Modify: `wisp-tui/app/Main.hs`

**Step 1: Import csToolCalls lens**

Ensure `csToolCalls` is imported from `Tui.Types`.

**Step 2: Add AgentRunning handler**

```haskell
handleSSEEvent :: ChatEvent -> EventM Name AppState ()
handleSSEEvent AgentRunning = do  -- NEW
  now <- liftIO getCurrentTime
  modify $ chatState . csStreaming .~ True
  modify $ chatState . csToolCalls .~ []
  modify $ statusMessage .~ Just ("Agent thinking...", now, StatusInfo)
handleSSEEvent (ChunkEvent chunk) = do
  modify $ chatState . csStreamBuffer %~ (<> chunk)
-- ... rest unchanged
```

**Step 3: Update ToolCallStart handler**

```haskell
handleSSEEvent (ToolCallStart name) = do
  now <- liftIO getCurrentTime
  modify $ chatState . csToolCalls %~ (++ [(name, Nothing)])
  modify $ statusMessage .~ Just ("Calling " <> name <> "...", now, StatusInfo)
```

**Step 4: Update ToolCallResult handler**

```haskell
handleSSEEvent (ToolCallResult name ms) = do
  now <- liftIO getCurrentTime
  modify $ chatState . csToolCalls %~ updateLastTool name ms
  modify $ statusMessage .~ Just (name <> " completed in " <> T.pack (show ms) <> "ms", now, StatusInfo)
  where
    updateLastTool toolName duration calls =
      reverse $ case reverse calls of
        ((n, Nothing):rest) | n == toolName -> (n, Just duration) : rest
        other -> other
```

**Step 5: Update DoneEvent handler to clear tools**

```haskell
handleSSEEvent (DoneEvent _ _) = do
  s <- get
  now <- liftIO getCurrentTime
  let response = s ^. chatState . csStreamBuffer
  if T.null response
    then pure ()
    else do
      let assistantMsg = ChatMessage "Assistant" response now
      modify $ chatState . csMessages %~ (++ [assistantMsg])
  modify $ chatState . csStreamBuffer .~ ""
  modify $ chatState . csStreaming .~ False
  modify $ chatState . csToolCalls .~ []  -- NEW: clear tool calls
```

**Step 6: Build to verify**

```bash
cabal build wisp-tui 2>&1 | grep -E "error" || echo "Build OK"
```

**Step 7: Commit**

```bash
git add wisp-tui/app/Main.hs
git commit -m "feat(tui): handle AgentRunning and track tool calls"
```

---

## Task 7: Display tool calls in streaming indicator

**Files:**
- Modify: `wisp-tui/src/Tui/Views/Chat.hs`

**Step 1: Import csToolCalls lens**

```haskell
import Tui.Types
```

**Step 2: Update streamingIndicator to show tools**

```haskell
streamingIndicator :: ChatState -> [Widget Name]
streamingIndicator cs
  | cs ^. csStreaming =
      toolCallsWidget (cs ^. csToolCalls)
      ++ [ padLeft (Pad 2) $ renderMultiline $ cs ^. csStreamBuffer
         , withAttr (attrName "cursor") $ txt "▌"
         ]
  | otherwise = []

toolCallsWidget :: [(Text, Maybe Int)] -> [Widget Name]
toolCallsWidget [] = []
toolCallsWidget calls = [vBox $ map renderToolCall calls]

renderToolCall :: (Text, Maybe Int) -> Widget Name
renderToolCall (name, Nothing) =
  withAttr (attrName "toolPending") $ txt $ "  ⏳ " <> name <> "..."
renderToolCall (name, Just ms) =
  withAttr (attrName "toolComplete") $ txt $ "  ✓ " <> name <> " (" <> T.pack (show ms) <> "ms)"
```

**Step 3: Build to verify**

```bash
cabal build wisp-tui 2>&1 | grep -E "error" || echo "Build OK"
```

**Step 4: Commit**

```bash
git add wisp-tui/src/Tui/Views/Chat.hs
git commit -m "feat(tui): display tool calls in streaming indicator"
```

---

## Task 8: Add tool call attributes to theme

**Files:**
- Modify: `wisp-tui/app/Main.hs`

**Step 1: Find attrMap definition and add new attributes**

```haskell
theAttrs :: AttrMap
theAttrs = attrMap V.defAttr
  [ (attrName "userRole", fg V.cyan)
  , (attrName "assistantRole", fg V.magenta)
  , (attrName "cursor", V.defAttr `V.withStyle` V.blink)
  , (attrName "toolPending", fg V.yellow)      -- NEW
  , (attrName "toolComplete", fg V.green)      -- NEW
  , (attrName "mdHeader", fg V.cyan `V.withStyle` V.bold)    -- NEW: for markdown
  , (attrName "mdBold", V.defAttr `V.withStyle` V.bold)      -- NEW
  , (attrName "mdCode", fg V.yellow)           -- NEW
  , (attrName "mdBullet", fg V.cyan)           -- NEW
  -- ... rest unchanged
  ]
```

**Step 2: Build to verify**

```bash
cabal build wisp-tui 2>&1 | grep -E "error" || echo "Build OK"
```

**Step 3: Commit**

```bash
git add wisp-tui/app/Main.hs
git commit -m "feat(tui): add theme attributes for tools and markdown"
```

---

## Task 9: Create markdown rendering module

**Files:**
- Create: `wisp-tui/src/Tui/Markdown.hs`
- Modify: `wisp-tui/wisp-tui.cabal`

**Step 1: Create the module**

```haskell
module Tui.Markdown
  ( renderMarkdown
  ) where

import Brick
import Data.Text (Text)
import qualified Data.Text as T

import Tui.Types (Name)

-- | Render text as markdown with basic formatting
renderMarkdown :: Text -> Widget Name
renderMarkdown content = vBox $ map renderLine $ T.lines content

renderLine :: Text -> Widget Name
renderLine line
  -- Headers
  | "### " `T.isPrefixOf` line =
      withAttr (attrName "mdHeader") $ txt $ T.drop 4 line
  | "## " `T.isPrefixOf` line =
      withAttr (attrName "mdHeader") $ txt $ T.drop 3 line
  | "# " `T.isPrefixOf` line =
      withAttr (attrName "mdHeader") $ txt $ T.drop 2 line
  -- Bullet points
  | "- " `T.isPrefixOf` line =
      hBox [ withAttr (attrName "mdBullet") $ txt "• "
           , renderInline $ T.drop 2 line
           ]
  | "* " `T.isPrefixOf` line =
      hBox [ withAttr (attrName "mdBullet") $ txt "• "
           , renderInline $ T.drop 2 line
           ]
  -- Normal lines
  | otherwise = renderInline line

-- | Render inline formatting (bold, code)
renderInline :: Text -> Widget Name
renderInline txt'
  | T.null txt' = emptyWidget
  | otherwise = hBox $ parseInline txt'

parseInline :: Text -> [Widget Name]
parseInline t
  | T.null t = []
  | Just rest <- T.stripPrefix "**" t =
      case T.breakOn "**" rest of
        (bold, after) | not (T.null after) ->
          withAttr (attrName "mdBold") (txt bold) : parseInline (T.drop 2 after)
        _ -> txt "**" : parseInline rest
  | Just rest <- T.stripPrefix "`" t =
      case T.breakOn "`" rest of
        (code, after) | not (T.null after) ->
          withAttr (attrName "mdCode") (txt code) : parseInline (T.drop 1 after)
        _ -> txt "`" : parseInline rest
  | otherwise =
      let (plain, rest) = T.break (\c -> c == '*' || c == '`') t
      in if T.null plain
         then txt (T.take 1 t) : parseInline (T.drop 1 t)
         else txtWrap plain : parseInline rest
```

**Step 2: Add to cabal file**

In `wisp-tui.cabal`, add to `other-modules`:

```
        Tui.Markdown
```

**Step 3: Build to verify**

```bash
cabal build wisp-tui 2>&1 | grep -E "error" || echo "Build OK"
```

**Step 4: Commit**

```bash
git add wisp-tui/src/Tui/Markdown.hs wisp-tui/wisp-tui.cabal
git commit -m "feat(tui): add basic markdown rendering module"
```

---

## Task 10: Use markdown rendering in Chat view

**Files:**
- Modify: `wisp-tui/src/Tui/Views/Chat.hs`

**Step 1: Import markdown module**

```haskell
import Tui.Markdown (renderMarkdown)
```

**Step 2: Replace renderMultiline with renderMarkdown**

Change `renderMessage`:

```haskell
renderMessage :: ChatMessage -> Widget Name
renderMessage msg = padBottom (Pad 1) $ vBox
  [ withAttr (roleAttr $ cmRole msg) $ txt $ "[" <> cmRole msg <> "]"
  , padLeft (Pad 2) $ renderMarkdown (cmContent msg)  -- CHANGED from renderMultiline
  ]
```

**Step 3: Update streamingIndicator**

```haskell
streamingIndicator :: ChatState -> [Widget Name]
streamingIndicator cs
  | cs ^. csStreaming =
      toolCallsWidget (cs ^. csToolCalls)
      ++ [ padLeft (Pad 2) $ renderMarkdown $ cs ^. csStreamBuffer  -- CHANGED
         , withAttr (attrName "cursor") $ txt "▌"
         ]
  | otherwise = []
```

**Step 4: Build to verify**

```bash
cabal build wisp-tui 2>&1 | grep -E "error" || echo "Build OK"
```

**Step 5: Commit**

```bash
git add wisp-tui/src/Tui/Views/Chat.hs
git commit -m "feat(tui): use markdown rendering for chat messages"
```

---

## Task 11: Final verification

**Step 1: Build all packages**

```bash
cabal build all 2>&1 | grep -E "error" || echo "Build OK"
```

**Step 2: Test manually**

Start server and TUI, send a message, verify:
- "Agent thinking..." appears when agent starts
- Tool calls show with ⏳ while pending
- Tool calls show with ✓ and timing when complete
- Markdown headers are cyan and bold
- Code in backticks is yellow
- Lists have bullet points

**Step 3: Final commit**

```bash
git add -A
git commit -m "feat: TUI markdown rendering and agent event display"
```

---

## Summary

| Task | Component | Description |
|------|-----------|-------------|
| 1-2 | Server | Add and emit AgentRunningEvent |
| 3 | Client | Parse AgentRunning SSE event |
| 4-6 | TUI State | Add tool tracking, handle events |
| 7-8 | TUI Display | Show tool calls, add theme attrs |
| 9-10 | Markdown | Create renderer, use in Chat view |
| 11 | Verify | Build all, manual test |
