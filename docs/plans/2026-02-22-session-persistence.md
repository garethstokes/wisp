# Session Persistence Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Enable chat sessions to persist to the database and reload on TUI startup, fixing two bugs: (1) agent details shows no sessions, (2) chats don't survive TUI restart.

**Architecture:** The server's Dispatcher currently ignores the session ID parameter (prefixed with underscore `_mSessionId`). We'll wire up the existing session DB functions (`Infra.Db.Session`) to create/append sessions during chat, add a `last_message_at` column for the 15-minute resume threshold, and update the TUI to fetch active sessions on startup.

**Tech Stack:** Haskell, PostgreSQL, Brick TUI, Servant API

---

## Task 1: Add `last_message_at` Column

**Files:**
- Create: `wisp-srv/migrations/014_session_last_message.sql`

**Step 1: Write migration file**

```sql
-- Track when last message was added for session resumption logic
ALTER TABLE sessions ADD COLUMN last_message_at TIMESTAMPTZ;

-- Backfill existing sessions
UPDATE sessions SET last_message_at = created_at;

-- Make non-nullable with default
ALTER TABLE sessions ALTER COLUMN last_message_at SET NOT NULL;
ALTER TABLE sessions ALTER COLUMN last_message_at SET DEFAULT now();

-- Index for efficient "recent active session" queries
CREATE INDEX idx_sessions_last_message ON sessions(agent_id, last_message_at DESC)
  WHERE ended_at IS NULL;
```

**Step 2: Run migration**

Run: `psql -d wisp -f wisp-srv/migrations/014_session_last_message.sql`
Expected: ALTER TABLE, UPDATE, CREATE INDEX output

**Step 3: Verify column exists**

Run: `psql -d wisp -c "\d sessions"`
Expected: Shows `last_message_at` column with timestamptz type

**Step 4: Commit**

```bash
git add wisp-srv/migrations/014_session_last_message.sql
git commit -m "feat(db): add last_message_at column to sessions table"
```

---

## Task 2: Update Session Domain Type

**Files:**
- Modify: `wisp-srv/src/Domain/Session.hs`

**Step 1: Read current file**

Read `wisp-srv/src/Domain/Session.hs` to understand current structure.

**Step 2: Add field to Session type**

Add `sessionLastMessageAt :: UTCTime` field to the Session record:

```haskell
data Session = Session
  { sessionId :: SessionId
  , sessionAgentId :: Text
  , sessionMessages :: [ChatMessage]
  , sessionCreatedAt :: UTCTime
  , sessionEndedAt :: Maybe UTCTime
  , sessionSummarized :: Bool
  , sessionLastMessageAt :: UTCTime  -- NEW
  }
```

**Step 3: Build to verify**

Run: `cabal build wisp-srv`
Expected: Build errors in `Infra.Db.Session` (expected - we'll fix in next task)

**Step 4: Commit**

```bash
git add wisp-srv/src/Domain/Session.hs
git commit -m "feat(domain): add sessionLastMessageAt field to Session type"
```

---

## Task 3: Update Session DB Module

**Files:**
- Modify: `wisp-srv/src/Infra/Db/Session.hs`

**Step 1: Update FromRow instance**

Update the `FromRow Session` instance to read the new column (column order: id, agent_id, messages, created_at, ended_at, summarized, last_message_at):

```haskell
instance FromRow Session where
  fromRow = Session
    <$> (SessionId <$> field)
    <*> field
    <*> (parseMessages <$> field)
    <*> field
    <*> field
    <*> field
    <*> field  -- last_message_at
    where
      parseMessages :: Value -> [ChatMessage]
      parseMessages v = case decode (encode v) of
        Just msgs -> msgs
        Nothing -> []
```

**Step 2: Update all SELECT queries**

Update all queries to include `last_message_at`:

```haskell
-- In getSession:
"select id, agent_id, messages, created_at, ended_at, summarized, last_message_at \
\from sessions where id = ?"

-- In getActiveSession:
"select id, agent_id, messages, created_at, ended_at, summarized, last_message_at \
\from sessions where agent_id = ? and ended_at is null \
\order by created_at desc limit 1"

-- In getRecentSessions:
"select id, agent_id, messages, created_at, ended_at, summarized, last_message_at \
\from sessions where agent_id = ? \
\order by created_at desc limit ?"

-- In getUnsummarizedSessions:
"select id, agent_id, messages, created_at, ended_at, summarized, last_message_at \
\from sessions \
\where agent_id = ? and ended_at is not null and not summarized \
\order by created_at"
```

**Step 3: Update createSession to return last_message_at**

```haskell
createSession :: Text -> App Session
createSession agentId = do
  conn <- getConn
  sid <- liftIO $ unEntityId <$> newEntityId
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "insert into sessions (id, agent_id, messages, created_at, last_message_at) values (?, ?, '[]', ?, ?)"
    (sid, agentId, now, now)
  pure Session
    { sessionId = SessionId sid
    , sessionAgentId = agentId
    , sessionMessages = []
    , sessionCreatedAt = now
    , sessionEndedAt = Nothing
    , sessionSummarized = False
    , sessionLastMessageAt = now
    }
```

**Step 4: Update appendMessage to set last_message_at**

```haskell
appendMessage :: SessionId -> ChatMessage -> App Session
appendMessage sid msg = do
  conn <- getConn
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "update sessions set messages = messages || ?::jsonb, last_message_at = ? where id = ?"
    (encode [msg], now, unSessionId sid)
  mSession <- getSession sid
  case mSession of
    Just s -> pure s
    Nothing -> error "Session not found after append"
```

**Step 5: Build to verify**

Run: `cabal build wisp-srv`
Expected: SUCCESS

**Step 6: Commit**

```bash
git add wisp-srv/src/Infra/Db/Session.hs
git commit -m "feat(db): update session queries for last_message_at column"
```

---

## Task 4: Add getOrCreateActiveSession Function

**Files:**
- Modify: `wisp-srv/src/Infra/Db/Session.hs`

**Step 1: Add import for NominalDiffTime**

```haskell
import Data.Time (getCurrentTime, NominalDiffTime, diffUTCTime)
```

**Step 2: Add getOrCreateActiveSession function**

Add to exports and implement:

```haskell
-- | Get active session if last message within threshold, otherwise create new.
-- Returns (session, isNew) where isNew indicates if a fresh session was created.
getOrCreateActiveSession :: Text -> NominalDiffTime -> App (Session, Bool)
getOrCreateActiveSession agentId threshold = do
  now <- liftIO getCurrentTime
  mActive <- getActiveSessionWithinThreshold agentId threshold now
  case mActive of
    Just session -> pure (session, False)
    Nothing -> do
      session <- createSession agentId
      pure (session, True)

-- | Get active session only if last message was within threshold
getActiveSessionWithinThreshold :: Text -> NominalDiffTime -> UTCTime -> App (Maybe Session)
getActiveSessionWithinThreshold agentId threshold now = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, agent_id, messages, created_at, ended_at, summarized, last_message_at \
    \from sessions \
    \where agent_id = ? and ended_at is null \
    \order by last_message_at desc limit 1"
    (Only agentId)
  pure $ case results of
    [s] | diffUTCTime now (sessionLastMessageAt s) <= threshold -> Just s
    _ -> Nothing
```

**Step 3: Update module exports**

```haskell
module Infra.Db.Session
  ( createSession
  , getSession
  , getActiveSession
  , getOrCreateActiveSession  -- NEW
  , getRecentSessions
  , appendMessage
  , endSession
  , markSummarized
  , getUnsummarizedSessions
  ) where
```

**Step 4: Build to verify**

Run: `cabal build wisp-srv`
Expected: SUCCESS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Db/Session.hs
git commit -m "feat(db): add getOrCreateActiveSession with time threshold"
```

---

## Task 5: Integrate Sessions into Dispatcher

**Files:**
- Modify: `wisp-srv/src/Agents/Dispatcher.hs`

**Step 1: Add import for Session module**

```haskell
import qualified Infra.Db.Session as SessionDb
import Domain.Session (sessionId, SessionId(..))
import Domain.Chat (ChatMessage(..))
```

**Step 2: Update dispatchChatNormal to use sessions**

Replace the current `dispatchChatNormal` function. Key changes:
- Remove underscore from `_mSessionId` (now `mSessionId`)
- Get or create session before processing
- Append user message to session
- Append assistant response to session

```haskell
dispatchChatNormal
  :: TenantId
  -> EntityId
  -> Text
  -> [ChatMessage]
  -> Maybe Text
  -> Maybe Text
  -> (ChatEvent -> IO ())
  -> App (Either Text ChatResponse)
dispatchChatNormal tenantId accountId agentName msgs mTimezone _mSessionIdFromClient emit = do
  -- Load the agent from knowledge
  mAgent <- loadAgentByTenant tenantId agentName
  case mAgent of
    Nothing -> pure $ Left $ "Unknown agent: " <> agentName
    Just agent -> do
      -- Get or create session (15 minute threshold)
      (session, _isNew) <- SessionDb.getOrCreateActiveSession agentName (15 * 60)
      let sid = Domain.Session.sessionId session

      -- Append user message to session
      let lastUserMsg = getLastUserMessage msgs
      case lastUserMsg of
        Just userContent -> do
          let userMsg = ChatMessage
                { messageRole = "user"
                , messageContent = userContent
                , messageAgent = Nothing
                , messageToolCall = Nothing
                }
          _ <- SessionDb.appendMessage sid userMsg
          pure ()
        Nothing -> pure ()

      -- Run with logging
      withRunLogging agentName (Just $ unSessionId sid) msgs $ \ctx messages -> do
        -- Fetch knowledge context
        knowledgeCtx <- getKnowledgeContext tenantId agentName []

        -- Load skill prompt if skill is active
        mSkillPrompt <- case agentSkill agent of
          Nothing -> pure Nothing
          Just skill -> loadSkillPromptByTenant tenantId (SkillsRegistry.skillName skill)

        -- Build the system prompt with knowledge
        let systemPrompt = buildSystemPrompt agent mSkillPrompt (Just knowledgeCtx)

        -- Run the agentic loop
        result <- runAgentLoop ctx agent accountId systemPrompt messages [] mTimezone emit

        -- Append assistant response to session
        case result of
          Right response -> do
            let assistantMsg = ChatMessage
                  { messageRole = "assistant"
                  , messageContent = responseMessage response
                  , messageAgent = Just agentName
                  , messageToolCall = responseToolCall response
                  }
            _ <- SessionDb.appendMessage sid assistantMsg
            pure ()
          Left _ -> pure ()

        pure result
  where
    getLastUserMessage :: [ChatMessage] -> Maybe Text
    getLastUserMessage messages =
      case filter (\m -> messageRole m == "user") messages of
        [] -> Nothing
        ms -> Just $ messageContent $ last ms
```

**Step 3: Add SessionId import to Domain.Session**

Update import to get `unSessionId`:

```haskell
import Domain.Session (sessionId, SessionId(..), unSessionId)
```

Wait - we need to check if `unSessionId` is exported. Read `Domain/Session.hs` first.

**Step 4: Build to verify**

Run: `cabal build wisp-srv`
Expected: SUCCESS (or errors we need to fix)

**Step 5: Commit**

```bash
git add wisp-srv/src/Agents/Dispatcher.hs
git commit -m "feat(dispatcher): integrate session persistence into chat flow"
```

---

## Task 6: Add Active Session API Endpoint

**Files:**
- Modify: `wisp-srv/src/Http/Handlers/Agents.hs`
- Modify: `wisp-srv/src/Http/Routes.hs` (if routes defined there)

**Step 1: Read current Agents handler to understand patterns**

Read `wisp-srv/src/Http/Handlers/Agents.hs` to see existing endpoint patterns.

**Step 2: Add getActiveSessionForAgent handler**

```haskell
-- | Get active session for an agent (for TUI resume)
getActiveSessionForAgent :: Text -> App (Maybe SessionWithMessages)
getActiveSessionForAgent agentName = do
  let threshold = 15 * 60  -- 15 minutes
  now <- liftIO getCurrentTime
  mSession <- SessionDb.getActiveSessionWithinThreshold agentName threshold now
  pure $ fmap toSessionWithMessages mSession

data SessionWithMessages = SessionWithMessages
  { swmId :: Text
  , swmAgentId :: Text
  , swmMessages :: [ChatMessage]
  , swmCreatedAt :: UTCTime
  , swmLastMessageAt :: UTCTime
  }

toSessionWithMessages :: Session -> SessionWithMessages
toSessionWithMessages s = SessionWithMessages
  { swmId = unSessionId (sessionId s)
  , swmAgentId = sessionAgentId s
  , swmMessages = sessionMessages s
  , swmCreatedAt = sessionCreatedAt s
  , swmLastMessageAt = sessionLastMessageAt s
  }
```

**Step 3: Add route**

Add route for `GET /api/agents/:name/active-session`

**Step 4: Build to verify**

Run: `cabal build wisp-srv`
Expected: SUCCESS

**Step 5: Test endpoint manually**

Run: `curl http://localhost:3000/api/agents/wisp%2Fconcierge/active-session`
Expected: JSON response with session or null

**Step 6: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Agents.hs wisp-srv/src/Http/Routes.hs
git commit -m "feat(api): add active session endpoint for TUI resume"
```

---

## Task 7: Add Client Function for Active Session

**Files:**
- Modify: `wisp-core/src/Wisp/Client.hs`

**Step 1: Add ActiveSession type**

```haskell
data ActiveSession = ActiveSession
  { asId :: Text
  , asAgentId :: Text
  , asMessages :: [ChatMessage]
  , asCreatedAt :: UTCTime
  , asLastMessageAt :: UTCTime
  } deriving (Show, Eq)

instance FromJSON ActiveSession where
  parseJSON = withObject "ActiveSession" $ \v -> ActiveSession
    <$> v .: "id"
    <*> v .: "agent_id"
    <*> v .: "messages"
    <*> v .: "created_at"
    <*> v .: "last_message_at"
```

**Step 2: Add getActiveSession function**

```haskell
-- | Get active session for an agent (if within 15 min threshold)
getActiveSession :: ClientConfig -> Text -> IO (Either ClientError (Maybe ActiveSession))
getActiveSession cfg agentName = do
  let url = baseUrl cfg <> "/api/agents/" <> encodePathSegment agentName <> "/active-session"
  result <- httpGet cfg url
  case result of
    Left err -> pure (Left err)
    Right body ->
      case decode body of
        Just session -> pure (Right (Just session))
        Nothing ->
          -- Check if response was empty/null
          if BL.null body || body == "null"
            then pure (Right Nothing)
            else pure (Left (ParseError "Failed to parse active session"))
```

**Step 3: Export from module**

Add to exports: `ActiveSession(..)`, `getActiveSession`

**Step 4: Build to verify**

Run: `cabal build wisp-core`
Expected: SUCCESS

**Step 5: Commit**

```bash
git add wisp-core/src/Wisp/Client.hs
git commit -m "feat(client): add getActiveSession for TUI resume"
```

---

## Task 8: Add Chat Session Loader to TUI

**Files:**
- Modify: `wisp-tui/src/Tui/DataLoader.hs`
- Modify: `wisp-tui/src/Tui/Types.hs`

**Step 1: Add ChatSessionLoaded to DataLoadResult**

In `Tui/Types.hs` or `DataLoader.hs`, add:

```haskell
data DataLoadResult
  = ActivitiesLoaded [Activity] (Maybe ActivityMetrics) Bool
  | ActivitiesAppended [Activity] Bool
  | KnowledgeLoaded [Document] [Document]
  | SkillsLoaded [Skill]
  | AgentsLoaded [AgentInfo]
  | AgentSessionsLoaded Text [SessionSummary]
  | ApprovalsLoaded [(Activity, Text, Text)]
  | ChatSessionLoaded (Maybe (Text, [ChatMessage]))  -- NEW: sessionId, messages
  | LoadError Text
  deriving (Show, Eq)
```

**Step 2: Add loadChatSession function**

```haskell
-- | Load active chat session for an agent
loadChatSession :: ClientConfig -> Text -> IO DataLoadResult
loadChatSession cfg agentName = do
  result <- getActiveSession cfg agentName
  pure $ case result of
    Left err -> LoadError $ "Failed to load session: " <> showError err
    Right Nothing -> ChatSessionLoaded Nothing
    Right (Just session) -> ChatSessionLoaded $ Just
      (asId session, asMessages session)
```

**Step 3: Build to verify**

Run: `cabal build wisp-tui`
Expected: SUCCESS

**Step 4: Commit**

```bash
git add wisp-tui/src/Tui/DataLoader.hs wisp-tui/src/Tui/Types.hs
git commit -m "feat(tui): add chat session loader"
```

---

## Task 9: Load Chat History on TUI Startup

**Files:**
- Modify: `wisp-tui/app/Main.hs`
- Modify: `wisp-tui/src/Tui/App.hs` (event handler)

**Step 1: Add event for session loading**

In Types.hs, add to AppEvent:

```haskell
data AppEvent
  = ...
  | ChatSessionLoaded (Maybe (Text, [ChatMessage]))
```

**Step 2: Trigger session load on startup**

In Main.hs after initializing, send a request to load the active session:

```haskell
-- After creating the app, load chat session
void $ async $ do
  result <- DL.loadChatSession cfg (initialState ^. chatState . csCurrentAgent)
  case result of
    DL.ChatSessionLoaded mSession -> writeBChan chan (ChatSessionLoaded mSession)
    _ -> pure ()
```

**Step 3: Handle ChatSessionLoaded event**

In the event handler:

```haskell
handleEvent (ChatSessionLoaded mSession) = do
  case mSession of
    Nothing -> pure ()  -- No active session, start fresh
    Just (sessionId, messages) -> do
      modify $ chatState . csCurrentSession .~ sessionId
      modify $ chatState . csMessages .~ convertMessages messages
  where
    convertMessages = map (\m -> ChatMessage
      { cmRole = if messageRole m == "user" then "You" else "Assistant"
      , cmContent = messageContent m
      })
```

**Step 4: Build and test**

Run: `cabal build wisp-tui && cabal run wisp-tui`
Expected: TUI launches, loads previous session if within 15 minutes

**Step 5: Commit**

```bash
git add wisp-tui/app/Main.hs wisp-tui/src/Tui/App.hs wisp-tui/src/Tui/Types.hs
git commit -m "feat(tui): load chat history on startup"
```

---

## Task 10: End-to-End Verification

**Step 1: Start fresh database state**

Run: `psql -d wisp -c "DELETE FROM sessions;"`

**Step 2: Start server and TUI**

Run server: `cabal run wisp-srv`
Run TUI: `cabal run wisp-tui`

**Step 3: Send a test message**

Type a message in the TUI and press Enter.

**Step 4: Verify session created in database**

Run: `psql -d wisp -c "SELECT id, agent_id, last_message_at, jsonb_array_length(messages) as msg_count FROM sessions;"`
Expected: One row with agent_id='wisp', msg_count=2 (user + assistant)

**Step 5: Restart TUI**

Close TUI (Ctrl+C), restart: `cabal run wisp-tui`

**Step 6: Verify chat history loaded**

Expected: Previous messages appear in the chat view

**Step 7: Check agent details page**

Navigate to Agents tab, select an agent.
Expected: Sessions section shows recent session with message count

**Step 8: Wait 15+ minutes and verify new session**

After waiting, restart TUI and send a message.
Run: `psql -d wisp -c "SELECT COUNT(*) FROM sessions WHERE agent_id='wisp';"`
Expected: 2 sessions (old one + new one)

**Step 9: Final commit**

```bash
git add -A
git commit -m "feat: complete session persistence implementation"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Add last_message_at column | migration |
| 2 | Update Session domain type | Domain/Session.hs |
| 3 | Update Session DB queries | Infra/Db/Session.hs |
| 4 | Add getOrCreateActiveSession | Infra/Db/Session.hs |
| 5 | Integrate sessions in Dispatcher | Agents/Dispatcher.hs |
| 6 | Add active session API endpoint | Http/Handlers/Agents.hs |
| 7 | Add client function | Wisp/Client.hs |
| 8 | Add TUI session loader | Tui/DataLoader.hs |
| 9 | Load history on startup | Main.hs, App.hs |
| 10 | End-to-end verification | - |
