# Phase 6: Conversational Agent Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add conversational chat capability to wisp, allowing natural language queries about activities, calendar, and people.

**Architecture:** Context assembly gathers relevant data (today's activities, calendar events, people) before calling Claude. The LLM receives structured context + user query and returns a natural response. Conversations are logged as activities for future reference.

**Tech Stack:** Haskell, scotty (HTTP), postgresql-simple, aeson (JSON), http-client (Claude API), optparse-applicative (CLI)

---

## Task 1: Add DB queries for chat context

**Files:**
- Modify: `wisp-srv/src/Infra/Db/Activity.hs`
- Test: `wisp-srv/test/Infra/Db/ActivitySpec.hs`

**Step 1: Add getRecentActivities export**

In `wisp-srv/src/Infra/Db/Activity.hs`, update exports:

```haskell
module Infra.Db.Activity
  ( insertActivity
  , activityExists
  , activityExistsForAccount
  , getActivity
  , getActivitiesByStatus
  , getActivitiesForToday
  , getRecentActivities
  , getTodaysCalendarEvents
  , updateActivityStatus
  , updateActivityClassification
  ) where
```

**Step 2: Add getRecentActivities function**

After `getActivitiesForToday`, add:

```haskell
-- Get activities from the last N hours
getRecentActivities :: Int -> App [Activity]
getRecentActivities hours = do
  conn <- getConn
  liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where created_at > now() - interval '1 hour' * ? \
    \order by created_at desc \
    \limit 50"
    (Only hours)
```

**Step 3: Add getTodaysCalendarEvents function**

After `getRecentActivities`, add:

```haskell
-- Get today's calendar events
getTodaysCalendarEvents :: App [Activity]
getTodaysCalendarEvents = do
  conn <- getConn
  liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where source = 'calendar' \
    \  and starts_at >= date_trunc('day', now()) \
    \  and starts_at < date_trunc('day', now()) + interval '1 day' \
    \order by starts_at"
    ()
```

**Step 4: Build to verify**

Run: `cabal build wisp-srv`
Expected: Compiles with no errors

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Db/Activity.hs
git commit -m "feat: add getRecentActivities and getTodaysCalendarEvents queries"
```

---

## Task 2: Create ChatContext domain type

**Files:**
- Create: `wisp-srv/src/Domain/Chat.hs`
- Modify: `wisp-srv/wisp-srv.cabal`
- Test: `wisp-srv/test/Domain/ChatSpec.hs`

**Step 1: Create Domain/Chat.hs**

Create `wisp-srv/src/Domain/Chat.hs`:

```haskell
module Domain.Chat
  ( ChatContext(..)
  , ChatRequest(..)
  , ChatResponse(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), withObject, (.:))
import Data.Text (Text)
import Domain.Activity (Activity)
import Domain.Person (Person)

-- Context assembled for the LLM
data ChatContext = ChatContext
  { contextCalendarEvents :: [Activity]
  , contextRecentActivities :: [Activity]
  , contextQuarantineCount :: Int
  , contextSurfacedCount :: Int
  , contextMentionedPeople :: [Person]
  } deriving (Show)

-- Incoming chat request
data ChatRequest = ChatRequest
  { chatMessage :: Text
  } deriving (Show)

instance FromJSON ChatRequest where
  parseJSON = withObject "ChatRequest" $ \v ->
    ChatRequest <$> v .: "message"

-- Chat response
data ChatResponse = ChatResponse
  { responseMessage :: Text
  } deriving (Show)

instance ToJSON ChatResponse where
  toJSON r = object ["message" .= responseMessage r]
```

**Step 2: Add to executable cabal other-modules**

In `wisp-srv/wisp-srv.cabal`, add to executable other-modules (after Domain.Classification):

```
        Domain.Chat
```

**Step 3: Add to test-suite cabal other-modules**

In `wisp-srv/wisp-srv.cabal`, add to test-suite other-modules (after Domain.ClassificationSpec):

```
        Domain.Chat
        Domain.ChatSpec
```

**Step 4: Create test spec**

Create `wisp-srv/test/Domain/ChatSpec.hs`:

```haskell
module Domain.ChatSpec where

import Test.Hspec
import Data.Aeson (decode, encode)
import Domain.Chat

spec :: Spec
spec = describe "Chat" $ do
  describe "ChatRequest" $ do
    it "parses from JSON" $ do
      let json = "{\"message\": \"hello\"}"
      let result = decode json :: Maybe ChatRequest
      fmap chatMessage result `shouldBe` Just "hello"

  describe "ChatResponse" $ do
    it "serializes to JSON" $ do
      let resp = ChatResponse "hi there"
      encode resp `shouldBe` "{\"message\":\"hi there\"}"
```

**Step 5: Build and test**

Run: `cabal build wisp-srv && cabal test wisp-srv-test`
Expected: All tests pass

**Step 6: Commit**

```bash
git add wisp-srv/src/Domain/Chat.hs wisp-srv/test/Domain/ChatSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Chat domain types"
```

---

## Task 3: Create ChatService for context assembly

**Files:**
- Create: `wisp-srv/src/Services/Chat.hs`
- Modify: `wisp-srv/wisp-srv.cabal`
- Test: `wisp-srv/test/Services/ChatSpec.hs`

**Step 1: Create Services/Chat.hs**

Create `wisp-srv/src/Services/Chat.hs`:

```haskell
module Services.Chat
  ( assembleContext
  , buildPrompt
  , processChat
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.Activity (Activity(..), ActivityStatus(..))
import Domain.Chat (ChatContext(..))
import Domain.Person (Person(..))
import Infra.Db.Activity (getRecentActivities, getTodaysCalendarEvents, getActivitiesByStatus)
import Infra.Db.Person (getAllPeople)
import App.Monad (App)

-- Assemble context for the LLM
assembleContext :: Text -> App ChatContext
assembleContext _query = do
  calendar <- getTodaysCalendarEvents
  recent <- getRecentActivities 24
  quarantined <- getActivitiesByStatus Quarantined 100
  surfaced <- getActivitiesByStatus Surfaced 100
  -- For now, fetch all people; later can filter by query mentions
  _allPeople <- getAllPeople
  pure ChatContext
    { contextCalendarEvents = calendar
    , contextRecentActivities = recent
    , contextQuarantineCount = length quarantined
    , contextSurfacedCount = length surfaced
    , contextMentionedPeople = []  -- TODO: extract mentions from query
    }

-- Build the prompt for Claude
buildPrompt :: ChatContext -> Text -> Text
buildPrompt ctx query = T.unlines
  [ "You are Wisp, a personal assistant. You help by providing information"
  , "and options, never pressure or demands."
  , ""
  , "Rules:"
  , "- Never say \"you should\" or \"don't forget\" - offer observations instead"
  , "- Frame gaps in schedule as possibilities, not obligations"
  , "- When discussing progress, celebrate what happened, don't mention what didn't"
  , "- Be concise and conversational"
  , ""
  , "## Today's Calendar"
  , formatCalendar (contextCalendarEvents ctx)
  , ""
  , "## Recent Activity Summary"
  , "- " <> T.pack (show (length (contextRecentActivities ctx))) <> " activities in last 24 hours"
  , "- " <> T.pack (show (contextQuarantineCount ctx)) <> " items in quarantine"
  , "- " <> T.pack (show (contextSurfacedCount ctx)) <> " items surfaced for attention"
  , ""
  , "## User Question"
  , query
  ]

formatCalendar :: [Activity] -> Text
formatCalendar [] = "No events scheduled today."
formatCalendar events = T.unlines $ map formatEvent events
  where
    formatEvent a = "- " <> maybe "(no title)" id (activityTitle a)
      <> maybe "" (\t -> " at " <> T.pack (show t)) (activityStartsAt a)

-- Process a chat message (context assembly + LLM call placeholder)
processChat :: Text -> App Text
processChat query = do
  ctx <- assembleContext query
  let prompt = buildPrompt ctx query
  -- For now, return the prompt as a placeholder
  -- Will be replaced with actual LLM call in next task
  pure $ "I received your question: " <> query <> "\n\n[Debug: prompt length = "
    <> T.pack (show (T.length prompt)) <> " chars]"
```

**Step 2: Add to cabal other-modules**

In `wisp-srv/wisp-srv.cabal`, add to executable other-modules (after Services.Scheduler):

```
        Services.Chat
```

And to test-suite other-modules (after Services.SchedulerSpec or wherever appropriate):

```
        Services.Chat
        Services.ChatSpec
```

**Step 3: Create test spec**

Create `wisp-srv/test/Services/ChatSpec.hs`:

```haskell
module Services.ChatSpec where

import Test.Hspec
import qualified Data.Text as T
import Domain.Chat (ChatContext(..))
import Services.Chat (buildPrompt)

spec :: Spec
spec = describe "Chat" $ do
  describe "buildPrompt" $ do
    it "includes system prompt and query" $ do
      let ctx = ChatContext
            { contextCalendarEvents = []
            , contextRecentActivities = []
            , contextQuarantineCount = 0
            , contextSurfacedCount = 0
            , contextMentionedPeople = []
            }
      let prompt = buildPrompt ctx "what's on today?"
      prompt `shouldSatisfy` T.isInfixOf "Wisp"
      prompt `shouldSatisfy` T.isInfixOf "what's on today?"
      prompt `shouldSatisfy` T.isInfixOf "No events scheduled today"
```

**Step 4: Build and test**

Run: `cabal build wisp-srv && cabal test wisp-srv-test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add wisp-srv/src/Services/Chat.hs wisp-srv/test/Services/ChatSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add ChatService with context assembly"
```

---

## Task 4: Integrate Claude API with ChatService

**Files:**
- Modify: `wisp-srv/src/Services/Chat.hs`
- Modify: `wisp-srv/src/Infra/Claude/Client.hs`

**Step 1: Add callClaudeWithSystem to Claude client**

In `wisp-srv/src/Infra/Claude/Client.hs`, add to exports:

```haskell
module Infra.Claude.Client
  ( ClaudeResponse(..)
  , ClaudeContent(..)
  , callClaude
  , callClaudeWithSystem
  , responseText
  ) where
```

**Step 2: Add the function**

After `callClaude`, add:

```haskell
-- Call Claude API with system prompt
callClaudeWithSystem :: Text -> Text -> Text -> Text -> IO (Either Text Text)
callClaudeWithSystem apiKey model systemPrompt userMessage = do
  manager <- newManager tlsManagerSettings
  let url = "https://api.anthropic.com/v1/messages"
  initReq <- parseRequest url
  let reqBody = object
        [ "model" .= model
        , "max_tokens" .= (2048 :: Int)
        , "system" .= systemPrompt
        , "messages" .= [object ["role" .= ("user" :: Text), "content" .= userMessage]]
        ]
  let req = initReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("x-api-key", encodeUtf8 apiKey)
            , ("anthropic-version", "2023-06-01")
            ]
        , requestBody = RequestBodyLBS (encode reqBody)
        }
  response <- httpLbs req manager
  let status = statusCode (responseStatus response)
  if status == 200
    then case decodeResponse (responseBody response) of
      Just txt -> pure $ Right txt
      Nothing -> pure $ Left "Failed to parse Claude response"
    else pure $ Left $ "Claude API error: " <> decodeUtf8 (toStrict $ responseBody response)
  where
    decodeResponse body = do
      resp <- Aeson.decode body :: Maybe ClaudeResponse
      responseText resp
```

**Step 3: Update ChatService to use LLM**

Replace the `processChat` function in `wisp-srv/src/Services/Chat.hs`:

```haskell
module Services.Chat
  ( assembleContext
  , buildSystemPrompt
  , processChat
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Activity (Activity(..), ActivityStatus(..))
import Domain.Chat (ChatContext(..))
import Domain.Person (Person(..))
import Infra.Db.Activity (getRecentActivities, getTodaysCalendarEvents, getActivitiesByStatus)
import Infra.Db.Person (getAllPeople)
import Infra.Claude.Client (callClaudeWithSystem)
import App.Monad (App, Env(..))
import App.Config (ClaudeConfig(..))

-- Assemble context for the LLM
assembleContext :: Text -> App ChatContext
assembleContext _query = do
  calendar <- getTodaysCalendarEvents
  recent <- getRecentActivities 24
  quarantined <- getActivitiesByStatus Quarantined 100
  surfaced <- getActivitiesByStatus Surfaced 100
  _allPeople <- getAllPeople
  pure ChatContext
    { contextCalendarEvents = calendar
    , contextRecentActivities = recent
    , contextQuarantineCount = length quarantined
    , contextSurfacedCount = length surfaced
    , contextMentionedPeople = []
    }

-- Build the system prompt with context
buildSystemPrompt :: ChatContext -> Text
buildSystemPrompt ctx = T.unlines
  [ "You are Wisp, a personal assistant. You help by providing information"
  , "and options, never pressure or demands."
  , ""
  , "Rules:"
  , "- Never say \"you should\" or \"don't forget\" - offer observations instead"
  , "- Frame gaps in schedule as possibilities, not obligations"
  , "- When discussing progress, celebrate what happened, don't mention what didn't"
  , "- Be concise and conversational"
  , ""
  , "## Today's Calendar"
  , formatCalendar (contextCalendarEvents ctx)
  , ""
  , "## Context"
  , "- " <> T.pack (show (length (contextRecentActivities ctx))) <> " activities in last 24 hours"
  , "- " <> T.pack (show (contextQuarantineCount ctx)) <> " items in quarantine needing review"
  , "- " <> T.pack (show (contextSurfacedCount ctx)) <> " items surfaced for attention"
  ]

formatCalendar :: [Activity] -> Text
formatCalendar [] = "No events scheduled today."
formatCalendar events = T.unlines $ map formatEvent events
  where
    formatEvent a = "- " <> maybe "(no title)" id (activityTitle a)
      <> maybe "" (\t -> " at " <> T.pack (show t)) (activityStartsAt a)

-- Process a chat message
processChat :: Text -> App (Either Text Text)
processChat query = do
  ctx <- assembleContext query
  let systemPrompt = buildSystemPrompt ctx
  claudeCfg <- asks (claude . envConfig)
  result <- liftIO $ callClaudeWithSystem
    (apiKey claudeCfg)
    (model claudeCfg)
    systemPrompt
    query
  pure result
```

**Step 4: Update the test**

Update `wisp-srv/test/Services/ChatSpec.hs` to use `buildSystemPrompt`:

```haskell
module Services.ChatSpec where

import Test.Hspec
import qualified Data.Text as T
import Domain.Chat (ChatContext(..))
import Services.Chat (buildSystemPrompt)

spec :: Spec
spec = describe "Chat" $ do
  describe "buildSystemPrompt" $ do
    it "includes Wisp identity and rules" $ do
      let ctx = ChatContext
            { contextCalendarEvents = []
            , contextRecentActivities = []
            , contextQuarantineCount = 0
            , contextSurfacedCount = 0
            , contextMentionedPeople = []
            }
      let prompt = buildSystemPrompt ctx
      prompt `shouldSatisfy` T.isInfixOf "Wisp"
      prompt `shouldSatisfy` T.isInfixOf "Never say"
      prompt `shouldSatisfy` T.isInfixOf "No events scheduled today"

    it "includes context counts" $ do
      let ctx = ChatContext
            { contextCalendarEvents = []
            , contextRecentActivities = []
            , contextQuarantineCount = 5
            , contextSurfacedCount = 3
            , contextMentionedPeople = []
            }
      let prompt = buildSystemPrompt ctx
      prompt `shouldSatisfy` T.isInfixOf "5 items in quarantine"
      prompt `shouldSatisfy` T.isInfixOf "3 items surfaced"
```

**Step 5: Build and test**

Run: `cabal build wisp-srv && cabal test wisp-srv-test`
Expected: All tests pass

**Step 6: Commit**

```bash
git add wisp-srv/src/Infra/Claude/Client.hs wisp-srv/src/Services/Chat.hs wisp-srv/test/Services/ChatSpec.hs
git commit -m "feat: integrate Claude API with ChatService"
```

---

## Task 5: Add POST /chat HTTP endpoint

**Files:**
- Create: `wisp-srv/src/Http/Handlers/Chat.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create Chat handler**

Create `wisp-srv/src/Http/Handlers/Chat.hs`:

```haskell
module Http.Handlers.Chat
  ( postChat
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Network.HTTP.Types.Status (status400, status500)
import Web.Scotty.Trans (ActionT, json, status, jsonData)
import App.Monad (Env)
import Domain.Chat (ChatRequest(..), ChatResponse(..))
import Services.Chat (processChat)

-- POST /chat
postChat :: ActionT (ReaderT Env IO) ()
postChat = do
  req <- jsonData :: ActionT (ReaderT Env IO) ChatRequest
  let msg = chatMessage req
  if msg == ""
    then do
      status status400
      json $ object ["error" .= ("Message cannot be empty" :: Text)]
    else do
      result <- lift $ processChat msg
      case result of
        Left err -> do
          status status500
          json $ object ["error" .= err]
        Right response -> json $ ChatResponse response
```

**Step 2: Add to cabal other-modules**

In `wisp-srv/wisp-srv.cabal`, add to executable other-modules (after Http.Handlers.Activities):

```
        Http.Handlers.Chat
```

And to test-suite other-modules:

```
        Http.Handlers.Chat
```

**Step 3: Add route**

In `wisp-srv/src/Http/Routes.hs`, add import:

```haskell
import Http.Handlers.Chat (postChat)
```

And add route at the end of `routes`:

```haskell
  -- Chat
  post "/chat" postChat
```

**Step 4: Build and test**

Run: `cabal build wisp-srv && cabal test wisp-srv-test`
Expected: All tests pass

**Step 5: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Chat.hs wisp-srv/src/Http/Routes.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add POST /chat endpoint"
```

---

## Task 6: Add wisp chat CLI command

**Files:**
- Modify: `wisp-cli/app/Main.hs`

**Step 1: Add Chat command type**

In `wisp-cli/app/Main.hs`, update the Command type:

```haskell
data Command
  = Auth
  | Status
  | Poll
  | Classify
  | Today
  | Approve Text
  | Dismiss Text
  | People
  | Activity Text
  | Logs Text
  | Chat Text
  | Help
  deriving (Show)
```

**Step 2: Add chat parser**

After `logsParser`, add:

```haskell
chatParser :: Parser Command
chatParser = Chat <$> strArgument (metavar "MESSAGE" <> help "Message to send to Wisp")
```

**Step 3: Add to command parser**

In `commandParser`, add before `help`:

```haskell
  <> command "chat" (info chatParser (progDesc "Ask Wisp a question"))
```

**Step 4: Add to main dispatch**

In `main`, add:

```haskell
    Chat msg -> runChat msg
```

**Step 5: Update runHelp**

Add to help output:

```haskell
  TIO.putStrLn "  chat MSG     Ask Wisp a question"
```

**Step 6: Add runChat function**

After `runLogs`, add:

```haskell
runChat :: Text -> IO ()
runChat msg = do
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/chat"
  let reqBody = object ["message" .= msg]
  let req = initialReq
        { method = "POST"
        , requestHeaders = [("Content-Type", "application/json")]
        , requestBody = RequestBodyLBS (encode reqBody)
        }
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "message" obj of
      Just (String s) -> TIO.putStrLn s
      _ -> case KM.lookup "error" obj of
        Just (String err) -> TIO.putStrLn $ "Error: " <> err
        _ -> TIO.putStrLn "Unexpected response"
    _ -> TIO.putStrLn "Failed to parse response"
```

**Step 7: Add encode import**

Add to imports:

```haskell
import Data.Aeson (Value(..), decode, encode, object, (.=))
import Data.Aeson.KeyMap (KeyMap)
import Network.HTTP.Client (RequestBody(..))
```

**Step 8: Build**

Run: `cabal build wisp-cli`
Expected: Compiles with no errors

**Step 9: Commit**

```bash
git add wisp-cli/app/Main.hs
git commit -m "feat: add wisp chat command"
```

---

## Task 7: Add conversation logging

**Files:**
- Modify: `wisp-srv/src/Domain/Activity.hs`
- Modify: `wisp-srv/src/Services/Chat.hs`
- Modify: `wisp-srv/src/Infra/Db/Activity.hs`

**Step 1: Add Conversation source**

In `wisp-srv/src/Domain/Activity.hs`, update ActivitySource:

```haskell
data ActivitySource = Email | Calendar | Conversation
  deriving (Eq, Show, Generic)

instance ToJSON ActivitySource where
  toJSON Email = "email"
  toJSON Calendar = "calendar"
  toJSON Conversation = "conversation"

instance FromJSON ActivitySource where
  parseJSON = withText "ActivitySource" $ \case
    "email" -> pure Email
    "calendar" -> pure Calendar
    "conversation" -> pure Conversation
    _ -> fail "Invalid activity source"
```

**Step 2: Update parseSource in Activity.hs DB module**

In `wisp-srv/src/Infra/Db/Activity.hs`, update parseSource:

```haskell
      parseSource :: Text -> ActivitySource
      parseSource "email" = Email
      parseSource "calendar" = Calendar
      parseSource "conversation" = Conversation
      parseSource _ = Email  -- default
```

**Step 3: Add insertConversation function**

In `wisp-srv/src/Infra/Db/Activity.hs`, add to exports:

```haskell
  , insertConversation
```

And add the function:

```haskell
-- Insert a conversation log
insertConversation :: Text -> Text -> App EntityId
insertConversation query response = do
  conn <- getConn
  aid <- liftIO newEntityId
  _ <- liftIO $ execute conn
    "insert into activities \
    \(id, account_id, source, source_id, raw, title, summary, status) \
    \values (?, (select id from accounts limit 1), 'conversation', ?, '{}', ?, ?, 'processed')"
    ( unEntityId aid
    , "chat-" <> unEntityId aid
    , "Chat: " <> T.take 50 query
    , response
    )
  pure aid
```

Add import at top:

```haskell
import qualified Data.Text as T
```

**Step 4: Update ChatService to log conversations**

In `wisp-srv/src/Services/Chat.hs`, update processChat:

```haskell
import Infra.Db.Activity (getRecentActivities, getTodaysCalendarEvents, getActivitiesByStatus, insertConversation)

-- Process a chat message
processChat :: Text -> App (Either Text Text)
processChat query = do
  ctx <- assembleContext query
  let systemPrompt = buildSystemPrompt ctx
  claudeCfg <- asks (claude . envConfig)
  result <- liftIO $ callClaudeWithSystem
    (apiKey claudeCfg)
    (model claudeCfg)
    systemPrompt
    query
  -- Log the conversation
  case result of
    Right response -> do
      _ <- insertConversation query response
      pure $ Right response
    Left err -> pure $ Left err
```

**Step 5: Build and test**

Run: `cabal build wisp-srv && cabal test wisp-srv-test`
Expected: All tests pass

**Step 6: Commit**

```bash
git add wisp-srv/src/Domain/Activity.hs wisp-srv/src/Infra/Db/Activity.hs wisp-srv/src/Services/Chat.hs
git commit -m "feat: add conversation logging to activities"
```

---

## Task 8: Run full test suite and verify

**Step 1: Run all tests**

Run: `cabal test wisp-srv-test`
Expected: All tests pass

**Step 2: Build both packages**

Run: `cabal build all`
Expected: Both wisp-srv and wisp-cli compile

**Step 3: Manual verification (if server running)**

```bash
# Start server in one terminal
cabal run wisp-srv

# In another terminal
cabal run wisp -- chat "what's on my calendar today?"
```

Expected: Response from Claude with calendar info

**Step 4: Final commit if any fixes needed**

```bash
git status
# If clean, done. If changes needed, commit them.
```

---

## Summary

After completing all tasks, you will have:

1. **DB queries** for chat context (recent activities, today's calendar)
2. **Domain types** for chat request/response
3. **ChatService** that assembles context and calls Claude
4. **POST /chat** HTTP endpoint
5. **wisp chat** CLI command
6. **Conversation logging** stored as activities

The flow is:
```
wisp chat "question"
  → POST /chat
  → assembleContext (fetch calendar, activities, counts)
  → buildSystemPrompt (format context for LLM)
  → callClaudeWithSystem (get response)
  → insertConversation (log to DB)
  → return response
```
