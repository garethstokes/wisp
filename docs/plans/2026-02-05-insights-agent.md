# Insights Agent Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement wisp agent for activity summaries, search, and contact insights.

**Architecture:** The insights agent handles retrospective queries via chat. It searches activities, generates summaries over time periods, and provides insights about contacts. Uses the same LLM chat pattern as Scheduler but with search and aggregation tools.

**Tech Stack:** Haskell, PostgreSQL, Claude API, same patterns as Agents.Scheduler

---

## Task 1: Add Activity Search and Aggregation DB Functions

**Files:**
- Modify: `wisp-srv/src/Infra/Db/Activity.hs`

**Step 1: Add new exports**

Add to the export list in `wisp-srv/src/Infra/Db/Activity.hs`:

```haskell
module Infra.Db.Activity
  ( -- existing exports...
  , searchActivities
  , getActivitySummaryStats
  ) where
```

**Step 2: Add searchActivities function**

Add after `getUpcomingCalendarEvents`:

```haskell
-- Search activities by title/summary text
searchActivities :: Text -> Int -> App [Activity]
searchActivities searchTerm limit = do
  conn <- getConn
  let pattern = "%" <> searchTerm <> "%"
  results <- liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where title ilike ? or summary ilike ? or sender_email ilike ? \
    \order by created_at desc \
    \limit ?"
    (pattern, pattern, pattern, limit)
  pure $ map unDbActivity results

-- Get activity counts grouped by status and source
getActivitySummaryStats :: App [(Text, Text, Int)]
getActivitySummaryStats = do
  conn <- getConn
  liftIO $ query_ conn
    "select source, status, count(*)::int \
    \from activities \
    \group by source, status \
    \order by source, status"
```

**Step 3: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Infra/Db/Activity.hs
git commit -m "feat: add activity search and summary stats queries"
```

---

## Task 2: Create Insights Agent Module

**Files:**
- Replace: `wisp-srv/src/Agents/Insights.hs`

**Step 1: Replace stub with full module**

Replace `wisp-srv/src/Agents/Insights.hs`:

```haskell
module Agents.Insights
  ( -- Decision flows
    handleChat
    -- Types
  , InsightsToolCall(..)
  , SearchQuery(..)
  , SummaryQuery(..)
  , ToolResult(..)
    -- Dispatcher
  , executeToolCall
    -- Agent metadata
  , agentInfo
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), decode, object, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Zones (TZ, loadSystemTZ, utcToLocalTimeTZ)
import App.Monad (App, Env(..))
import App.Config (Config(..), ClaudeConfig(..))
import Domain.Activity (Activity(..))
import Domain.Person (Person(..))
import Domain.Id (unEntityId)
import Infra.Db.Activity (searchActivities, getActivitySummaryStats, getRecentActivities, insertConversation)
import Infra.Db.Person (getAllPeople, getImportantPeople, searchPeople)
import Infra.Claude.Client (callClaudeWithSystem)
import Domain.Agent (AgentInfo(..), ToolInfo(..), ToolType(..))
import Domain.Chat (ChatMessage(..), ChatResponse)
import qualified Domain.Chat as Chat

--------------------------------------------------------------------------------
-- Timezone Loading
--------------------------------------------------------------------------------

loadTimezone :: Text -> IO (Maybe TZ)
loadTimezone tzName = do
  result <- try $ loadSystemTZ (T.unpack tzName)
  case result of
    Left (_ :: SomeException) -> pure Nothing
    Right tz -> pure (Just tz)

--------------------------------------------------------------------------------
-- Tool Call Types
--------------------------------------------------------------------------------

data InsightsToolCall
  = SearchActivities SearchQuery
  | GetSummary SummaryQuery
  | GetPeopleInsights PeopleQuery
  deriving (Show, Eq)

data SearchQuery = SearchQuery
  { searchTerm :: Text
  , searchLimit :: Maybe Int
  } deriving (Show, Eq)

data SummaryQuery = SummaryQuery
  { summaryHours :: Maybe Int  -- Look back N hours (default 24)
  } deriving (Show, Eq)

data PeopleQuery = PeopleQuery
  { peopleSearch :: Maybe Text
  , peopleImportantOnly :: Maybe Bool
  } deriving (Show, Eq)

data ToolResult
  = ToolSuccess Value
  | ToolError Text
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- JSON Instances
--------------------------------------------------------------------------------

instance FromJSON InsightsToolCall where
  parseJSON = withObject "InsightsToolCall" $ \v -> do
    tool <- v .: "tool"
    case (tool :: Text) of
      "search_activities" -> SearchActivities <$> parseJSON (Object v)
      "get_summary" -> GetSummary <$> parseJSON (Object v)
      "get_people_insights" -> GetPeopleInsights <$> parseJSON (Object v)
      _ -> fail $ "Unknown tool: " <> T.unpack tool

instance FromJSON SearchQuery where
  parseJSON = withObject "SearchQuery" $ \v -> SearchQuery
    <$> v .: "query"
    <*> v .:? "limit"

instance FromJSON SummaryQuery where
  parseJSON = withObject "SummaryQuery" $ \v -> SummaryQuery
    <$> v .:? "hours"

instance FromJSON PeopleQuery where
  parseJSON = withObject "PeopleQuery" $ \v -> PeopleQuery
    <$> v .:? "search"
    <*> v .:? "important_only"

instance ToJSON ToolResult where
  toJSON (ToolSuccess v) = object ["success" .= True, "data" .= v]
  toJSON (ToolError err) = object ["success" .= False, "error" .= err]

--------------------------------------------------------------------------------
-- Tool Dispatcher
--------------------------------------------------------------------------------

executeToolCall :: Maybe TZ -> InsightsToolCall -> App ToolResult
executeToolCall mTz (SearchActivities q) = do
  let limit = fromMaybe 20 (searchLimit q)
  activities <- searchActivities (searchTerm q) limit
  pure $ ToolSuccess $ object
    [ "activities" .= map (activityToJson mTz) activities
    , "count" .= length activities
    , "query" .= searchTerm q
    ]

executeToolCall _ (GetSummary q) = do
  let hours = fromMaybe 24 (summaryHours q)
  recent <- getRecentActivities hours
  stats <- getActivitySummaryStats
  pure $ ToolSuccess $ object
    [ "period_hours" .= hours
    , "recent_count" .= length recent
    , "breakdown" .= [object ["source" .= s, "status" .= st, "count" .= c] | (s, st, c) <- stats]
    ]

executeToolCall _ (GetPeopleInsights q) = do
  people <- case (peopleSearch q, fromMaybe False (peopleImportantOnly q)) of
    (Just search, _) -> searchPeople search 20
    (_, True) -> getImportantPeople
    (_, False) -> getAllPeople
  pure $ ToolSuccess $ object
    [ "people" .= map personToJson people
    , "count" .= length people
    ]

activityToJson :: Maybe TZ -> Activity -> Value
activityToJson mTz a = object
  [ "id" .= unEntityId (activityId a)
  , "title" .= activityTitle a
  , "summary" .= activitySummary a
  , "source" .= show (activitySource a)
  , "status" .= show (activityStatus a)
  , "sender" .= activitySenderEmail a
  , "created_at" .= fmap (formatTime mTz) (Just $ activityCreatedAt a)
  ]

personToJson :: Person -> Value
personToJson p = object
  [ "email" .= personEmail p
  , "name" .= personDisplayName p
  , "contact_count" .= personContactCount p
  , "relationship" .= personRelationship p
  , "last_contact" .= personLastContact p
  ]

formatTime :: Maybe TZ -> Maybe a -> Text
formatTime _ Nothing = ""
formatTime Nothing (Just _) = ""
formatTime (Just _) (Just _) = ""  -- Will be fixed in step 3

--------------------------------------------------------------------------------
-- Chat Handler
--------------------------------------------------------------------------------

data LLMResponse = LLMResponse
  { llmMessage :: Text
  , llmToolCall :: Maybe InsightsToolCall
  } deriving (Show, Eq)

instance FromJSON LLMResponse where
  parseJSON = withObject "LLMResponse" $ \v -> LLMResponse
    <$> v .: "message"
    <*> v .:? "tool_call"

handleChat :: [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
handleChat messages mTzName = do
  let userMessages = [m | m <- messages, messageRole m == "user"]
  case userMessages of
    [] -> pure $ Left "No user message provided"
    _ -> do
      let query = messageContent (last userMessages)

      mTz <- case mTzName of
        Nothing -> pure Nothing
        Just tzName -> liftIO $ loadTimezone tzName

      -- Get context
      stats <- getActivitySummaryStats
      recentCount <- length <$> getRecentActivities 24

      let systemPrompt = buildChatPrompt recentCount stats
      let conversationPrompt = buildConversationPrompt messages

      claudeCfg <- asks (claude . config)
      result <- liftIO $ callClaudeWithSystem
        (apiKey claudeCfg)
        (model claudeCfg)
        systemPrompt
        conversationPrompt

      case result of
        Left err -> pure $ Left err
        Right response -> do
          _ <- insertConversation query response
          case parseLLMResponse response of
            Left err -> pure $ Left err
            Right llmResp -> do
              case llmToolCall llmResp of
                Nothing -> pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
                Just toolCall -> do
                  toolResult <- executeToolCall mTz toolCall
                  case toolResult of
                    ToolSuccess _ -> pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
                    ToolError err -> pure $ Left err

buildConversationPrompt :: [ChatMessage] -> Text
buildConversationPrompt msgs = T.unlines
  [ role <> ": " <> messageContent m
  | m <- msgs
  , let role = case messageRole m of
          "user" -> "User"
          "assistant" -> fromMaybe "Assistant" (messageAgent m)
          "tool" -> "Tool Result"
          r -> r
  ]

buildChatPrompt :: Int -> [(Text, Text, Int)] -> Text
buildChatPrompt recentCount stats = T.unlines
  [ "You are Wisp's insights agent. You help users understand their activity patterns and find information."
  , ""
  , "## Response Format"
  , "IMPORTANT: Respond with ONLY valid JSON, no other text."
  , ""
  , "{\"message\": \"Your response\", \"tool_call\": null}"
  , ""
  , "Or with a tool call:"
  , ""
  , "{\"message\": \"Your response\", \"tool_call\": {\"tool\": \"...\", ...}}"
  , ""
  , "## Available Tools"
  , ""
  , "### search_activities - Find activities by keyword"
  , "```json"
  , "{\"tool\": \"search_activities\", \"query\": \"meeting notes\", \"limit\": 10}"
  , "```"
  , ""
  , "### get_summary - Get activity summary stats"
  , "```json"
  , "{\"tool\": \"get_summary\", \"hours\": 24}"
  , "```"
  , ""
  , "### get_people_insights - Get contact insights"
  , "```json"
  , "{\"tool\": \"get_people_insights\", \"important_only\": true}"
  , "{\"tool\": \"get_people_insights\", \"search\": \"john\"}"
  , "```"
  , ""
  , "## Style"
  , "- Present insights naturally, not as raw data dumps"
  , "- Highlight interesting patterns or anomalies"
  , "- Be concise but informative"
  , ""
  , "## Current Context"
  , "- Activities in last 24h: " <> T.pack (show recentCount)
  , "- Breakdown: " <> formatStats stats
  ]

formatStats :: [(Text, Text, Int)] -> Text
formatStats [] = "(no data)"
formatStats stats = T.intercalate ", " [s <> "/" <> st <> ": " <> T.pack (show c) | (s, st, c) <- stats]

parseLLMResponse :: Text -> Either Text LLMResponse
parseLLMResponse raw =
  let extracted = extractJson raw
      jsonBytes = BL.fromStrict (encodeUtf8 extracted)
  in case decode jsonBytes of
    Just resp -> Right resp
    Nothing -> Left $ "Failed to parse response: " <> T.take 200 raw

extractJson :: Text -> Text
extractJson t =
  let stripped = stripCodeBlock t
      startIdx = T.findIndex (== '{') stripped
      endIdx = findLastIndex (== '}') stripped
  in case (startIdx, endIdx) of
    (Just s, Just e) | e >= s -> T.drop s $ T.take (e + 1) stripped
    _ -> stripped

findLastIndex :: (Char -> Bool) -> Text -> Maybe Int
findLastIndex p t =
  let len = T.length t
      indices = [i | i <- [0..len-1], p (T.index t i)]
  in if null indices then Nothing else Just (last indices)

stripCodeBlock :: Text -> Text
stripCodeBlock t =
  let lines' = T.lines t
      withoutStart = case lines' of
        (l:rest) | "```" `T.isPrefixOf` l -> rest
        other -> other
      withoutEnd = case reverse withoutStart of
        (l:rest) | l == "```" -> reverse rest
        other -> reverse other
  in T.unlines withoutEnd

--------------------------------------------------------------------------------
-- Agent Metadata
--------------------------------------------------------------------------------

agentInfo :: AgentInfo
agentInfo = AgentInfo
  { agentId = "wisp"
  , agentDescription = "Activity search, summaries, and contact insights"
  , agentTools =
      [ ToolInfo "search_activities" Decision
      , ToolInfo "get_summary" Decision
      , ToolInfo "get_people_insights" Decision
      ]
  , agentWorkflows = ["search", "summarize", "people-insights"]
  , agentImplemented = True
  }
```

**Step 2: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds (may have warnings about formatTime)

**Step 3: Commit**

```bash
git add wisp-srv/src/Agents/Insights.hs
git commit -m "feat: implement insights agent with search and summary tools"
```

---

## Task 3: Fix formatTime and Add Timezone Support

**Files:**
- Modify: `wisp-srv/src/Agents/Insights.hs`

**Step 1: Fix the formatTime function**

Replace the placeholder `formatTime` in `wisp-srv/src/Agents/Insights.hs`:

```haskell
formatTime :: Maybe TZ -> UTCTime -> Text
formatTime Nothing utc = T.pack $ show utc
formatTime (Just tz) utc = T.pack $ show $ utcToLocalTimeTZ tz utc
```

**Step 2: Update activityToJson to use the fixed formatTime**

Replace the `activityToJson` function:

```haskell
activityToJson :: Maybe TZ -> Activity -> Value
activityToJson mTz a = object
  [ "id" .= unEntityId (activityId a)
  , "title" .= activityTitle a
  , "summary" .= activitySummary a
  , "source" .= show (activitySource a)
  , "status" .= show (activityStatus a)
  , "sender" .= activitySenderEmail a
  , "created_at" .= formatTime mTz (activityCreatedAt a)
  ]
```

**Step 3: Add UTCTime import**

Add to imports:

```haskell
import Data.Time (UTCTime)
```

**Step 4: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds with no warnings

**Step 5: Commit**

```bash
git add wisp-srv/src/Agents/Insights.hs
git commit -m "fix: add timezone support to insights agent"
```

---

## Task 4: Wire Up Insights in Dispatcher

**Files:**
- Modify: `wisp-srv/src/Agents/Dispatcher.hs`

**Step 1: Update dispatcher**

Replace the insights dispatch case in `wisp-srv/src/Agents/Dispatcher.hs`:

```haskell
dispatchChat "wisp" msgs tz = Insights.handleChat msgs tz
```

(Change from: `dispatchChat "wisp" _ _ = pure $ Left "Agent 'wisp' not yet implemented"`)

**Step 2: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Agents/Dispatcher.hs
git commit -m "feat: wire insights agent into dispatcher"
```

---

## Task 5: Add Insights Tests

**Files:**
- Create: `wisp-srv/test/Agents/InsightsSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create test file**

Create `wisp-srv/test/Agents/InsightsSpec.hs`:

```haskell
module Agents.InsightsSpec (spec) where

import Test.Hspec
import Data.Aeson (decode)

import Agents.Insights
import Domain.Agent (agentId, agentImplemented)

spec :: Spec
spec = do
  describe "Insights" $ do
    describe "agentInfo" $ do
      it "has correct agent ID" $ do
        agentId agentInfo `shouldBe` "wisp"

      it "is marked as implemented" $ do
        agentImplemented agentInfo `shouldBe` True

    describe "SearchQuery parsing" $ do
      it "parses search_activities with query" $ do
        let json = "{\"tool\": \"search_activities\", \"query\": \"meeting\"}"
        let result = decode json :: Maybe InsightsToolCall
        case result of
          Just (SearchActivities q) -> searchTerm q `shouldBe` "meeting"
          _ -> expectationFailure "Expected SearchActivities"

      it "parses search_activities with limit" $ do
        let json = "{\"tool\": \"search_activities\", \"query\": \"test\", \"limit\": 5}"
        let result = decode json :: Maybe InsightsToolCall
        case result of
          Just (SearchActivities q) -> searchLimit q `shouldBe` Just 5
          _ -> expectationFailure "Expected SearchActivities"

    describe "SummaryQuery parsing" $ do
      it "parses get_summary with hours" $ do
        let json = "{\"tool\": \"get_summary\", \"hours\": 48}"
        let result = decode json :: Maybe InsightsToolCall
        case result of
          Just (GetSummary q) -> summaryHours q `shouldBe` Just 48
          _ -> expectationFailure "Expected GetSummary"

      it "parses get_summary with defaults" $ do
        let json = "{\"tool\": \"get_summary\"}"
        let result = decode json :: Maybe InsightsToolCall
        case result of
          Just (GetSummary q) -> summaryHours q `shouldBe` Nothing
          _ -> expectationFailure "Expected GetSummary"

    describe "PeopleQuery parsing" $ do
      it "parses get_people_insights with search" $ do
        let json = "{\"tool\": \"get_people_insights\", \"search\": \"john\"}"
        let result = decode json :: Maybe InsightsToolCall
        case result of
          Just (GetPeopleInsights q) -> peopleSearch q `shouldBe` Just "john"
          _ -> expectationFailure "Expected GetPeopleInsights"

      it "parses get_people_insights with important_only" $ do
        let json = "{\"tool\": \"get_people_insights\", \"important_only\": true}"
        let result = decode json :: Maybe InsightsToolCall
        case result of
          Just (GetPeopleInsights q) -> peopleImportantOnly q `shouldBe` Just True
          _ -> expectationFailure "Expected GetPeopleInsights"
```

**Step 2: Add to cabal test modules**

In `wisp-srv/wisp-srv.cabal`, add `Agents.InsightsSpec` to the test-suite other-modules list (after `Agents.SchedulerSpec`).

**Step 3: Run tests**

Run: `cabal test wisp-srv-test`
Expected: All tests pass

**Step 4: Commit**

```bash
git add wisp-srv/test/Agents/InsightsSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "test: add insights agent tests"
```

---

## Task 6: Update Documentation

**Files:**
- Modify: `docs/agents.md`
- Modify: `docs/tools.md`

**Step 1: Update agents.md**

Replace the insights section in `docs/agents.md`:

```markdown
### wisp [IMPLEMENTED]

Activity search, summaries, and contact insights.

**Deterministic flows:** (none)

**Decision flows:**
- `search` - Search activities by keyword
- `summarize` - Get activity summary stats
- `people-insights` - Get contact insights

**Tools:**
- `search_activities` (decision) - Find activities by keyword
- `get_summary` (decision) - Get activity counts and breakdown
- `get_people_insights` (decision) - Get contact information
```

**Step 2: Update tools.md**

Add to `docs/tools.md` after find_free_slots:

```markdown
### search_activities (insights_tools)

Type: decision

Used by insights agent to search activities.

**Input:**
```json
{
  "query": "Text (search term)",
  "limit": "Int | null (default 20)"
}
```

**Output:**
```json
{
  "activities": [Activity],
  "count": "Int",
  "query": "Text"
}
```

### get_summary (insights_tools)

Type: decision

Used by insights agent to get activity statistics.

**Input:**
```json
{
  "hours": "Int | null (default 24)"
}
```

**Output:**
```json
{
  "period_hours": "Int",
  "recent_count": "Int",
  "breakdown": [{"source": "Text", "status": "Text", "count": "Int"}]
}
```

### get_people_insights (insights_tools)

Type: decision

Used by insights agent to get contact information.

**Input:**
```json
{
  "search": "Text | null",
  "important_only": "Bool | null (default false)"
}
```

**Output:**
```json
{
  "people": [Person],
  "count": "Int"
}
```
```

**Step 3: Commit**

```bash
git add docs/agents.md docs/tools.md
git commit -m "docs: update insights agent documentation"
```

---

## Task 7: Final Build and Test

**Files:** None (verification only)

**Step 1: Full build**

Run: `cabal build all`
Expected: Build succeeds with no errors

**Step 2: Run all tests**

Run: `cabal test wisp-srv-test`
Expected: All tests pass

**Step 3: Test manually (optional)**

```bash
wisp chat -a wisp -m "Search for emails about meetings"
wisp chat -a wisp -m "How many activities did I have today?"
wisp chat -a wisp -m "Who do I interact with most?"
```

**Step 4: Final commit (if any cleanup needed)**

```bash
git add -A
git commit -m "feat: complete insights agent implementation"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Add DB search/stats queries | `Infra/Db/Activity.hs` |
| 2 | Create Insights module | `Agents/Insights.hs` |
| 3 | Fix timezone support | `Agents/Insights.hs` |
| 4 | Wire into Dispatcher | `Agents/Dispatcher.hs` |
| 5 | Add tests | `test/Agents/InsightsSpec.hs` |
| 6 | Update docs | `docs/agents.md`, `docs/tools.md` |
| 7 | Verify build and tests | (verification) |

**Dependencies:**
- Tasks 1-3 must be sequential
- Task 4 depends on Task 2
- Task 5 depends on Task 2
- Task 6 can run after Task 4
- Task 7 runs last
