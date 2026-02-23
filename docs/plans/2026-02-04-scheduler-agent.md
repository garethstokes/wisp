# Scheduler Agent Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement wisp agent for calendar reasoning, schedule queries, and finding available time slots.

**Architecture:** The scheduler agent handles calendar-focused queries via chat. It queries calendar events for date ranges, identifies free slots, and helps users reason about their schedule. Uses the same LLM chat pattern as Concierge but with calendar-specific tools and context.

**Tech Stack:** Haskell, PostgreSQL, Claude API, same patterns as Agents.Concierge

---

## Task 1: Add Calendar Query Functions to DB Layer

**Files:**
- Modify: `wisp-srv/src/Infra/Db/Activity.hs`
- Test: `wisp-srv/test/Infra/Db/ActivitySpec.hs`

**Step 1: Add new exports to Activity.hs**

Add to the export list in `wisp-srv/src/Infra/Db/Activity.hs`:

```haskell
module Infra.Db.Activity
  ( -- existing exports...
  , getCalendarEventsInRange
  , getUpcomingCalendarEvents
  ) where
```

**Step 2: Add getCalendarEventsInRange function**

Add after `getTodaysCalendarEvents` in `wisp-srv/src/Infra/Db/Activity.hs`:

```haskell
-- Get calendar events within a date range
getCalendarEventsInRange :: UTCTime -> UTCTime -> App [Activity]
getCalendarEventsInRange startTime endTime = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where source = 'calendar' \
    \  and starts_at >= ? \
    \  and starts_at < ? \
    \order by starts_at"
    (startTime, endTime)
  pure $ map unDbActivity results

-- Get upcoming calendar events (next N days)
getUpcomingCalendarEvents :: Int -> App [Activity]
getUpcomingCalendarEvents days = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where source = 'calendar' \
    \  and starts_at >= now() \
    \  and starts_at < now() + interval '1 day' * ? \
    \order by starts_at"
    (Only days)
  pure $ map unDbActivity results
```

**Step 3: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Infra/Db/Activity.hs
git commit -m "feat: add calendar range query functions"
```

---

## Task 2: Create Scheduler Agent Module Structure

**Files:**
- Create: `wisp-srv/src/Agents/Scheduler.hs` (replace stub)
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Replace the stub Scheduler.hs with full module**

Replace `wisp-srv/src/Agents/Scheduler.hs`:

```haskell
module Agents.Scheduler
  ( -- Decision flows
    handleChat
    -- Types
  , SchedulerToolCall(..)
  , CalendarQuery(..)
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
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay)
import Data.Time.Zones (TZ, loadSystemTZ, utcToLocalTimeTZ)
import App.Monad (App, Env(..))
import App.Config (Config(..), ClaudeConfig(..))
import Domain.Activity (Activity(..))
import Domain.Id (unEntityId)
import Infra.Db.Activity (getTodaysCalendarEvents, getCalendarEventsInRange, getUpcomingCalendarEvents, insertConversation)
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

data SchedulerToolCall
  = QueryCalendar CalendarQuery
  | FindFreeSlots FreeSlotQuery
  deriving (Show, Eq)

data CalendarQuery = CalendarQuery
  { calendarDays :: Maybe Int      -- Number of days to look ahead (default 7)
  , calendarDate :: Maybe Text     -- Specific date (ISO format)
  } deriving (Show, Eq)

data FreeSlotQuery = FreeSlotQuery
  { slotDays :: Maybe Int          -- Days to search (default 7)
  , slotDuration :: Maybe Int      -- Desired duration in minutes (default 60)
  , slotStartHour :: Maybe Int     -- Earliest hour (default 9)
  , slotEndHour :: Maybe Int       -- Latest hour (default 17)
  } deriving (Show, Eq)

data ToolResult
  = ToolSuccess Value
  | ToolError Text
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- JSON Instances
--------------------------------------------------------------------------------

instance FromJSON SchedulerToolCall where
  parseJSON = withObject "SchedulerToolCall" $ \v -> do
    tool <- v .: "tool"
    case (tool :: Text) of
      "query_calendar" -> QueryCalendar <$> parseJSON (Object v)
      "find_free_slots" -> FindFreeSlots <$> parseJSON (Object v)
      _ -> fail $ "Unknown tool: " <> T.unpack tool

instance FromJSON CalendarQuery where
  parseJSON = withObject "CalendarQuery" $ \v -> CalendarQuery
    <$> v .:? "days"
    <*> v .:? "date"

instance FromJSON FreeSlotQuery where
  parseJSON = withObject "FreeSlotQuery" $ \v -> FreeSlotQuery
    <$> v .:? "days"
    <*> v .:? "duration_minutes"
    <*> v .:? "start_hour"
    <*> v .:? "end_hour"

instance ToJSON ToolResult where
  toJSON (ToolSuccess v) = object ["success" .= True, "data" .= v]
  toJSON (ToolError err) = object ["success" .= False, "error" .= err]

--------------------------------------------------------------------------------
-- Tool Dispatcher
--------------------------------------------------------------------------------

executeToolCall :: Maybe TZ -> SchedulerToolCall -> App ToolResult
executeToolCall mTz (QueryCalendar q) = do
  let days = fromMaybe 7 (calendarDays q)
  events <- getUpcomingCalendarEvents days
  pure $ ToolSuccess $ object
    [ "events" .= map (eventToJson mTz) events
    , "count" .= length events
    ]

executeToolCall mTz (FindFreeSlots q) = do
  let days = fromMaybe 7 (slotDays q)
  let duration = fromMaybe 60 (slotDuration q)
  let startH = fromMaybe 9 (slotStartHour q)
  let endH = fromMaybe 17 (slotEndHour q)

  events <- getUpcomingCalendarEvents days
  now <- liftIO getCurrentTime
  let slots = findFreeSlots mTz now days startH endH duration events

  pure $ ToolSuccess $ object
    [ "free_slots" .= slots
    , "count" .= length slots
    , "parameters" .= object
        [ "days" .= days
        , "duration_minutes" .= duration
        , "work_hours" .= (show startH <> ":00-" <> show endH <> ":00")
        ]
    ]

eventToJson :: Maybe TZ -> Activity -> Value
eventToJson mTz a = object
  [ "id" .= unEntityId (activityId a)
  , "title" .= activityTitle a
  , "starts_at" .= fmap (formatTime mTz) (activityStartsAt a)
  , "ends_at" .= fmap (formatTime mTz) (activityEndsAt a)
  ]

formatTime :: Maybe TZ -> UTCTime -> Text
formatTime Nothing utc = T.pack $ show utc
formatTime (Just tz) utc = T.pack $ show $ utcToLocalTimeTZ tz utc

-- Find free slots between events
findFreeSlots :: Maybe TZ -> UTCTime -> Int -> Int -> Int -> Int -> [Activity] -> [Value]
findFreeSlots mTz now days startHour endHour durationMins events =
  -- Simplified: return work hours that don't overlap with events
  -- Real implementation would be more sophisticated
  let slots = [] -- Placeholder - will be implemented in Task 4
  in slots

--------------------------------------------------------------------------------
-- Chat Handler
--------------------------------------------------------------------------------

data LLMResponse = LLMResponse
  { llmMessage :: Text
  , llmToolCall :: Maybe SchedulerToolCall
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

      -- Get calendar context
      todayEvents <- getTodaysCalendarEvents
      upcomingEvents <- getUpcomingCalendarEvents 7

      let systemPrompt = buildChatPrompt mTz todayEvents upcomingEvents
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
          "assistant" -> "Assistant"
          "tool" -> "Tool Result"
          r -> r
  ]

buildChatPrompt :: Maybe TZ -> [Activity] -> [Activity] -> Text
buildChatPrompt mTz todayEvents upcomingEvents = T.unlines
  [ "You are Wisp's scheduler agent. You help with calendar questions and finding time."
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
  , "### query_calendar - Get calendar events"
  , "```json"
  , "{\"tool\": \"query_calendar\", \"days\": 7}"
  , "{\"tool\": \"query_calendar\", \"date\": \"2026-02-05\"}"
  , "```"
  , ""
  , "### find_free_slots - Find available time"
  , "```json"
  , "{\"tool\": \"find_free_slots\", \"days\": 7, \"duration_minutes\": 60, \"start_hour\": 9, \"end_hour\": 17}"
  , "```"
  , ""
  , "## Style"
  , "- Present schedule information naturally, not as a raw list"
  , "- When discussing gaps, frame as possibilities, not obligations"
  , "- Be concise and helpful"
  , "- All times are in the user's local timezone"
  , ""
  , "## Current Context"
  , ""
  , "### Today's Schedule (" <> T.pack (show (length todayEvents)) <> " events)"
  , formatEventsForPrompt mTz todayEvents
  , ""
  , "### Upcoming Week (" <> T.pack (show (length upcomingEvents)) <> " events)"
  , formatEventsForPrompt mTz (take 10 upcomingEvents)
  ]

formatEventsForPrompt :: Maybe TZ -> [Activity] -> Text
formatEventsForPrompt _ [] = "(no events)"
formatEventsForPrompt mTz events = T.unlines $ map formatOne events
  where
    formatOne a = "- " <> formatEventTime mTz a <> fromMaybe "(no title)" (activityTitle a)

    formatEventTime :: Maybe TZ -> Activity -> Text
    formatEventTime mtz act = case activityStartsAt act of
      Nothing -> ""
      Just start ->
        let startStr = formatTimeShort mtz start
            endStr = maybe "" (\e -> "-" <> formatTimeShort mtz e) (activityEndsAt act)
        in startStr <> endStr <> " "

    formatTimeShort :: Maybe TZ -> UTCTime -> Text
    formatTimeShort Nothing utc = T.take 5 $ T.drop 11 $ T.pack $ show utc
    formatTimeShort (Just tz) utc = T.take 5 $ T.drop 11 $ T.pack $ show $ utcToLocalTimeTZ tz utc

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
  , agentDescription = "Calendar reasoning, schedule queries, finding free time"
  , agentTools =
      [ ToolInfo "query_calendar" Decision
      , ToolInfo "find_free_slots" Decision
      ]
  , agentWorkflows = ["schedule-query", "find-time"]
  , agentImplemented = True
  }
```

**Step 2: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Agents/Scheduler.hs
git commit -m "feat: implement scheduler agent with calendar tools"
```

---

## Task 3: Wire Up Scheduler in Dispatcher

**Files:**
- Modify: `wisp-srv/src/Agents/Dispatcher.hs`

**Step 1: Update dispatcher to call Scheduler.handleChat**

Replace the dispatcher file `wisp-srv/src/Agents/Dispatcher.hs`:

```haskell
module Agents.Dispatcher
  ( allAgents
  , getAgent
  , dispatchChat
  ) where

import Data.Text (Text)
import Domain.Agent (AgentInfo(..))
import Domain.Chat (ChatMessage, ChatResponse)
import App.Monad (App)
import qualified Agents.Concierge as Concierge
import qualified Agents.Scheduler as Scheduler
import qualified Agents.Housekeeper as Housekeeper
import qualified Agents.Insights as Insights

allAgents :: [AgentInfo]
allAgents =
  [ Concierge.agentInfo
  , Scheduler.agentInfo
  , Housekeeper.agentInfo
  , Insights.agentInfo
  ]

getAgent :: Text -> Maybe AgentInfo
getAgent aid = case [a | a <- allAgents, agentId a == aid] of
  [a] -> Just a
  _ -> Nothing

-- | Dispatch chat to the appropriate agent
-- timezone: Optional IANA timezone for converting dates to local time in agent context
dispatchChat :: Text -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
dispatchChat "wisp" msgs tz = Concierge.handleChat msgs tz
dispatchChat "wisp" msgs tz = Scheduler.handleChat msgs tz
dispatchChat "wisp/housekeeper" _ _ = pure $ Left "Agent 'wisp/housekeeper' not yet implemented"
dispatchChat "wisp" _ _ = pure $ Left "Agent 'wisp' not yet implemented"
dispatchChat agent _ _ = pure $ Left $ "Unknown agent: " <> agent
```

**Step 2: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Agents/Dispatcher.hs
git commit -m "feat: wire scheduler agent into dispatcher"
```

---

## Task 4: Implement Free Slots Algorithm

**Files:**
- Modify: `wisp-srv/src/Agents/Scheduler.hs`

**Step 1: Replace the findFreeSlots placeholder**

In `wisp-srv/src/Agents/Scheduler.hs`, replace the `findFreeSlots` function:

```haskell
-- Find free slots between events during work hours
findFreeSlots :: Maybe TZ -> UTCTime -> Int -> Int -> Int -> Int -> [Activity] -> [Value]
findFreeSlots mTz baseTime days startHour endHour durationMins events =
  let -- Generate work hour windows for each day
      dayStarts = [addUTCTime (fromIntegral d * nominalDay) baseTime | d <- [0..days-1]]
      workWindows = concatMap (dayWorkWindows startHour endHour) dayStarts
      -- Filter out windows that overlap with events
      freeWindows = filter (not . overlapsAnyEvent events) workWindows
      -- Only keep windows >= requested duration
      validSlots = filter (\(s, e) -> diffMins s e >= durationMins) freeWindows
  in map (slotToJson mTz) (take 10 validSlots)  -- Limit to 10 suggestions
  where
    -- Generate work hours for a day (simplified: assumes UTC, real would use timezone)
    dayWorkWindows :: Int -> Int -> UTCTime -> [(UTCTime, UTCTime)]
    dayWorkWindows sh eh dayStart =
      let startOfDay = dayStart  -- Simplified
          workStart = addUTCTime (fromIntegral (sh * 3600)) startOfDay
          workEnd = addUTCTime (fromIntegral (eh * 3600)) startOfDay
      in [(workStart, workEnd)]

    overlapsAnyEvent :: [Activity] -> (UTCTime, UTCTime) -> Bool
    overlapsAnyEvent evts (ws, we) = any (overlaps ws we) evts

    overlaps :: UTCTime -> UTCTime -> Activity -> Bool
    overlaps ws we evt = case (activityStartsAt evt, activityEndsAt evt) of
      (Just es, Just ee) -> not (we <= es || ws >= ee)  -- Overlap if NOT (completely before OR completely after)
      _ -> False

    diffMins :: UTCTime -> UTCTime -> Int
    diffMins s e = round (realToFrac (diffUTCTime e s) / 60 :: Double)

    slotToJson :: Maybe TZ -> (UTCTime, UTCTime) -> Value
    slotToJson mtz (s, e) = object
      [ "start" .= formatTime mtz s
      , "end" .= formatTime mtz e
      , "duration_minutes" .= diffMins s e
      ]
```

**Step 2: Add missing import**

Add to imports in `wisp-srv/src/Agents/Scheduler.hs`:

```haskell
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay, diffUTCTime)
```

**Step 3: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Agents/Scheduler.hs
git commit -m "feat: implement free slots algorithm for scheduler"
```

---

## Task 5: Add Scheduler Tests

**Files:**
- Create: `wisp-srv/test/Agents/SchedulerSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal` (test modules)

**Step 1: Create test file**

Create `wisp-srv/test/Agents/SchedulerSpec.hs`:

```haskell
module Agents.SchedulerSpec (spec) where

import Test.Hspec
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL

import Agents.Scheduler
import Domain.Agent (agentId, agentImplemented)

spec :: Spec
spec = do
  describe "Scheduler" $ do
    describe "agentInfo" $ do
      it "has correct agent ID" $ do
        agentId agentInfo `shouldBe` "wisp"

      it "is marked as implemented" $ do
        agentImplemented agentInfo `shouldBe` True

    describe "CalendarQuery parsing" $ do
      it "parses query_calendar with days" $ do
        let json = "{\"tool\": \"query_calendar\", \"days\": 14}"
        let result = decode json :: Maybe SchedulerToolCall
        case result of
          Just (QueryCalendar q) -> calendarDays q `shouldBe` Just 14
          _ -> expectationFailure "Expected QueryCalendar"

      it "parses query_calendar with date" $ do
        let json = "{\"tool\": \"query_calendar\", \"date\": \"2026-02-10\"}"
        let result = decode json :: Maybe SchedulerToolCall
        case result of
          Just (QueryCalendar q) -> calendarDate q `shouldBe` Just "2026-02-10"
          _ -> expectationFailure "Expected QueryCalendar"

    describe "FreeSlotQuery parsing" $ do
      it "parses find_free_slots with all parameters" $ do
        let json = "{\"tool\": \"find_free_slots\", \"days\": 5, \"duration_minutes\": 30, \"start_hour\": 10, \"end_hour\": 16}"
        let result = decode json :: Maybe SchedulerToolCall
        case result of
          Just (FindFreeSlots q) -> do
            slotDays q `shouldBe` Just 5
            slotDuration q `shouldBe` Just 30
            slotStartHour q `shouldBe` Just 10
            slotEndHour q `shouldBe` Just 16
          _ -> expectationFailure "Expected FindFreeSlots"

      it "parses find_free_slots with defaults" $ do
        let json = "{\"tool\": \"find_free_slots\"}"
        let result = decode json :: Maybe SchedulerToolCall
        case result of
          Just (FindFreeSlots q) -> do
            slotDays q `shouldBe` Nothing
            slotDuration q `shouldBe` Nothing
          _ -> expectationFailure "Expected FindFreeSlots"
```

**Step 2: Add to cabal test modules**

In `wisp-srv/wisp-srv.cabal`, add `Agents.SchedulerSpec` to the test-suite other-modules list (after `Agents.DispatcherSpec`).

**Step 3: Run tests**

Run: `cabal test wisp-srv-test`
Expected: All tests pass

**Step 4: Commit**

```bash
git add wisp-srv/test/Agents/SchedulerSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "test: add scheduler agent tests"
```

---

## Task 6: Update Documentation

**Files:**
- Modify: `docs/agents.md`
- Modify: `docs/tools.md`

**Step 1: Update agents.md**

In `docs/agents.md`, replace the scheduler section:

```markdown
### wisp [IMPLEMENTED]

Calendar reasoning, schedule queries, finding free time.

**Deterministic flows:** (none)

**Decision flows:**
- `schedule-query` - Query calendar events
- `find-time` - Find available time slots

**Tools:**
- `query_calendar` (decision) - Get calendar events for date range
- `find_free_slots` (decision) - Find available time slots
```

**Step 2: Update tools.md**

Add to `docs/tools.md` after query_people:

```markdown
### query_calendar (calendar_tools)

Type: decision

Used by scheduler agent to query calendar events.

**Input:**
```json
{
  "days": "Int | null (default 7)",
  "date": "Text | null (specific date ISO format)"
}
```

**Output:**
```json
{
  "events": [CalendarEvent],
  "count": "Int"
}
```

### find_free_slots (calendar_tools)

Type: decision

Used by scheduler agent to find available time.

**Input:**
```json
{
  "days": "Int | null (default 7)",
  "duration_minutes": "Int | null (default 60)",
  "start_hour": "Int | null (default 9)",
  "end_hour": "Int | null (default 17)"
}
```

**Output:**
```json
{
  "free_slots": [TimeSlot],
  "count": "Int",
  "parameters": {
    "days": "Int",
    "duration_minutes": "Int",
    "work_hours": "Text"
  }
}
```
```

**Step 3: Commit**

```bash
git add docs/agents.md docs/tools.md
git commit -m "docs: update scheduler agent documentation"
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

Start server and test via CLI:
```bash
# In one terminal
cabal run wisp-srv

# In another terminal
wisp chat -a wisp -m "What's on my calendar this week?"
wisp chat -a wisp -m "Find me 30 minutes for a call"
```

**Step 4: Final commit**

```bash
git add -A
git commit -m "feat: complete scheduler agent implementation"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Add calendar DB queries | `Infra/Db/Activity.hs` |
| 2 | Create Scheduler module | `Agents/Scheduler.hs` |
| 3 | Wire into Dispatcher | `Agents/Dispatcher.hs` |
| 4 | Implement free slots algorithm | `Agents/Scheduler.hs` |
| 5 | Add tests | `test/Agents/SchedulerSpec.hs` |
| 6 | Update docs | `docs/agents.md`, `docs/tools.md` |
| 7 | Verify build and tests | (verification) |

**Dependencies:**
- Tasks 1-4 must be sequential (each builds on previous)
- Task 5 can run after Task 4
- Task 6 can run after Task 3
- Task 7 runs last
