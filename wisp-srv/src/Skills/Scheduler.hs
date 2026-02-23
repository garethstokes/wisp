module Skills.Scheduler
  ( -- Decision flows
    handleChat
  , handleChatWithContext
    -- Types
  , SchedulerToolCall(..)
  , CalendarQuery(..)
  , FreeSlotQuery(..)
  , ToolResult(..)
    -- Dispatcher
  , executeToolCall
    -- Agent metadata
  , agentInfo
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), decode, encode, object, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay, diffUTCTime)
import qualified Data.Time.Format as TF
import Data.Time.Zones (TZ, loadSystemTZ, utcToLocalTimeTZ)
import App.Monad (App, Env(..))
import App.Config (Config(..), ClaudeConfig(..))
import Domain.Activity (Activity(..))
import Domain.Id (unEntityId)
import Infra.Db.Activity (getTodaysCalendarEvents, getUpcomingCalendarEvents, insertConversation)
import Infra.Db.Account (getAllAccounts)
import Infra.Db.Auth (getAllTokens, tokenNeedsRefresh, AuthToken(..))
import Domain.Account (Account(..), accountIdentifier)
import Infra.Claude.Client (callClaudeWithSystem)
import Agents.Run (RunContext(..), callClaudeLogged, logToolRequest, logToolSuccess, logToolFailure)
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
  | ListConnectedAccounts
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

-- | A tool call paired with its execution result (for feeding back to LLM)
data ExecutedTool = ExecutedTool
  { executedCall :: SchedulerToolCall
  , executedResult :: ToolResult
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- JSON Instances
--------------------------------------------------------------------------------

instance FromJSON SchedulerToolCall where
  parseJSON = withObject "SchedulerToolCall" $ \v -> do
    tool <- v .: "tool"
    case (tool :: Text) of
      "query_calendar" -> QueryCalendar <$> parseJSON (Object v)
      "find_free_slots" -> FindFreeSlots <$> parseJSON (Object v)
      "list_connected_accounts" -> pure ListConnectedAccounts
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

instance ToJSON SchedulerToolCall where
  toJSON (QueryCalendar q) = object
    [ "tool" .= ("query_calendar" :: Text)
    , "days" .= calendarDays q
    , "date" .= calendarDate q
    ]
  toJSON (FindFreeSlots q) = object
    [ "tool" .= ("find_free_slots" :: Text)
    , "days" .= slotDays q
    , "duration_minutes" .= slotDuration q
    , "start_hour" .= slotStartHour q
    , "end_hour" .= slotEndHour q
    ]
  toJSON ListConnectedAccounts = object
    [ "tool" .= ("list_connected_accounts" :: Text)
    ]

instance ToJSON CalendarQuery where
  toJSON CalendarQuery {..} = object
    [ "days" .= calendarDays
    , "date" .= calendarDate
    ]

instance ToJSON FreeSlotQuery where
  toJSON FreeSlotQuery {..} = object
    [ "days" .= slotDays
    , "duration_minutes" .= slotDuration
    , "start_hour" .= slotStartHour
    , "end_hour" .= slotEndHour
    ]

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

executeToolCall _ ListConnectedAccounts = do
  accounts <- getAllAccounts
  tokens <- getAllTokens "google"

  -- Build account info with connection status
  accountInfos <- liftIO $ mapM (buildAccountInfo tokens) accounts

  pure $ ToolSuccess $ object
    [ "accounts" .= accountInfos
    , "count" .= length accounts
    , "provider" .= ("google" :: Text)
    ]
  where
    buildAccountInfo :: [AuthToken] -> Account -> IO Value
    buildAccountInfo tokens acc = do
      let accId = accountId acc
      let mToken = find (\t -> tokenAccountId t == accId) tokens
      status <- case mToken of
        Nothing -> pure ("disconnected" :: Text)
        Just tok -> do
          needsRefresh <- tokenNeedsRefresh tok
          pure $ if needsRefresh then "token_expiring" else "connected"
      pure $ object
        [ "email" .= accountIdentifier acc
        , "display_name" .= accountDisplayName acc
        , "status" .= status
        , "scopes" .= maybe ([] :: [Text]) tokenScopes mToken
        ]

eventToJson :: Maybe TZ -> Activity -> Value
eventToJson mTz a = object
  [ "id" .= unEntityId (activityId a)
  , "title" .= activityTitle a
  , "starts_at" .= fmap (formatTime mTz) (activityStartsAt a)
  , "ends_at" .= fmap (formatTime mTz) (activityEndsAt a)
  ]

formatTime :: Maybe TZ -> UTCTime -> Text
formatTime Nothing utc = T.pack $ TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" utc
formatTime (Just tz) utc =
  let localTime = utcToLocalTimeTZ tz utc
  in T.pack $ TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" localTime

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

--------------------------------------------------------------------------------
-- Chat Handler
--------------------------------------------------------------------------------

data LLMResponse = LLMResponse
  { llmMessage :: Text
  , llmTools :: [SchedulerToolCall]
  } deriving (Show, Eq)

instance FromJSON LLMResponse where
  parseJSON = withObject "LLMResponse" $ \v -> LLMResponse
    <$> v .: "msg"
    <*> (fromMaybe [] <$> v .:? "tools")

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

      -- Run the agentic loop
      claudeCfg <- asks (claude . config)
      runAgentLoop claudeCfg systemPrompt mTz messages [] query

-- | Agentic loop: call LLM, execute tools, feed results back, repeat until done
runAgentLoop
  :: ClaudeConfig
  -> Text                    -- System prompt
  -> Maybe TZ                -- Timezone
  -> [ChatMessage]           -- Original conversation
  -> [ExecutedTool]          -- Accumulated tool results from this turn
  -> Text                    -- Original query (for logging)
  -> App (Either Text ChatResponse)
runAgentLoop claudeCfg systemPrompt mTz messages prevTools query = do
  let conversationPrompt = buildConversationWithTools messages prevTools

  result <- liftIO $ callClaudeWithSystem
    (apiKey claudeCfg)
    (model claudeCfg)
    systemPrompt
    conversationPrompt

  case result of
    Left err -> pure $ Left err
    Right (response, _, _) -> do
      -- Log conversation on final response only
      case parseLLMResponse response of
        Left err -> pure $ Left err
        Right llmResp -> do
          case llmTools llmResp of
            [] -> do
              -- No more tools - we're done, log and return
              _ <- insertConversation query response
              pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
            toolCalls -> do
              -- Execute all tools
              executedTools <- mapM (executeAndWrap mTz) toolCalls
              -- Check for any errors
              let errors = [err | ExecutedTool _ (ToolError err) <- executedTools]
              case errors of
                (err:_) -> pure $ Left err
                [] -> do
                  -- Continue the loop with tool results
                  runAgentLoop claudeCfg systemPrompt mTz messages executedTools query

-- | Execute a tool and wrap with its call for feedback
executeAndWrap :: Maybe TZ -> SchedulerToolCall -> App ExecutedTool
executeAndWrap mTz call = do
  result <- executeToolCall mTz call
  pure $ ExecutedTool call result

-- | Handle chat with run context for event logging
handleChatWithContext :: RunContext -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
handleChatWithContext ctx messages mTzName = do
  let userMessages = [m | m <- messages, messageRole m == "user"]
  case userMessages of
    [] -> pure $ Left "No user message provided"
    _ -> do
      mTz <- case mTzName of
        Nothing -> pure Nothing
        Just tzName -> liftIO $ loadTimezone tzName

      -- Get calendar context
      todayEvents <- getTodaysCalendarEvents
      upcomingEvents <- getUpcomingCalendarEvents 7

      let systemPrompt = buildChatPrompt mTz todayEvents upcomingEvents

      -- Run the agentic loop with logging
      runAgentLoopLogged ctx systemPrompt mTz messages []

-- | Agentic loop with RunContext logging
runAgentLoopLogged
  :: RunContext
  -> Text                    -- System prompt
  -> Maybe TZ                -- Timezone
  -> [ChatMessage]           -- Original conversation
  -> [ExecutedTool]          -- Accumulated tool results from this turn
  -> App (Either Text ChatResponse)
runAgentLoopLogged ctx systemPrompt mTz messages prevTools = do
  let conversationPrompt = buildConversationWithTools messages prevTools

  result <- callClaudeLogged ctx systemPrompt conversationPrompt

  case result of
    Left err -> pure $ Left err
    Right response -> do
      case parseLLMResponse response of
        Left err -> pure $ Left err
        Right llmResp -> do
          case llmTools llmResp of
            [] -> do
              -- No more tools - we're done
              pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
            toolCalls -> do
              -- Execute all tools with logging
              executedTools <- mapM (executeAndWrapLogged ctx mTz) toolCalls
              -- Check for any errors
              let errors = [err | ExecutedTool _ (ToolError err) <- executedTools]
              case errors of
                (err:_) -> pure $ Left err
                [] -> do
                  -- Continue the loop with tool results
                  runAgentLoopLogged ctx systemPrompt mTz messages executedTools

-- | Execute a tool with logging and wrap with its call for feedback
executeAndWrapLogged :: RunContext -> Maybe TZ -> SchedulerToolCall -> App ExecutedTool
executeAndWrapLogged ctx mTz call = do
  let toolName = case call of
        QueryCalendar {} -> "query_calendar"
        FindFreeSlots {} -> "find_free_slots"
        ListConnectedAccounts -> "list_connected_accounts"

  _ <- logToolRequest ctx toolName (toJSON call)
  result <- executeToolCall mTz call

  case result of
    ToolSuccess val -> logToolSuccess ctx toolName val
    ToolError err -> logToolFailure ctx toolName err

  pure $ ExecutedTool call result

buildConversationPrompt :: [ChatMessage] -> Text
buildConversationPrompt msgs = T.unlines
  [ role <> ": " <> messageContent m
  | m <- msgs
  , let role = case messageRole m of
          "user" -> "User"
          "assistant" -> fromMaybe "Assistant" (messageAgent m)  -- Use agent name if available
          "tool" -> "Tool Result"
          r -> r
  ]

-- | Build conversation including tool results from current turn
buildConversationWithTools :: [ChatMessage] -> [ExecutedTool] -> Text
buildConversationWithTools msgs executedTools =
  let baseConvo = buildConversationPrompt msgs
      toolSection = case executedTools of
        [] -> ""
        tools -> T.unlines $
          [ ""
          , "## Tool Results"
          , "You called these tools. Here are the results:"
          , ""
          ] ++ map formatExecutedTool tools
  in baseConvo <> toolSection

formatExecutedTool :: ExecutedTool -> Text
formatExecutedTool (ExecutedTool call result) =
  let toolName = case call of
        QueryCalendar {} -> "query_calendar"
        FindFreeSlots {} -> "find_free_slots"
        ListConnectedAccounts -> "list_connected_accounts"
      resultJson = case result of
        ToolSuccess val -> decodeUtf8 $ BL.toStrict $ encode val
        ToolError err -> "{\"error\": \"" <> err <> "\"}"
  in "### " <> toolName <> "\n" <> resultJson

buildChatPrompt :: Maybe TZ -> [Activity] -> [Activity] -> Text
buildChatPrompt mTz todayEvents upcomingEvents = T.unlines
  [ "You are Wisp's scheduler agent. You help with calendar questions and finding time."
  , ""
  , "## Response Format"
  , "IMPORTANT: Respond with ONLY valid JSON, no other text."
  , ""
  , "Response without tools:"
  , "{\"msg\": \"Your response to the user\"}"
  , ""
  , "Response with tools (you can call multiple):"
  , "{\"msg\": \"What you're doing\", \"tools\": [{\"tool\": \"...\", ...}, ...]}"
  , ""
  , "After tool results are returned, you'll see them and can respond or call more tools."
  , "When done, respond with just msg (no tools array)."
  , ""
  , "## Available Tools"
  , ""
  , "### query_calendar - Get calendar events"
  , "{\"tool\": \"query_calendar\", \"days\": 7}"
  , "{\"tool\": \"query_calendar\", \"date\": \"2026-02-05\"}"
  , ""
  , "### find_free_slots - Find available time"
  , "{\"tool\": \"find_free_slots\", \"days\": 7, \"duration_minutes\": 60, \"start_hour\": 9, \"end_hour\": 17}"
  , ""
  , "### list_connected_accounts - Show which accounts provide calendar data"
  , "{\"tool\": \"list_connected_accounts\"}"
  , "Returns email, connection status, and granted scopes for each account."
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
  { agentId = "scheduler"
  , agentDescription = "Calendar reasoning, schedule queries, finding free time"
  , agentTools =
      [ ToolInfo "query_calendar" Decision
      , ToolInfo "find_free_slots" Decision
      , ToolInfo "list_connected_accounts" Decision
      ]
  , agentWorkflows = ["schedule-query", "find-time"]
  , agentImplemented = True
  }
