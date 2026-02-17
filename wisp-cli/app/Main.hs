-- wisp-cli/app/Main.hs
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), decode, encode, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.List (isSuffixOf)
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Network.HTTP.Client
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, listDirectory, removeFile)
import System.FilePath ((</>))
import System.Process (callCommand)

import Config (CliConfig (..), loadCliConfig)
import Time (TZ, loadTimezone, utcToLocal)

-- CLI Command types
data Command
  = Auth
  | Status
  | Poll
  | Classify
  | Inbox
  | Review
  | Approve Text
  | Dismiss Text
  | People
  | Activity Text
  | Logs Text
  | Chat ChatOptions
  | Agents
  | Sessions SessionsOptions
  | Runs
  | Run Text
  | Help
  deriving (Show)

data ChatOptions = ChatOptions
  { chatOptAgent :: Text
  , chatOptMessage :: Maybe Text
  , chatOptSession :: Text
  , chatOptNew :: Bool
  }
  deriving (Show)

data SessionsOptions = SessionsOptions
  { sessionsDelete :: Maybe Text
  }
  deriving (Show)

-- Parse commands
commandParser :: Parser Command
commandParser =
  subparser
    ( command "status" (info (pure Status) (progDesc "Server status and activity counts"))
        <> command "inbox" (info (pure Inbox) (progDesc "Activities needing attention (quarantined, surfaced)"))
        <> command "review" (info (pure Review) (progDesc "Activities needing review (tier 3 - uncertain classification)"))
        <> command "activity" (info activityParser (progDesc "Show full details for an activity"))
        <> command "logs" (info logsParser (progDesc "Show processing history for an activity"))
        <> command "approve" (info approveParser (progDesc "Move quarantined activity to surfaced"))
        <> command "dismiss" (info dismissParser (progDesc "Archive an activity"))
        <> command "chat" (info chatOptionsParser (progDesc "Chat with an agent"))
        <> command "agents" (info (pure Agents) (progDesc "List available agents"))
        <> command "sessions" (info sessionsParser (progDesc "Manage chat sessions"))
        <> command "people" (info (pure People) (progDesc "List contacts extracted from activities"))
        <> command "poll" (info (pure Poll) (progDesc "Fetch new emails and calendar events now"))
        <> command "classify" (info (pure Classify) (progDesc "Run classification on pending activities"))
        <> command "auth" (info (pure Auth) (progDesc "Add a Google account via OAuth"))
        <> command "runs" (info (pure Runs) (progDesc "List recent agent runs"))
        <> command "run" (info runParser (progDesc "Show full details for an agent run"))
        <> command "help" (info (pure Help) (progDesc "Show this help"))
    )

logsParser :: Parser Command
logsParser = Logs <$> strArgument (metavar "ID" <> help "Activity ID to show logs for")

chatOptionsParser :: Parser Command
chatOptionsParser =
  Chat
    <$> ( ChatOptions
            <$> strOption (long "agent" <> short 'a' <> metavar "AGENT" <> help "Agent path (e.g., wisp/concierge)")
            <*> optional (strOption (long "message" <> short 'm' <> metavar "MSG" <> help "Message to send"))
            <*> strOption (long "session" <> short 's' <> metavar "NAME" <> value "default" <> help "Session name")
            <*> switch (long "new" <> help "Start fresh session")
        )

sessionsParser :: Parser Command
sessionsParser =
  Sessions
    <$> ( SessionsOptions
            <$> optional (strOption (long "delete" <> short 'd' <> metavar "NAME" <> help "Delete session"))
        )

activityParser :: Parser Command
activityParser = Activity <$> strArgument (metavar "ID" <> help "Activity ID to show")

approveParser :: Parser Command
approveParser = Approve <$> strArgument (metavar "ID" <> help "Activity ID to approve")

dismissParser :: Parser Command
dismissParser = Dismiss <$> strArgument (metavar "ID" <> help "Activity ID to dismiss")

runParser :: Parser Command
runParser = Run <$> strArgument (metavar "ID" <> help "Run ID to show")

-- Parser that defaults to Help when no command given
commandParserWithDefault :: Parser Command
commandParserWithDefault = commandParser <|> pure Help

opts :: ParserInfo Command
opts =
  info
    (commandParserWithDefault <**> helper)
    ( fullDesc
        <> progDesc "Wisp personal assistant CLI"
        <> header "wisp - your autonomy-preserving assistant"
    )

-- Base URL for wisp-srv
baseUrl :: String
baseUrl = "http://127.0.0.1:5812"

--------------------------------------------------------------------------------
-- Session Management Types
--------------------------------------------------------------------------------

data Session = Session
  { sessionCreatedAt :: Text
  , sessionUpdatedAt :: Text
  , sessionMessages :: [SessionMessage]
  }
  deriving (Show)

data SessionMessage = SessionMessage
  { smRole :: Text
  , smContent :: Text
  , smAgent :: Maybe Text  -- Which agent responded (for assistant messages)
  , smToolCall :: Maybe Value
  }
  deriving (Show)

instance FromJSON Session where
  parseJSON = withObject "Session" $ \v ->
    Session
      <$> v .: "created_at"
      <*> v .: "updated_at"
      <*> v .: "messages"

instance ToJSON Session where
  toJSON s =
    object
      [ "created_at" .= sessionCreatedAt s
      , "updated_at" .= sessionUpdatedAt s
      , "messages" .= sessionMessages s
      ]

instance FromJSON SessionMessage where
  parseJSON = withObject "SessionMessage" $ \v ->
    SessionMessage
      <$> v .: "role"
      <*> v .: "content"
      <*> v .:? "agent"
      <*> v .:? "tool_call"

instance ToJSON SessionMessage where
  toJSON m =
    object $
      [ "role" .= smRole m
      , "content" .= smContent m
      ]
        ++ maybe [] (\a -> ["agent" .= a]) (smAgent m)
        ++ maybe [] (\tc -> ["tool_call" .= tc]) (smToolCall m)

getSessionsDir :: IO FilePath
getSessionsDir = do
  home <- getHomeDirectory
  let dir = home </> ".wisp" </> "sessions"
  createDirectoryIfMissing True dir
  pure dir

getSessionPath :: Text -> IO FilePath
getSessionPath name = do
  dir <- getSessionsDir
  pure $ dir </> unpack name <> ".json"

loadSession :: Text -> IO (Maybe Session)
loadSession name = do
  path <- getSessionPath name
  exists <- doesFileExist path
  if exists
    then decode <$> BL.readFile path
    else pure Nothing

saveSession :: Text -> Session -> IO ()
saveSession name session = do
  path <- getSessionPath name
  BL.writeFile path (encode session)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  cmd <- execParser opts
  -- Load config and timezone for commands that need it
  cfg <- loadCliConfig
  tz <- loadTimezone (timezone cfg)
  case cmd of
    Auth -> runAuth
    Status -> runStatus
    Poll -> runPoll
    Classify -> runClassify
    Inbox -> runInbox tz
    Review -> runReview tz
    Approve aid -> runApprove aid
    Dismiss aid -> runDismiss aid
    People -> runPeople
    Activity aid -> runActivity tz aid
    Logs aid -> runLogs aid
    Chat chatOpts -> runChatWithOptions cfg chatOpts
    Agents -> runAgents
    Sessions sessOpts -> runSessions sessOpts
    Runs -> runRuns tz
    Run rid -> runRun tz rid
    Help -> runHelp

runHelp :: IO ()
runHelp = do
  TIO.putStrLn "wisp - your autonomy-preserving assistant"
  TIO.putStrLn ""
  TIO.putStrLn "Commands:"
  TIO.putStrLn "  status              Server status and activity counts"
  TIO.putStrLn "  inbox               Activities needing attention"
  TIO.putStrLn "  review              Activities needing review (tier 3)"
  TIO.putStrLn "  activity ID         Show full details for an activity"
  TIO.putStrLn "  logs ID             Show processing history for an activity"
  TIO.putStrLn "  approve ID          Move quarantined activity to surfaced"
  TIO.putStrLn "  dismiss ID          Archive an activity"
  TIO.putStrLn "  chat                Chat with an agent"
  TIO.putStrLn "    -a, --agent       Agent path (e.g., wisp/concierge)"
  TIO.putStrLn "    -m, --message     Message to send"
  TIO.putStrLn "    -s, --session     Session name (default: \"default\")"
  TIO.putStrLn "    --new             Start fresh session"
  TIO.putStrLn "  agents              List available agents"
  TIO.putStrLn "  sessions            List chat sessions"
  TIO.putStrLn "    -d, --delete      Delete a session"
  TIO.putStrLn "  people              List contacts from activities"
  TIO.putStrLn "  poll                Fetch new emails and events now"
  TIO.putStrLn "  classify            Run classification on pending activities"
  TIO.putStrLn "  auth                Add a Google account via OAuth"
  TIO.putStrLn "  runs                List recent agent runs"
  TIO.putStrLn "  run ID              Show full details for an agent run"
  TIO.putStrLn ""
  TIO.putStrLn "Examples:"
  TIO.putStrLn "  wisp chat -a wisp/concierge -m \"Show quarantined items\""
  TIO.putStrLn "  wisp chat -a wisp/concierge -m \"Approve them\" -s work"
  TIO.putStrLn "  wisp agents"
  TIO.putStrLn "  wisp sessions"
  TIO.putStrLn ""
  TIO.putStrLn "Activity IDs are shown in brackets, e.g. [abc123]"

runInbox :: TZ -> IO ()
runInbox tz = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/inbox"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> do
      -- Show today's calendar first
      case KM.lookup "calendar" obj of
        Just (Array items) | not (null items) -> do
          TIO.putStrLn "📅 Today's Schedule:"
          showCalendarWithGaps tz (toList items)
          TIO.putStrLn ""
        _ -> return ()
      -- Show quarantined items (need decision)
      case KM.lookup "quarantined" obj of
        Just (Array items) | not (null items) -> do
          TIO.putStrLn "⚠️  Quarantined (needs review):"
          mapM_ (showActivityBrief tz "  ") (toList items)
          TIO.putStrLn ""
        _ -> return ()
      -- Show surfaced items (ready to act)
      case KM.lookup "surfaced" obj of
        Just (Array items) | not (null items) -> do
          TIO.putStrLn "📋 Ready for action:"
          mapM_ (showActivityBrief tz "  ") (toList items)
          TIO.putStrLn ""
        _ -> return ()
      -- Show high urgency pending
      case KM.lookup "high_urgency" obj of
        Just (Array items) | not (null items) -> do
          TIO.putStrLn "🔥 High urgency (pending classification):"
          mapM_ (showActivityBrief tz "  ") (toList items)
          TIO.putStrLn ""
        _ -> return ()
      -- Show summary if nothing to show
      let hasQuarantined = case KM.lookup "quarantined" obj of
            Just (Array items) -> not (null items)
            _ -> False
      let hasSurfaced = case KM.lookup "surfaced" obj of
            Just (Array items) -> not (null items)
            _ -> False
      let hasCalendar = case KM.lookup "calendar" obj of
            Just (Array items) -> not (null items)
            _ -> False
      when (not hasQuarantined && not hasSurfaced && not hasCalendar) $
        TIO.putStrLn "No activities requiring attention."
    _ -> TIO.putStrLn "Failed to fetch inbox"

-- Show calendar events with gaps highlighted
showCalendarWithGaps :: TZ -> [Value] -> IO ()
showCalendarWithGaps _ [] = TIO.putStrLn "  No events today."
showCalendarWithGaps tz events = do
  -- Sort events by start time and show with gaps
  let sorted = sortByStartTime events
  showEventsWithGaps Nothing sorted
 where
  sortByStartTime = id -- Events already sorted by server
  showEventsWithGaps :: Maybe Text -> [Value] -> IO ()
  showEventsWithGaps _ [] = return ()
  showEventsWithGaps lastEnd (e : es) = do
    let mStart = getStartTime e
    let mEnd = getEndTime e
    -- Show gap if there's time between events
    case (lastEnd, mStart) of
      (Just end, Just start) | end < start -> do
        let gap = "  ⬜ " <> formatTimeRange end start <> " (free)"
        TIO.putStrLn gap
      _ -> return ()
    -- Show the event
    showCalendarEvent tz e
    showEventsWithGaps mEnd es

  getStartTime (Object o) = case KM.lookup "starts_at" o of
    Just (String s) -> Just s
    _ -> Nothing
  getStartTime _ = Nothing

  getEndTime (Object o) = case KM.lookup "ends_at" o of
    Just (String s) -> Just s
    _ -> Nothing
  getEndTime _ = Nothing

  formatTimeRange start end =
    formatTimeLocal tz start <> " - " <> formatTimeLocal tz end

-- Show a calendar event
showCalendarEvent :: TZ -> Value -> IO ()
showCalendarEvent tz (Object e) = do
  let title = case KM.lookup "title" e of
        Just (String s) -> s
        _ -> "(no title)"
  let timeStr = case KM.lookup "starts_at" e of
        Just (String s) -> formatTimeLocal tz s
        _ -> "??:??"
  let endStr = case KM.lookup "ends_at" e of
        Just (String s) -> " - " <> formatTimeLocal tz s
        _ -> ""
  TIO.putStrLn $ "  📅 " <> timeStr <> endStr <> " " <> title
showCalendarEvent _ _ = return ()

runReview :: TZ -> IO ()
runReview tz = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/review"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> do
      case KM.lookup "activities" obj of
        Just (Array items) | not (null items) -> do
          let total = case KM.lookup "total" obj of
                Just (Number n) -> round n :: Int
                _ -> length (toList items)
          TIO.putStrLn $ "🔍 Needs Review (" <> showT (length (toList items)) <> " of " <> showT total <> "):"
          TIO.putStrLn ""
          mapM_ (showActivityBrief tz "  ") (toList items)
          TIO.putStrLn ""
          TIO.putStrLn "Use 'wisp activity ID' to see details, 'wisp approve ID' or 'wisp dismiss ID' to act"
        _ -> TIO.putStrLn "No activities needing review."
    _ -> TIO.putStrLn "Failed to fetch review queue"

-- Show a brief activity line
showActivityBrief :: TZ -> Text -> Value -> IO ()
showActivityBrief tz prefix (Object act) = do
  let getId = case KM.lookup "id" act of
        Just (String s) -> s
        _ -> "?"
  let getTitle = case KM.lookup "title" act of
        Just (String s) -> s
        _ -> "(no title)"
  let getUrgency = case KM.lookup "urgency" act of
        Just (String "high") -> "🔴"
        Just (String "normal") -> "🟡"
        Just (String "low") -> "🟢"
        _ -> "⚪"
  let getSource = case KM.lookup "source" act of
        Just (String "email") -> "📧"
        Just (String "calendar") -> "📅"
        _ -> "📝"
  -- Get date from starts_at (for calendar) or created_at (for email)
  let getDate = case KM.lookup "starts_at" act of
        Just (String s) -> formatDateLocal tz s
        _ -> case KM.lookup "created_at" act of
          Just (String s) -> formatDateLocal tz s
          _ -> ""
  let datePart = if getDate == "" then "" else getDate <> " "
  TIO.putStrLn $ prefix <> getSource <> " " <> getUrgency <> " " <> datePart <> "[" <> getId <> "] " <> getTitle
showActivityBrief _ _ _ = return ()

-- Parse ISO8601 UTC time and convert to local timezone, returning just HH:MM
formatTimeLocal :: TZ -> Text -> Text
formatTimeLocal tz isoDate =
  case parseUtcTime isoDate of
    Nothing -> T.take 5 $ T.drop 11 $ T.takeWhile (/= '.') isoDate -- fallback to raw extraction
    Just utc ->
      let local = utcToLocal tz utc
          timeStr = T.pack $ show local -- "2026-02-04 14:30:00"
       in T.take 5 $ T.drop 11 timeStr -- "14:30"

-- Parse ISO8601 UTC time and convert to local timezone, returning HH:MM:SS
formatTimeWithSeconds :: TZ -> Text -> Text
formatTimeWithSeconds tz isoDate =
  case parseUtcTime isoDate of
    Nothing -> T.take 8 $ T.drop 11 $ T.takeWhile (/= '.') isoDate -- fallback to raw extraction
    Just utc ->
      let local = utcToLocal tz utc
          timeStr = T.pack $ show local -- "2026-02-04 14:30:00"
       in T.take 8 $ T.drop 11 timeStr -- "14:30:00"

-- Parse ISO8601 UTC time and convert to local, returning "Mon D HH:MM" or "Mon D"
formatDateLocal :: TZ -> Text -> Text
formatDateLocal tz isoDate =
  case parseUtcTime isoDate of
    Nothing -> formatDateFallback isoDate -- fallback to raw parsing
    Just utc ->
      let local = utcToLocal tz utc
          localStr = T.pack $ show local -- "2026-02-04 14:30:00"
          datePart = T.take 10 localStr -- "2026-02-04"
          timePart = T.drop 11 localStr -- "14:30:00"
          month = case T.take 2 (T.drop 5 datePart) of
            "01" -> "Jan"
            "02" -> "Feb"
            "03" -> "Mar"
            "04" -> "Apr"
            "05" -> "May"
            "06" -> "Jun"
            "07" -> "Jul"
            "08" -> "Aug"
            "09" -> "Sep"
            "10" -> "Oct"
            "11" -> "Nov"
            "12" -> "Dec"
            _ -> "???"
          day = T.dropWhile (== '0') $ T.drop 8 datePart
          time = T.take 5 timePart
       in if T.null timePart || time == "00:00"
            then month <> " " <> day
            else month <> " " <> day <> " " <> time

-- Fallback date formatting when parsing fails (uses raw ISO string)
formatDateFallback :: Text -> Text
formatDateFallback isoDate =
  let dateStr = T.takeWhile (/= '.') isoDate
      datePart = T.take 10 dateStr
      timePart = T.drop 11 dateStr
      month = case T.take 2 (T.drop 5 datePart) of
        "01" -> "Jan"
        "02" -> "Feb"
        "03" -> "Mar"
        "04" -> "Apr"
        "05" -> "May"
        "06" -> "Jun"
        "07" -> "Jul"
        "08" -> "Aug"
        "09" -> "Sep"
        "10" -> "Oct"
        "11" -> "Nov"
        "12" -> "Dec"
        _ -> "???"
      day = T.dropWhile (== '0') $ T.drop 8 datePart
      time = T.take 5 timePart
   in if T.null timePart || time == "00:00"
        then month <> " " <> day
        else month <> " " <> day <> " " <> time

-- Parse ISO8601 UTC time string to UTCTime
parseUtcTime :: Text -> Maybe UTCTime
parseUtcTime t = iso8601ParseM (T.unpack t)

runApprove :: Text -> IO ()
runApprove aid = do
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/activities/" <> unpack aid <> "/approve"
  let req = initialReq{method = "POST"}
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "status" obj of
      Just (String "approved") -> TIO.putStrLn $ "✅ Activity " <> aid <> " approved (moved to surfaced)"
      _ -> case KM.lookup "error" obj of
        Just (String err) -> TIO.putStrLn $ "❌ Error: " <> err
        _ -> TIO.putStrLn "Approve request sent"
    _ -> TIO.putStrLn "❌ Failed to approve activity"

runDismiss :: Text -> IO ()
runDismiss aid = do
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/activities/" <> unpack aid <> "/dismiss"
  let req = initialReq{method = "POST"}
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "status" obj of
      Just (String "dismissed") -> TIO.putStrLn $ "🗑️  Activity " <> aid <> " dismissed (archived)"
      _ -> case KM.lookup "error" obj of
        Just (String err) -> TIO.putStrLn $ "❌ Error: " <> err
        _ -> TIO.putStrLn "Dismiss request sent"
    _ -> TIO.putStrLn "❌ Failed to dismiss activity"

runPeople :: IO ()
runPeople = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/people"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> do
      case KM.lookup "people" obj of
        Just (Array people) | not (null people) -> do
          TIO.putStrLn "Known People:"
          TIO.putStrLn "============="
          mapM_ showPersonBrief (toList people)
        _ -> TIO.putStrLn "No people found"
      case KM.lookup "count" obj of
        Just (Number n) -> TIO.putStrLn $ "\nTotal: " <> showT (round n :: Int) <> " people"
        _ -> return ()
    _ -> TIO.putStrLn "❌ Failed to fetch people"

-- Show a brief person line
showPersonBrief :: Value -> IO ()
showPersonBrief (Object p) = do
  let getEmail = case KM.lookup "email" p of
        Just (String s) -> s
        _ -> "(no email)"
  let getName = case KM.lookup "display_name" p of
        Just (String s) -> s
        Just Null -> ""
        _ -> ""
  let getCount = case KM.lookup "contact_count" p of
        Just (Number n) -> round n :: Int
        _ -> 0
  let displayName = if getName == "" then "" else " (" <> getName <> ")"
  TIO.putStrLn $ "  📧 " <> getEmail <> displayName <> " - " <> showT getCount <> " contacts"
showPersonBrief _ = return ()

runActivity :: TZ -> Text -> IO ()
runActivity tz aid = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/activities/" <> unpack aid
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object act) -> do
      TIO.putStrLn "Activity Details"
      TIO.putStrLn "================"
      showField "ID" "id" act
      showField "Title" "title" act
      showField "Status" "status" act
      showField "Source" "source" act
      showField "Sender" "sender_email" act
      showField "Summary" "summary" act
      TIO.putStrLn ""
      TIO.putStrLn "Classification:"
      showField "  Type" "activity_type" act
      showField "  Urgency" "urgency" act
      showField "  Autonomy Tier" "autonomy_tier" act
      showField "  Confidence" "confidence" act
      showArrayField "  Personas" "personas" act
      TIO.putStrLn ""
      showDateField tz "Created" "created_at" act
      showDateField tz "Starts" "starts_at" act
      showDateField tz "Ends" "ends_at" act
    _ -> TIO.putStrLn "❌ Failed to fetch activity (not found or error)"

-- Show a field from a JSON object
showField :: Text -> Text -> KM.KeyMap Value -> IO ()
showField label key obj = case KM.lookup (fromString $ unpack key) obj of
  Just (String s) -> TIO.putStrLn $ label <> ": " <> s
  Just (Number n) -> TIO.putStrLn $ label <> ": " <> showT n
  Just Null -> return ()
  Just v -> TIO.putStrLn $ label <> ": " <> pack (show v)
  Nothing -> return ()

-- Show a date field, converting from UTC to local timezone
showDateField :: TZ -> Text -> Text -> KM.KeyMap Value -> IO ()
showDateField tz label key obj = case KM.lookup (fromString $ unpack key) obj of
  Just (String s) -> TIO.putStrLn $ label <> ": " <> formatDateTimeLocal tz s
  Just Null -> return ()
  _ -> return ()

-- Format full datetime in local timezone (for activity details view)
formatDateTimeLocal :: TZ -> Text -> Text
formatDateTimeLocal tz isoDate =
  case parseUtcTime isoDate of
    Nothing -> isoDate -- fallback to raw string
    Just utc ->
      let local = utcToLocal tz utc
       in T.pack $ show local -- "2026-02-04 14:30:00"

-- Show an array field
showArrayField :: Text -> Text -> KM.KeyMap Value -> IO ()
showArrayField label key obj = case KM.lookup (fromString $ unpack key) obj of
  Just (Array arr) | not (null arr) -> do
    let items = [s | String s <- toList arr]
    TIO.putStrLn $ label <> ": " <> pack (show items)
  _ -> return ()

runLogs :: Text -> IO ()
runLogs aid = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/activities/" <> unpack aid <> "/logs"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> do
      TIO.putStrLn $ "Processing Logs for Activity " <> aid
      TIO.putStrLn "======================================="
      case KM.lookup "logs" obj of
        Just (Array logs) | not (null logs) -> mapM_ showLogEntry (toList logs)
        _ -> TIO.putStrLn "No logs found"
      case KM.lookup "count" obj of
        Just (Number n) -> TIO.putStrLn $ "\nTotal: " <> showT (round n :: Int) <> " log entries"
        _ -> return ()
    _ -> TIO.putStrLn "❌ Failed to fetch logs"

-- Show a single log entry
showLogEntry :: Value -> IO ()
showLogEntry (Object log') = do
  let getAction = case KM.lookup "action_taken" log' of
        Just (String s) -> s
        _ -> "unknown"
  let getDetail = case KM.lookup "action_detail" log' of
        Just (String s) -> Just s
        Just Null -> Nothing
        _ -> Nothing
  let getConfidence = case KM.lookup "confidence" log' of
        Just (Number n) -> Just n
        _ -> Nothing
  let getTime = case KM.lookup "created_at" log' of
        Just (String s) -> s
        _ -> ""
  TIO.putStrLn $ "  " <> getTime <> " - " <> getAction
  case getDetail of
    Just detail -> TIO.putStrLn $ "    Detail: " <> detail
    Nothing -> return ()
  case getConfidence of
    Just conf -> TIO.putStrLn $ "    Confidence: " <> showT conf
    Nothing -> return ()
showLogEntry _ = return ()

runAgents :: IO ()
runAgents = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/agents"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "agents" obj of
      Just (Array agents) -> do
        TIO.putStrLn "Available Agents"
        TIO.putStrLn "================"
        TIO.putStrLn ""
        mapM_ showAgent (toList agents)
      _ -> TIO.putStrLn "No agents found"
    _ -> TIO.putStrLn "Failed to fetch agents"

showAgent :: Value -> IO ()
showAgent (Object a) = do
  let getId = case KM.lookup "id" a of
        Just (String s) -> s
        _ -> "?"
  let getDesc = case KM.lookup "description" a of
        Just (String s) -> s
        _ -> ""
  let getImpl = case KM.lookup "implemented" a of
        Just (Bool True) -> ""
        _ -> " [not implemented]"
  let getTools = case KM.lookup "tools" a of
        Just (Array ts) -> [n | Object t <- toList ts, Just (String n) <- [KM.lookup "name" t]]
        _ -> []
  let getWorkflows = case KM.lookup "workflows" a of
        Just (Array ws) -> [w | String w <- toList ws]
        _ -> []
  TIO.putStrLn $ getId <> getImpl
  TIO.putStrLn $ "  " <> getDesc
  TIO.putStrLn $ "  Tools: " <> T.intercalate ", " getTools
  TIO.putStrLn $ "  Workflows: " <> T.intercalate ", " getWorkflows
  TIO.putStrLn ""
showAgent _ = return ()

runChatWithOptions :: CliConfig -> ChatOptions -> IO ()
runChatWithOptions cfg chatOpts = do
  let agent = chatOptAgent chatOpts
  let sessionName = chatOptSession chatOpts
  let isNew = chatOptNew chatOpts

  case chatOptMessage chatOpts of
    Nothing -> TIO.putStrLn "Please provide a message with --message"
    Just msg -> do
      -- Load or create session
      existingSession <- if isNew then pure Nothing else loadSession sessionName

      -- Build messages list
      now <- pack . iso8601Show <$> getCurrentTime
      let existingMessages = maybe [] sessionMessages existingSession
      let newUserMsg = SessionMessage "user" msg Nothing Nothing  -- user messages have no agent
      let allMessages = existingMessages ++ [newUserMsg]

      -- Send to server with timezone and agent info per message
      manager <- newManager defaultManagerSettings
      initialReq <- parseRequest $ baseUrl <> "/chat"
      let reqBody =
            object
              [ "agent" .= agent
              , "messages" .= [messageToJson m | m <- allMessages]
              , "timezone" .= timezone cfg -- Include user's timezone
              ]
      let req =
            initialReq
              { method = "POST"
              , requestHeaders = [("Content-Type", "application/json")]
              , requestBody = RequestBodyLBS (encode reqBody)
              }
      response <- httpLbs req manager

      case decode (responseBody response) of
        Just (Object obj) -> case KM.lookup "message" obj of
          Just (String respMsg) -> do
            TIO.putStrLn respMsg

            -- Save session with response (tag assistant message with agent)
            let toolCall = KM.lookup "tool_call" obj
            let assistantMsg = SessionMessage "assistant" respMsg (Just agent) toolCall
            let updatedMessages = allMessages ++ [assistantMsg]
            let session =
                  Session
                    { sessionCreatedAt = maybe now sessionCreatedAt existingSession
                    , sessionUpdatedAt = now
                    , sessionMessages = updatedMessages
                    }
            saveSession sessionName session
          _ -> case KM.lookup "error" obj of
            Just (String err) -> TIO.putStrLn $ "Error: " <> err
            _ -> TIO.putStrLn "Unexpected response"
        _ -> TIO.putStrLn "Failed to parse response"

-- Convert a session message to JSON for the API request
messageToJson :: SessionMessage -> Value
messageToJson m =
  object $
    [ "role" .= smRole m
    , "content" .= smContent m
    ]
      ++ maybe [] (\a -> ["agent" .= a]) (smAgent m)

runSessions :: SessionsOptions -> IO ()
runSessions sessOpts = case sessionsDelete sessOpts of
  Just name -> do
    path <- getSessionPath name
    exists <- doesFileExist path
    if exists
      then do
        removeFile path
        TIO.putStrLn $ "Deleted session: " <> name
      else TIO.putStrLn $ "Session not found: " <> name
  Nothing -> do
    dir <- getSessionsDir
    files <- listDirectory dir
    let sessions = [pack (takeWhile (/= '.') f) | f <- files, ".json" `isSuffixOf` f]
    if null sessions
      then TIO.putStrLn "No sessions found."
      else do
        TIO.putStrLn "Sessions:"
        forM_ sessions $ \name -> do
          mSession <- loadSession name
          case mSession of
            Just s -> TIO.putStrLn $ "  " <> name <> " (" <> showT (length (sessionMessages s)) <> " messages)"
            Nothing -> TIO.putStrLn $ "  " <> name <> " (corrupt)"

runClassify :: IO ()
runClassify = do
  TIO.putStrLn "Running classification pipeline..."
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/pipeline/run"
  let req = initialReq{method = "POST"}
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> do
      let getNum key = case KM.lookup key obj of
            Just (Number n) -> round n :: Int
            _ -> 0
      let processed = getNum "processed"
      let failed = getNum "failed"
      let total = getNum "total"
      TIO.putStrLn $ "Processed: " <> showT processed <> " activities"
      TIO.putStrLn $ "Failed:    " <> showT failed <> " activities"
      TIO.putStrLn $ "Total:     " <> showT total <> " activities"
    _ -> TIO.putStrLn "Classification request sent"

runPoll :: IO ()
runPoll = do
  TIO.putStrLn "Triggering poll..."
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/poll"
  let req = initialReq{method = "POST"}
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "status" obj of
      Just (String s) -> TIO.putStrLn $ "Result: " <> s
      _ -> TIO.putStrLn "Poll triggered"
    _ -> TIO.putStrLn "Poll request sent"

runAuth :: IO ()
runAuth = do
  manager <- newManager defaultManagerSettings

  -- Show current account count
  currentCount <- getAccountCount manager
  when (currentCount > 0) $ do
    TIO.putStrLn $ "Currently have " <> showT currentCount <> " account(s) connected."
    TIO.putStrLn "Adding another account..."

  TIO.putStrLn "Starting OAuth flow..."
  TIO.putStrLn "Opening browser for Google authentication..."
  let authUrl = baseUrl <> "/auth/google"
  -- Open browser (cross-platform)
  callCommand $ "xdg-open '" <> authUrl <> "' 2>/dev/null || open '" <> authUrl <> "' 2>/dev/null || start '' '" <> authUrl <> "'"
  TIO.putStrLn "\nWaiting for authentication..."
  waitForNewAccount manager currentCount 60

getAccountCount :: Manager -> IO Int
getAccountCount manager = do
  req <- parseRequest $ baseUrl <> "/auth/status"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "count" obj of
      Just (Number n) -> return (round n)
      _ -> return 0
    _ -> return 0

-- Wait for account count to increase (new account added)
waitForNewAccount :: Manager -> Int -> Int -> IO ()
waitForNewAccount _ _ 0 = TIO.putStrLn "Timed out waiting for authentication."
waitForNewAccount manager oldCount remaining = do
  newCount <- getAccountCount manager
  if newCount > oldCount
    then TIO.putStrLn "Authentication successful! New account added."
    else do
      threadDelay 1000000 -- 1 second
      waitForNewAccount manager oldCount (remaining - 1)

runStatus :: IO ()
runStatus = do
  TIO.putStrLn "Wisp Status"
  TIO.putStrLn "==========="
  manager <- newManager defaultManagerSettings

  -- Check server health
  healthReq <- parseRequest $ baseUrl <> "/health"
  healthResp <- httpLbs healthReq manager
  case decode (responseBody healthResp) :: Maybe Value of
    Just _ -> TIO.putStrLn "Server:     online"
    Nothing -> TIO.putStrLn "Server:     offline"

  -- Check auth status (show connected accounts)
  authReq <- parseRequest $ baseUrl <> "/auth/status"
  authResp <- httpLbs authReq manager
  case decode (responseBody authResp) of
    Just (Object obj) -> do
      case KM.lookup "count" obj of
        Just (Number n) | n > 0 -> do
          TIO.putStrLn $ "Accounts:   " <> showT (round n :: Int) <> " connected"
          -- Show account emails
          case KM.lookup "accounts" obj of
            Just (Array accs) -> mapM_ showAccount (toList accs)
            _ -> return ()
        _ -> TIO.putStrLn "Accounts:   none connected"
    _ -> TIO.putStrLn "Accounts:   unknown"

  -- Check activities stats
  statsReq <- parseRequest $ baseUrl <> "/activities/stats"
  statsResp <- httpLbs statsReq manager
  case decode (responseBody statsResp) of
    Just (Object obj) -> do
      TIO.putStrLn "Activities:"
      let getCount key = case KM.lookup key obj of
            Just (Number n) -> round n :: Int
            _ -> 0
      let pending = getCount "pending"
      let needsReview = getCount "needs_review"
      let quarantined = getCount "quarantined"
      let surfaced = getCount "surfaced"
      when (pending > 0) $ TIO.putStrLn $ "            " <> showT pending <> " pending"
      when (needsReview > 0) $ TIO.putStrLn $ "            " <> showT needsReview <> " needs review"
      when (quarantined > 0) $ TIO.putStrLn $ "            " <> showT quarantined <> " quarantined"
      when (surfaced > 0) $ TIO.putStrLn $ "            " <> showT surfaced <> " surfaced"
      when (pending == 0 && needsReview == 0 && quarantined == 0 && surfaced == 0) $
        TIO.putStrLn "            all clear"
    _ -> TIO.putStrLn "Activities: unavailable"

showT :: (Show a) => a -> Text
showT = pack . show

-- Show a single account from auth status
showAccount :: Value -> IO ()
showAccount (Object acc) = case KM.lookup "email" acc of
  Just (String email) -> TIO.putStrLn $ "            - " <> email
  _ -> return ()
showAccount _ = return ()

-- GET /runs - List recent agent runs
runRuns :: TZ -> IO ()
runRuns tz = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/runs"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "runs" obj of
      Just (Array runs) | not (null runs) -> do
        TIO.putStrLn "Recent Agent Runs"
        TIO.putStrLn "================="
        TIO.putStrLn ""
        mapM_ (showRunBrief tz) (toList runs)
        case KM.lookup "count" obj of
          Just (Number n) -> TIO.putStrLn $ "\nTotal: " <> showT (round n :: Int) <> " runs"
          _ -> return ()
      _ -> TIO.putStrLn "No runs found"
    _ -> TIO.putStrLn "Failed to fetch runs"

-- Show a brief run line
showRunBrief :: TZ -> Value -> IO ()
showRunBrief tz (Object run) = do
  let getId = case KM.lookup "id" run of
        Just (String s) -> s
        _ -> "?"
  let getAgent = case KM.lookup "agent" run of
        Just (String s) -> s
        _ -> "unknown"
  let getStatus = case KM.lookup "status" run of
        Just (String "running") -> "🔄"
        Just (String "waiting") -> "⏸️"
        Just (String "completed") -> "✅"
        Just (String "failed") -> "❌"
        _ -> "⚪"
  let getCreated = case KM.lookup "created_at" run of
        Just (String s) -> formatDateLocal tz s
        _ -> ""
  TIO.putStrLn $ "  " <> getStatus <> " " <> getCreated <> " [" <> getId <> "] " <> getAgent
showRunBrief _ _ = return ()

-- GET /runs/:id - Show full run with events
runRun :: TZ -> Text -> IO ()
runRun tz rid = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/runs/" <> unpack rid
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object run) -> do
      TIO.putStrLn "Run Details"
      TIO.putStrLn "==========="
      showField "ID" "id" run
      showField "Agent" "agent" run
      showField "Status" "status" run
      showField "Session ID" "session_id" run
      showDateField tz "Created" "created_at" run
      showDateField tz "Updated" "updated_at" run
      TIO.putStrLn ""
      TIO.putStrLn "Events:"
      TIO.putStrLn "-------"
      case KM.lookup "events" run of
        Just (Array events) | not (null events) -> mapM_ (showRunEvent tz) (toList events)
        _ -> TIO.putStrLn "  No events"
    _ -> TIO.putStrLn "❌ Failed to fetch run (not found or error)"

-- Show a single run event
showRunEvent :: TZ -> Value -> IO ()
showRunEvent tz (Object event) = do
  let getType = case KM.lookup "type" event of
        Just (String s) -> s
        _ -> "unknown"
  let getTime = case KM.lookup "timestamp" event of
        Just (String s) -> formatTimeWithSeconds tz s
        _ -> "??:??:??"

  case getType of
    "input" -> do
      let tool = case KM.lookup "tool" event of
            Just (String s) -> s
            _ -> "?"
      TIO.putStrLn $ "  " <> getTime <> " 📥 Input from " <> tool
      case KM.lookup "data" event of
        Just (Object dataObj) -> do
          TIO.putStrLn "    Data:"
          forM_ (KM.toList dataObj) $ \(k, v) -> do
            let valuePreview = case v of
                  String s -> if T.length s > 100 then T.take 100 s <> "..." else s
                  Number n -> showT n
                  Bool b -> if b then "true" else "false"
                  Null -> "null"
                  Array arr -> "[" <> showT (length arr) <> " items]"
                  Object obj -> "{" <> showT (length (KM.toList obj)) <> " fields}"
            TIO.putStrLn $ "      " <> pack (show k) <> ": " <> valuePreview
        _ -> return ()

    "llm_called" -> do
      let model = case KM.lookup "model" event of
            Just (String s) -> s
            _ -> "?"

      -- Get token usage if available
      let inputTokens = case KM.lookup "input_tokens" event of
            Just (Number n) -> Just (round n :: Int)
            _ -> Nothing
      let outputTokens = case KM.lookup "output_tokens" event of
            Just (Number n) -> Just (round n :: Int)
            _ -> Nothing

      let tokenInfo = case (inputTokens, outputTokens) of
            (Just inp, Just out) ->
              " [" <> showT inp <> " in, " <> showT out <> " out, " <> showT (inp + out) <> " total]"
            _ -> ""

      TIO.putStrLn $ "  " <> getTime <> " 🤖 LLM called (" <> model <> ")" <> tokenInfo

      case KM.lookup "system_prompt" event of
        Just (String sp) -> do
          TIO.putStrLn "    System prompt:"
          showWrappedText "      " sp 100
        _ -> return ()

      case KM.lookup "user_prompt" event of
        Just (String up) -> do
          TIO.putStrLn "    User prompt:"
          showWrappedText "      " up 100
        _ -> return ()

      case KM.lookup "raw_response" event of
        Just (String resp) -> do
          TIO.putStrLn "    Response:"
          showWrappedText "      " resp 80
          TIO.putStrLn $ "    (Response length: " <> showT (T.length resp) <> " chars)"
        _ -> return ()

    "tool_requested" -> do
      let toolName = case KM.lookup "tool_name" event of
            Just (String s) -> s
            _ -> "?"
      TIO.putStrLn $ "  " <> getTime <> " 🔧 Tool requested: " <> toolName
      case KM.lookup "tool_args" event of
        Just v -> do
          TIO.putStrLn "    Arguments:"
          TIO.putStrLn $ "      " <> pack (show v)
        _ -> return ()

    "tool_succeeded" -> do
      let toolName = case KM.lookup "tool_name" event of
            Just (String s) -> s
            _ -> "?"
      TIO.putStrLn $ "  " <> getTime <> " ✅ Tool succeeded: " <> toolName
      case KM.lookup "result" event of
        Just v -> do
          TIO.putStrLn "    Result:"
          let resultStr = pack (show v)
          if T.length resultStr > 500
            then TIO.putStrLn $ "      " <> T.take 500 resultStr <> "... (truncated, " <> showT (T.length resultStr) <> " chars total)"
            else TIO.putStrLn $ "      " <> resultStr
        _ -> return ()

    "tool_failed" -> do
      let toolName = case KM.lookup "tool_name" event of
            Just (String s) -> s
            _ -> "?"
      let err = case KM.lookup "error" event of
            Just (String s) -> s
            _ -> "?"
      TIO.putStrLn $ "  " <> getTime <> " ❌ Tool failed: " <> toolName
      TIO.putStrLn $ "    Error: " <> err

    "context_assembled" -> do
      TIO.putStrLn $ "  " <> getTime <> " 📋 Context assembled"
      case KM.lookup "context" event of
        Just (Object ctx) -> do
          TIO.putStrLn $ "    Context fields: " <> showT (length (KM.toList ctx))
        _ -> return ()

    _ -> TIO.putStrLn $ "  " <> getTime <> " ❓ " <> getType
showRunEvent _ _ = return ()

-- Helper to show text with line wrapping and maximum number of lines
showWrappedText :: Text -> Text -> Int -> IO ()
showWrappedText prefix text maxLines = do
  let textLines = T.lines text
  let linesToShow = take maxLines textLines
  forM_ linesToShow $ \line -> TIO.putStrLn $ prefix <> line
  when (length textLines > maxLines) $
    TIO.putStrLn $ prefix <> "... (" <> showT (length textLines - maxLines) <> " more lines)"
