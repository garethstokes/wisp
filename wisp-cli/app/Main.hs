-- wisp-cli/app/Main.hs
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Aeson (Value(..), decode, encode, object, (.=))
import Data.Foldable (toList)
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text.IO as TIO
import Network.HTTP.Client
import Options.Applicative
import System.Process (callCommand)

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
  | Help
  deriving (Show)

data ChatOptions = ChatOptions
  { chatOptAgent   :: Text
  , chatOptMessage :: Maybe Text
  , chatOptSession :: Text
  , chatOptNew     :: Bool
  } deriving (Show)

data SessionsOptions = SessionsOptions
  { sessionsDelete :: Maybe Text
  } deriving (Show)

-- Parse commands
commandParser :: Parser Command
commandParser = subparser
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
  <> command "help" (info (pure Help) (progDesc "Show this help"))
  )

logsParser :: Parser Command
logsParser = Logs <$> strArgument (metavar "ID" <> help "Activity ID to show logs for")

chatOptionsParser :: Parser Command
chatOptionsParser = Chat <$> (ChatOptions
  <$> strOption (long "agent" <> short 'a' <> metavar "AGENT" <> help "Agent path (e.g., wisp/concierge)")
  <*> optional (strOption (long "message" <> short 'm' <> metavar "MSG" <> help "Message to send"))
  <*> strOption (long "session" <> short 's' <> metavar "NAME" <> value "default" <> help "Session name")
  <*> switch (long "new" <> help "Start fresh session"))

sessionsParser :: Parser Command
sessionsParser = Sessions <$> (SessionsOptions
  <$> optional (strOption (long "delete" <> short 'd' <> metavar "NAME" <> help "Delete session")))

activityParser :: Parser Command
activityParser = Activity <$> strArgument (metavar "ID" <> help "Activity ID to show")

approveParser :: Parser Command
approveParser = Approve <$> strArgument (metavar "ID" <> help "Activity ID to approve")

dismissParser :: Parser Command
dismissParser = Dismiss <$> strArgument (metavar "ID" <> help "Activity ID to dismiss")

-- Parser that defaults to Help when no command given
commandParserWithDefault :: Parser Command
commandParserWithDefault = commandParser <|> pure Help

opts :: ParserInfo Command
opts = info (commandParserWithDefault <**> helper)
  ( fullDesc
  <> progDesc "Wisp personal assistant CLI"
  <> header "wisp - your autonomy-preserving assistant"
  )

-- Base URL for wisp-srv
baseUrl :: String
baseUrl = "http://127.0.0.1:8080"

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Auth -> runAuth
    Status -> runStatus
    Poll -> runPoll
    Classify -> runClassify
    Inbox -> runInbox
    Review -> runReview
    Approve aid -> runApprove aid
    Dismiss aid -> runDismiss aid
    People -> runPeople
    Activity aid -> runActivity aid
    Logs aid -> runLogs aid
    Chat chatOpts -> runChatWithOptions chatOpts
    Agents -> runAgents
    Sessions sessOpts -> runSessions sessOpts
    Help -> runHelp

runHelp :: IO ()
runHelp = do
  TIO.putStrLn "wisp - your autonomy-preserving assistant"
  TIO.putStrLn ""
  TIO.putStrLn "Commands:"
  TIO.putStrLn "  status         Server status and activity counts"
  TIO.putStrLn "  inbox          Activities needing attention (surfaced, quarantined)"
  TIO.putStrLn "  review         Activities needing review (tier 3 - uncertain)"
  TIO.putStrLn "  activity ID    Show full details for an activity"
  TIO.putStrLn "  logs ID        Show processing history for an activity"
  TIO.putStrLn "  approve ID     Move quarantined activity to surfaced"
  TIO.putStrLn "  dismiss ID     Archive an activity"
  TIO.putStrLn "  chat MSG       Natural language queries and commands"
  TIO.putStrLn "  people         List contacts from activities"
  TIO.putStrLn "  poll           Fetch new emails and events now"
  TIO.putStrLn "  classify       Run classification on pending activities"
  TIO.putStrLn "  auth           Add a Google account via OAuth"
  TIO.putStrLn ""
  TIO.putStrLn "Activity IDs are shown in brackets, e.g. [abc123]"
  TIO.putStrLn "Use 'wisp inbox' to see activities and their IDs"

runInbox :: IO ()
runInbox = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/inbox"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> do
      -- Show today's calendar first
      case KM.lookup "calendar" obj of
        Just (Array items) | not (null items) -> do
          TIO.putStrLn "📅 Today's Schedule:"
          showCalendarWithGaps (toList items)
          TIO.putStrLn ""
        _ -> return ()
      -- Show quarantined items (need decision)
      case KM.lookup "quarantined" obj of
        Just (Array items) | not (null items) -> do
          TIO.putStrLn "⚠️  Quarantined (needs review):"
          mapM_ (showActivityBrief "  ") (toList items)
          TIO.putStrLn ""
        _ -> return ()
      -- Show surfaced items (ready to act)
      case KM.lookup "surfaced" obj of
        Just (Array items) | not (null items) -> do
          TIO.putStrLn "📋 Ready for action:"
          mapM_ (showActivityBrief "  ") (toList items)
          TIO.putStrLn ""
        _ -> return ()
      -- Show high urgency pending
      case KM.lookup "high_urgency" obj of
        Just (Array items) | not (null items) -> do
          TIO.putStrLn "🔥 High urgency (pending classification):"
          mapM_ (showActivityBrief "  ") (toList items)
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
showCalendarWithGaps :: [Value] -> IO ()
showCalendarWithGaps [] = TIO.putStrLn "  No events today."
showCalendarWithGaps events = do
  -- Sort events by start time and show with gaps
  let sorted = sortByStartTime events
  showEventsWithGaps Nothing sorted
  where
    sortByStartTime = id  -- Events already sorted by server

    showEventsWithGaps :: Maybe Text -> [Value] -> IO ()
    showEventsWithGaps _ [] = return ()
    showEventsWithGaps lastEnd (e:es) = do
      let mStart = getStartTime e
      let mEnd = getEndTime e
      -- Show gap if there's time between events
      case (lastEnd, mStart) of
        (Just end, Just start) | end < start -> do
          let gap = "  ⬜ " <> formatTimeRange end start <> " (free)"
          TIO.putStrLn gap
        _ -> return ()
      -- Show the event
      showCalendarEvent e
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
      extractTime start <> " - " <> extractTime end

    extractTime t = T.take 5 $ T.drop 11 $ T.takeWhile (/= '.') t  -- "HH:MM"

-- Show a calendar event
showCalendarEvent :: Value -> IO ()
showCalendarEvent (Object e) = do
  let title = case KM.lookup "title" e of
        Just (String s) -> s
        _ -> "(no title)"
  let timeStr = case KM.lookup "starts_at" e of
        Just (String s) -> extractTime s
        _ -> "??:??"
  let endStr = case KM.lookup "ends_at" e of
        Just (String s) -> " - " <> extractTime s
        _ -> ""
  TIO.putStrLn $ "  📅 " <> timeStr <> endStr <> " " <> title
  where
    extractTime t = T.take 5 $ T.drop 11 $ T.takeWhile (/= '.') t
showCalendarEvent _ = return ()

runReview :: IO ()
runReview = do
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
          mapM_ (showActivityBrief "  ") (toList items)
          TIO.putStrLn ""
          TIO.putStrLn "Use 'wisp activity ID' to see details, 'wisp approve ID' or 'wisp dismiss ID' to act"
        _ -> TIO.putStrLn "No activities needing review."
    _ -> TIO.putStrLn "Failed to fetch review queue"

-- Show a brief activity line
showActivityBrief :: Text -> Value -> IO ()
showActivityBrief prefix (Object act) = do
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
        Just (String s) -> formatDate s
        _ -> case KM.lookup "created_at" act of
          Just (String s) -> formatDate s
          _ -> ""
  let datePart = if getDate == "" then "" else getDate <> " "
  TIO.putStrLn $ prefix <> getSource <> " " <> getUrgency <> " " <> datePart <> "[" <> getId <> "] " <> getTitle
showActivityBrief _ _ = return ()

-- Format ISO date to short form (e.g., "Jan 30" or "Feb 3 14:00")
formatDate :: Text -> Text
formatDate isoDate =
  let dateStr = T.takeWhile (/= '.') isoDate  -- Remove milliseconds
      -- Extract date and time parts
      datePart = T.take 10 dateStr  -- "2026-01-30"
      timePart = T.drop 11 dateStr  -- "14:00:00" or empty
      -- Parse month and day
      month = case T.take 2 (T.drop 5 datePart) of
        "01" -> "Jan"; "02" -> "Feb"; "03" -> "Mar"; "04" -> "Apr"
        "05" -> "May"; "06" -> "Jun"; "07" -> "Jul"; "08" -> "Aug"
        "09" -> "Sep"; "10" -> "Oct"; "11" -> "Nov"; "12" -> "Dec"
        _ -> "???"
      day = T.dropWhile (== '0') $ T.drop 8 datePart
      time = T.take 5 timePart  -- "14:00"
  in if T.null timePart || time == "00:00"
     then month <> " " <> day
     else month <> " " <> day <> " " <> time

runApprove :: Text -> IO ()
runApprove aid = do
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/activities/" <> unpack aid <> "/approve"
  let req = initialReq { method = "POST" }
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
  let req = initialReq { method = "POST" }
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

runActivity :: Text -> IO ()
runActivity aid = do
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
      showField "Created" "created_at" act
      showField "Starts" "starts_at" act
      showField "Ends" "ends_at" act
    _ -> TIO.putStrLn "❌ Failed to fetch activity (not found or error)"

-- Show a field from a JSON object
showField :: Text -> Text -> KM.KeyMap Value -> IO ()
showField label key obj = case KM.lookup (fromString $ unpack key) obj of
  Just (String s) -> TIO.putStrLn $ label <> ": " <> s
  Just (Number n) -> TIO.putStrLn $ label <> ": " <> showT n
  Just Null -> return ()
  Just v -> TIO.putStrLn $ label <> ": " <> pack (show v)
  Nothing -> return ()

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
        Just (Bool True) -> " [implemented]"
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

runChatWithOptions :: ChatOptions -> IO ()
runChatWithOptions chatOpts = do
  let agent = chatOptAgent chatOpts
  case chatOptMessage chatOpts of
    Nothing -> TIO.putStrLn "Please provide a message with --message"
    Just msg -> do
      manager <- newManager defaultManagerSettings
      initialReq <- parseRequest $ baseUrl <> "/chat"
      -- For now, just send single message (session support in Task 10)
      let reqBody = object
            [ "agent" .= agent
            , "messages" .= [object ["role" .= ("user" :: Text), "content" .= msg]]
            ]
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

runSessions :: SessionsOptions -> IO ()
runSessions _opts = do
  -- Placeholder for Task 10
  TIO.putStrLn "Session management not yet implemented. Coming soon!"

runClassify :: IO ()
runClassify = do
  TIO.putStrLn "Running classification pipeline..."
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/pipeline/run"
  let req = initialReq { method = "POST" }
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
  let req = initialReq { method = "POST" }
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
      threadDelay 1000000  -- 1 second
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

showT :: Show a => a -> Text
showT = pack . show

-- Show a single account from auth status
showAccount :: Value -> IO ()
showAccount (Object acc) = case KM.lookup "email" acc of
  Just (String email) -> TIO.putStrLn $ "            - " <> email
  _ -> return ()
showAccount _ = return ()
