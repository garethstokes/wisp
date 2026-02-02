-- wisp-cli/app/Main.hs
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Aeson (Value(..), decode, encode, object, (.=))
import Data.Foldable (toList)
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
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
  | Today
  | Approve Text
  | Dismiss Text
  | People
  | Activity Text
  | Logs Text
  | Chat Text
  | Help
  deriving (Show)

-- Parse commands
commandParser :: Parser Command
commandParser = subparser
  ( command "auth" (info (pure Auth) (progDesc "Start OAuth flow"))
  <> command "status" (info (pure Status) (progDesc "Quick status overview"))
  <> command "poll" (info (pure Poll) (progDesc "Trigger a poll cycle"))
  <> command "classify" (info (pure Classify) (progDesc "Run classification pipeline"))
  <> command "today" (info (pure Today) (progDesc "Show activities requiring attention"))
  <> command "approve" (info approveParser (progDesc "Approve a quarantined activity"))
  <> command "dismiss" (info dismissParser (progDesc "Dismiss/archive an activity"))
  <> command "people" (info (pure People) (progDesc "List known people"))
  <> command "activity" (info activityParser (progDesc "Show activity details"))
  <> command "logs" (info logsParser (progDesc "Show activity processing logs"))
  <> command "chat" (info chatParser (progDesc "Ask Wisp a question"))
  <> command "help" (info (pure Help) (progDesc "Show help"))
  )

logsParser :: Parser Command
logsParser = Logs <$> strArgument (metavar "ID" <> help "Activity ID to show logs for")

chatParser :: Parser Command
chatParser = Chat <$> strArgument (metavar "MESSAGE" <> help "Message to send to Wisp")

activityParser :: Parser Command
activityParser = Activity <$> strArgument (metavar "ID" <> help "Activity ID to show")

approveParser :: Parser Command
approveParser = Approve <$> strArgument (metavar "ID" <> help "Activity ID to approve")

dismissParser :: Parser Command
dismissParser = Dismiss <$> strArgument (metavar "ID" <> help "Activity ID to dismiss")

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
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
    Today -> runToday
    Approve aid -> runApprove aid
    Dismiss aid -> runDismiss aid
    People -> runPeople
    Activity aid -> runActivity aid
    Logs aid -> runLogs aid
    Chat msg -> runChat msg
    Help -> runHelp

runHelp :: IO ()
runHelp = do
  TIO.putStrLn "wisp - your autonomy-preserving assistant"
  TIO.putStrLn ""
  TIO.putStrLn "Commands:"
  TIO.putStrLn "  auth         Start OAuth flow with Google"
  TIO.putStrLn "  status       Show server and auth status"
  TIO.putStrLn "  poll         Trigger a poll cycle"
  TIO.putStrLn "  classify     Run classification pipeline"
  TIO.putStrLn "  today        Show activities requiring attention"
  TIO.putStrLn "  approve ID   Approve a quarantined activity"
  TIO.putStrLn "  dismiss ID   Dismiss/archive an activity"
  TIO.putStrLn "  people       List known people"
  TIO.putStrLn "  activity ID  Show detailed activity info"
  TIO.putStrLn "  logs ID      Show activity processing logs"
  TIO.putStrLn "  chat MSG     Ask Wisp a question"
  TIO.putStrLn "  help         Show this help"
  TIO.putStrLn ""
  TIO.putStrLn "Use --help for more details"

runToday :: IO ()
runToday = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/today"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> do
      -- Show quarantined items first (need decision)
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
      -- Show total
      case KM.lookup "total" obj of
        Just (Number n) | n == 0 -> TIO.putStrLn "No activities requiring attention."
        _ -> return ()
    _ -> TIO.putStrLn "Failed to fetch today's activities"

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
  TIO.putStrLn $ prefix <> getSource <> " " <> getUrgency <> " [" <> getId <> "] " <> getTitle
showActivityBrief _ _ = return ()

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

  -- Check activities count
  activitiesReq <- parseRequest $ baseUrl <> "/activities"
  activitiesResp <- httpLbs activitiesReq manager
  case decode (responseBody activitiesResp) of
    Just (Object obj) -> do
      case KM.lookup "count" obj of
        Just (Number n) -> TIO.putStrLn $ "Activities: " <> showT (round n :: Int) <> " pending"
        _ -> TIO.putStrLn "Activities: unknown"
    _ -> TIO.putStrLn "Activities: unavailable"

showT :: Show a => a -> Text
showT = pack . show

-- Show a single account from auth status
showAccount :: Value -> IO ()
showAccount (Object acc) = case KM.lookup "email" acc of
  Just (String email) -> TIO.putStrLn $ "            - " <> email
  _ -> return ()
showAccount _ = return ()
