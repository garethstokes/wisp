-- wisp-cli/app/Main.hs
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), decode, encode, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding (decodeUtf8)
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
data AuthProvider = AuthGoogle | AuthGitHub
  deriving (Show)

data Command
  = Auth AuthProvider
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
  | Agents (Maybe Text)           -- List agents or show one
  | Skills (Maybe Text)           -- List skills or show one
  | Activate Text Text            -- agent skill
  | Deactivate Text               -- agent
  | Tenant TenantCommand          -- Tenant management
  | Project ProjectCommand        -- Project management
  | Note NoteCommand              -- Note management
  | Pref PrefCommand              -- Preference management
  | Sessions SessionsOptions
  | Runs
  | Run Text
  | Help
  deriving (Show)

data TenantCommand
  = TenantList
  | TenantCreate Text             -- name
  | TenantShow Text               -- id
  deriving (Show)

data ProjectCommand
  = ProjectList
  | ProjectCreate Text Text       -- name, type
  | ProjectArchive Text           -- id
  deriving (Show)

data NoteCommand
  = NoteList (Maybe Text)         -- optional tag filter
  | NoteCreate Text (Maybe Text) [Text]  -- title, content, tags
  deriving (Show)

data PrefCommand
  = PrefList
  | PrefSet Text Text (Maybe Text)  -- key, value, context
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
        <> command "agents" (info agentsParser (progDesc "List agents or show agent details"))
        <> command "skills" (info skillsParser (progDesc "List skills or show skill details"))
        <> command "activate" (info activateParser (progDesc "Activate a skill for an agent"))
        <> command "deactivate" (info deactivateParser (progDesc "Deactivate current skill for an agent"))
        <> command "tenant" (info tenantParser (progDesc "Manage tenants"))
        <> command "project" (info projectParser (progDesc "Manage projects"))
        <> command "note" (info noteParser (progDesc "Manage notes"))
        <> command "pref" (info prefParser (progDesc "Manage preferences"))
        <> command "sessions" (info sessionsParser (progDesc "Manage chat sessions"))
        <> command "people" (info (pure People) (progDesc "List contacts extracted from activities"))
        <> command "poll" (info (pure Poll) (progDesc "Fetch new emails and calendar events now"))
        <> command "classify" (info (pure Classify) (progDesc "Run classification on pending activities"))
        <> command "auth" (info authParser (progDesc "Add an account via OAuth (google or github)"))
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

agentsParser :: Parser Command
agentsParser = Agents <$> optional (strArgument (metavar "NAME" <> help "Agent name to show details"))

skillsParser :: Parser Command
skillsParser = Skills <$> optional (strArgument (metavar "NAME" <> help "Skill name to show details"))

activateParser :: Parser Command
activateParser = Activate
  <$> strArgument (metavar "AGENT" <> help "Agent name")
  <*> strArgument (metavar "SKILL" <> help "Skill to activate")

deactivateParser :: Parser Command
deactivateParser = Deactivate <$> strArgument (metavar "AGENT" <> help "Agent name")

tenantParser :: Parser Command
tenantParser = Tenant <$> subparser
  ( command "list" (info (pure TenantList) (progDesc "List all tenants"))
  <> command "create" (info tenantCreateParser (progDesc "Create a new tenant"))
  <> command "show" (info tenantShowParser (progDesc "Show tenant details"))
  )

tenantCreateParser :: Parser TenantCommand
tenantCreateParser = TenantCreate <$> strArgument (metavar "NAME" <> help "Tenant name")

tenantShowParser :: Parser TenantCommand
tenantShowParser = TenantShow <$> strArgument (metavar "ID" <> help "Tenant UUID")

projectParser :: Parser Command
projectParser = Project <$> subparser
  ( command "list" (info (pure ProjectList) (progDesc "List active projects"))
  <> command "create" (info projectCreateParser (progDesc "Create a project"))
  <> command "archive" (info projectArchiveParser (progDesc "Archive a project"))
  )

projectCreateParser :: Parser ProjectCommand
projectCreateParser = ProjectCreate
  <$> strArgument (metavar "NAME" <> help "Project name")
  <*> strArgument (metavar "TYPE" <> help "Type: work, personal, family, health, spiritual")

projectArchiveParser :: Parser ProjectCommand
projectArchiveParser = ProjectArchive
  <$> strArgument (metavar "ID" <> help "Project ID")

noteParser :: Parser Command
noteParser = Note <$> subparser
  ( command "list" (info noteListParser (progDesc "List notes"))
  <> command "create" (info noteCreateParser (progDesc "Create a note"))
  )

noteListParser :: Parser NoteCommand
noteListParser = NoteList <$> optional (strOption (long "tag" <> short 't' <> metavar "TAG"))

noteCreateParser :: Parser NoteCommand
noteCreateParser = NoteCreate
  <$> strArgument (metavar "TITLE" <> help "Note title")
  <*> optional (strOption (long "content" <> short 'c' <> metavar "CONTENT"))
  <*> (parseTags <$> strOption (long "tags" <> value "" <> metavar "TAGS" <> help "Comma-separated tags"))
  where
    parseTags s = filter (not . T.null) $ map T.strip $ T.splitOn "," (T.pack s)

prefParser :: Parser Command
prefParser = Pref <$> subparser
  ( command "list" (info (pure PrefList) (progDesc "List preferences"))
  <> command "set" (info prefSetParser (progDesc "Set a preference"))
  )

prefSetParser :: Parser PrefCommand
prefSetParser = PrefSet
  <$> strArgument (metavar "KEY" <> help "Preference key")
  <*> strArgument (metavar "VALUE" <> help "Preference value")
  <*> optional (strOption (long "context" <> metavar "CONTEXT"))

authParser :: Parser Command
authParser = Auth <$> subparser
  ( command "google" (info (pure AuthGoogle) (progDesc "Add a Google account via OAuth"))
  <> command "github" (info (pure AuthGitHub) (progDesc "Add a GitHub account via OAuth"))
  )

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
    Auth provider -> runAuth provider
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
    Agents mName -> runAgents mName
    Skills mName -> runSkills mName
    Activate agent skill -> runActivate agent skill
    Deactivate agent -> runDeactivate agent
    Tenant tenantCmd -> runTenant tenantCmd
    Project projectCmd -> runProject projectCmd
    Note noteCmd -> runNote noteCmd
    Pref prefCmd -> runPref prefCmd
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
  TIO.putStrLn "  agents NAME         Show agent details"
  TIO.putStrLn "  skills              List available skills"
  TIO.putStrLn "  skills NAME         Show skill details and prompt"
  TIO.putStrLn "  activate AGENT SKILL  Activate a skill for an agent"
  TIO.putStrLn "  deactivate AGENT    Deactivate current skill for agent"
  TIO.putStrLn "  tenant list         List all tenants"
  TIO.putStrLn "  tenant create NAME  Create a new tenant"
  TIO.putStrLn "  tenant show ID      Show tenant details"
  TIO.putStrLn "  project list        List active projects"
  TIO.putStrLn "  project create NAME TYPE  Create a project"
  TIO.putStrLn "  project archive ID  Archive a project"
  TIO.putStrLn "  note list           List notes"
  TIO.putStrLn "    -t, --tag TAG     Filter by tag"
  TIO.putStrLn "  note create TITLE   Create a note"
  TIO.putStrLn "    -c, --content     Note content"
  TIO.putStrLn "    --tags            Comma-separated tags"
  TIO.putStrLn "  pref list           List preferences"
  TIO.putStrLn "  pref set KEY VALUE  Set a preference"
  TIO.putStrLn "    --context         Optional context"
  TIO.putStrLn "  sessions            List chat sessions"
  TIO.putStrLn "    -d, --delete      Delete a session"
  TIO.putStrLn "  people              List contacts from activities"
  TIO.putStrLn "  poll                Fetch new emails and events now"
  TIO.putStrLn "  classify            Run classification on pending activities"
  TIO.putStrLn "  auth google         Add a Google account via OAuth"
  TIO.putStrLn "  auth github         Add a GitHub account via OAuth"
  TIO.putStrLn "  runs                List recent agent runs"
  TIO.putStrLn "  run ID              Show full details for an agent run"
  TIO.putStrLn ""
  TIO.putStrLn "Examples:"
  TIO.putStrLn "  wisp agents                           List all agents"
  TIO.putStrLn "  wisp agents wisp                      Show wisp agent details"
  TIO.putStrLn "  wisp skills                           List all skills"
  TIO.putStrLn "  wisp skills concierge                 Show concierge skill"
  TIO.putStrLn "  wisp activate wisp concierge          Activate concierge for wisp"
  TIO.putStrLn "  wisp deactivate wisp                  Deactivate wisp's skill"
  TIO.putStrLn "  wisp chat -a wisp/concierge -m \"hi\"   Chat with agent"
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

runAgents :: Maybe Text -> IO ()
runAgents Nothing = do
  -- List all agents from new API
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/api/agents"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "agents" obj of
      Just (Array agents) -> do
        TIO.putStrLn "Available Agents"
        TIO.putStrLn "================"
        TIO.putStrLn ""
        if null agents
          then TIO.putStrLn "No agents found. Run seeds to create default agent."
          else mapM_ showAgentName (toList agents)
      _ -> TIO.putStrLn "No agents found"
    _ -> TIO.putStrLn "Failed to fetch agents"
runAgents (Just name) = do
  -- Show single agent details
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/api/agents/" <> unpack name
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object agent) -> do
      case KM.lookup "error" agent of
        Just (String err) -> TIO.putStrLn $ "Error: " <> err
        _ -> showAgentDetail agent
    _ -> TIO.putStrLn $ "Failed to fetch agent: " <> name

showAgentName :: Value -> IO ()
showAgentName (String name) = TIO.putStrLn $ "  " <> name
showAgentName _ = return ()

showAgentDetail :: KM.KeyMap Value -> IO ()
showAgentDetail agent = do
  let getName = case KM.lookup "name" agent of
        Just (String s) -> s
        _ -> "?"
  let getPersonality = case KM.lookup "personality" agent of
        Just (String s) -> s
        Just Null -> "(none)"
        _ -> "(none)"
  let getActiveSkill = case KM.lookup "active_skill" agent of
        Just (String s) -> Just s
        _ -> Nothing
  let getAvailableSkills = case KM.lookup "available_skills" agent of
        Just (Array ss) -> [s | String s <- toList ss]
        _ -> []

  TIO.putStrLn $ "Agent: " <> getName
  TIO.putStrLn "========"
  TIO.putStrLn ""
  TIO.putStrLn $ "Personality: " <> getPersonality
  TIO.putStrLn ""
  case getActiveSkill of
    Just skill -> TIO.putStrLn $ "Active Skill: " <> skill
    Nothing -> TIO.putStrLn "Active Skill: (none)"
  TIO.putStrLn ""
  TIO.putStrLn $ "Available Skills: " <> T.intercalate ", " getAvailableSkills
  TIO.putStrLn ""

  -- Show soul info if present
  case KM.lookup "soul" agent of
    Just (Object soul) -> do
      TIO.putStrLn "Soul:"
      case KM.lookup "personality" soul of
        Just (String p) | not (T.null p) -> TIO.putStrLn $ "  Communication style: " <> p
        _ -> return ()
      case KM.lookup "insights" soul of
        Just (Array insights) | not (null insights) -> do
          TIO.putStrLn "  Insights:"
          forM_ (toList insights) $ \i -> case i of
            String s -> TIO.putStrLn $ "    - " <> s
            _ -> return ()
        _ -> return ()
    _ -> return ()

runSkills :: Maybe Text -> IO ()
runSkills Nothing = do
  -- List all skills
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/api/skills"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "skills" obj of
      Just (Array skills) -> do
        TIO.putStrLn "Available Skills"
        TIO.putStrLn "================"
        TIO.putStrLn ""
        mapM_ showSkillBrief (toList skills)
      _ -> TIO.putStrLn "No skills found"
    _ -> TIO.putStrLn "Failed to fetch skills"
runSkills (Just name) = do
  -- Show single skill details
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/api/skills/" <> unpack name
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object skill) -> do
      case KM.lookup "error" skill of
        Just (String err) -> TIO.putStrLn $ "Error: " <> err
        _ -> showSkillDetail skill
    _ -> TIO.putStrLn $ "Failed to fetch skill: " <> name

showSkillBrief :: Value -> IO ()
showSkillBrief (Object s) = do
  let getName = case KM.lookup "name" s of
        Just (String n) -> n
        _ -> "?"
  let getTools = case KM.lookup "tools" s of
        Just (Array ts) -> [t | String t <- toList ts]
        _ -> []
  let getAvailable = case KM.lookup "available" s of
        Just (Bool True) -> ""
        _ -> " [not available]"
  TIO.putStrLn $ "  " <> getName <> getAvailable
  TIO.putStrLn $ "    Tools: " <> T.intercalate ", " getTools
showSkillBrief _ = return ()

showSkillDetail :: KM.KeyMap Value -> IO ()
showSkillDetail skill = do
  let getName = case KM.lookup "name" skill of
        Just (String s) -> s
        _ -> "?"
  let getTools = case KM.lookup "tools" skill of
        Just (Array ts) -> [t | String t <- toList ts]
        _ -> []
  let getTag = case KM.lookup "tag" skill of
        Just (String s) -> s
        _ -> ""

  TIO.putStrLn $ "Skill: " <> getName
  TIO.putStrLn "========"
  TIO.putStrLn ""
  TIO.putStrLn $ "Tag: " <> getTag
  TIO.putStrLn $ "Tools: " <> T.intercalate ", " getTools
  TIO.putStrLn ""

  case KM.lookup "prompt" skill of
    Just (String prompt) -> do
      TIO.putStrLn "Prompt:"
      TIO.putStrLn "-------"
      TIO.putStrLn prompt
    Just Null -> TIO.putStrLn "Prompt: (not set - run seeds to initialize)"
    _ -> TIO.putStrLn "Prompt: (not set)"

runActivate :: Text -> Text -> IO ()
runActivate agent skill = do
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/api/agents/" <> unpack agent <> "/activate/" <> unpack skill
  let reqBody = object ["confirm" .= True]
  let req = initialReq
        { method = "POST"
        , requestHeaders = [("Content-Type", "application/json")]
        , requestBody = RequestBodyLBS (encode reqBody)
        }
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "status" obj of
      Just (String "activated") -> TIO.putStrLn $ "✅ Activated " <> skill <> " for agent " <> agent
      _ -> case KM.lookup "error" obj of
        Just (String err) -> TIO.putStrLn $ "❌ Error: " <> err
        _ -> TIO.putStrLn "Activation request sent"
    _ -> TIO.putStrLn "❌ Failed to activate skill"

runDeactivate :: Text -> IO ()
runDeactivate agent = do
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/api/agents/" <> unpack agent <> "/deactivate"
  let req = initialReq { method = "POST" }
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "status" obj of
      Just (String "deactivated") -> TIO.putStrLn $ "✅ Deactivated skill for agent " <> agent
      _ -> case KM.lookup "error" obj of
        Just (String err) -> TIO.putStrLn $ "❌ Error: " <> err
        _ -> TIO.putStrLn "Deactivation request sent"
    _ -> TIO.putStrLn "❌ Failed to deactivate skill"

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

runAuth :: AuthProvider -> IO ()
runAuth provider = do
  manager <- newManager defaultManagerSettings

  -- Show current account count
  currentCount <- getAccountCount manager
  when (currentCount > 0) $ do
    TIO.putStrLn $ "Currently have " <> showT currentCount <> " account(s) connected."
    TIO.putStrLn "Adding another account..."

  let (providerName, authPath) = case provider of
        AuthGoogle -> ("Google", "/auth/google")
        AuthGitHub -> ("GitHub", "/auth/github")

  TIO.putStrLn "Starting OAuth flow..."
  TIO.putStrLn $ "Opening browser for " <> providerName <> " authentication..."
  let authUrl = baseUrl <> authPath
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

  -- Check auth status (show connected accounts and tenant)
  authReq <- parseRequest $ baseUrl <> "/auth/status"
  authResp <- httpLbs authReq manager
  case decode (responseBody authResp) of
    Just (Object obj) -> do
      -- Show tenant first
      case KM.lookup "tenant" obj of
        Just (Object tenant) -> do
          let getName = case KM.lookup "name" tenant of
                Just (String s) -> s
                _ -> "?"
          let getId = case KM.lookup "id" tenant of
                Just (String s) -> s
                _ -> ""
          TIO.putStrLn $ "Tenant:     " <> getName
          TIO.putStrLn $ "            " <> getId
        Just Null -> TIO.putStrLn "Tenant:     none (run: wisp tenant create <name>)"
        Nothing -> TIO.putStrLn "Tenant:     none (run: wisp tenant create <name>)"
        _ -> return ()
      -- Show accounts
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
showAccount (Object acc) = do
  let provider = case KM.lookup "provider" acc of
        Just (String "Google") -> "google"
        Just (String "GitHub") -> "github"
        Just (String p) -> p
        _ -> "?"
  let identifier = case KM.lookup "identifier" acc of
        Just (String i) -> i
        Just Null -> "(unknown)"
        _ -> "(unknown)"
  let displayName = case KM.lookup "display_name" acc of
        Just (String n) -> " (" <> n <> ")"
        _ -> ""
  TIO.putStrLn $ "            - [" <> provider <> "] " <> identifier <> displayName
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
  let getSession = case KM.lookup "session_id" run of
        Just (String s) -> s
        Just Null -> "-"
        _ -> "-"
  let getStatus = case KM.lookup "status" run of
        Just (String "running") -> "🔄"
        Just (String "waiting") -> "⏸️"
        Just (String "completed") -> "✅"
        Just (String "failed") -> "❌"
        _ -> "⚪"
  let getCreated = case KM.lookup "created_at" run of
        Just (String s) -> formatDateLocal tz s
        _ -> ""
  TIO.putStrLn $ "  " <> getStatus <> " " <> getCreated <> " [" <> getId <> "] " <> getAgent <> " (session: " <> getSession <> ")"
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
      TIO.putStrLn "Conversation:"
      TIO.putStrLn "============="
      case KM.lookup "events" run of
        Just (Array events) | not (null events) -> showConversation tz (toList events)
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
          showFullText "      " sp
        _ -> return ()

      case KM.lookup "user_prompt" event of
        Just (String up) -> do
          TIO.putStrLn "    User prompt:"
          showFullText "      " up
        _ -> return ()

      case KM.lookup "raw_response" event of
        Just (String resp) -> do
          TIO.putStrLn "    Response:"
          showFullText "      " resp
        _ -> return ()

    "tool_requested" -> do
      let toolName = case KM.lookup "tool_name" event of
            Just (String s) -> s
            _ -> "?"
      TIO.putStrLn $ "  " <> getTime <> " 🔧 Tool requested: " <> toolName
      case KM.lookup "tool_args" event of
        Just v -> do
          TIO.putStrLn "    Arguments:"
          TIO.putStrLn $ "      " <> decodeUtf8 (BL.toStrict (encode v))
        _ -> return ()

    "tool_succeeded" -> do
      let toolName = case KM.lookup "tool_name" event of
            Just (String s) -> s
            _ -> "?"
      TIO.putStrLn $ "  " <> getTime <> " ✅ Tool succeeded: " <> toolName
      case KM.lookup "result" event of
        Just v -> do
          TIO.putStrLn "    Result:"
          showFullText "      " (decodeUtf8 (BL.toStrict (encode v)))
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

-- Helper to show full text without truncation
showFullText :: Text -> Text -> IO ()
showFullText prefix text = do
  let textLines = T.lines text
  forM_ textLines $ \line -> TIO.putStrLn $ prefix <> line

-- | Show conversation in a clear sequential format
showConversation :: TZ -> [Value] -> IO ()
showConversation tz events = do
  -- First show system prompt from the first llm_called event
  case findFirstLlmCall events of
    Just (Object ev) -> do
      TIO.putStrLn ""
      TIO.putStrLn "───── system prompt ─────"
      case KM.lookup "system_prompt" ev of
        Just (String sp) -> TIO.putStrLn sp
        _ -> TIO.putStrLn "(none)"
    _ -> return ()

  -- Then show the conversation turns
  TIO.putStrLn ""
  mapM_ (showConversationEvent tz) events

findFirstLlmCall :: [Value] -> Maybe Value
findFirstLlmCall [] = Nothing
findFirstLlmCall (v@(Object ev):rest) = case KM.lookup "type" ev of
  Just (String "llm_called") -> Just v
  _ -> findFirstLlmCall rest
findFirstLlmCall (_:rest) = findFirstLlmCall rest

showConversationEvent :: TZ -> Value -> IO ()
showConversationEvent _tz (Object event) = do
  let getType = case KM.lookup "type" event of
        Just (String s) -> s
        _ -> "unknown"

  case getType of
    "llm_called" -> do
      -- Show user prompt (input to LLM)
      TIO.putStrLn ""
      TIO.putStrLn "───── user message ─────"
      case KM.lookup "user_prompt" event of
        Just (String up) -> TIO.putStrLn up
        _ -> TIO.putStrLn "(none)"

      -- Show LLM response
      TIO.putStrLn ""
      TIO.putStrLn "───── agent response ─────"
      case KM.lookup "raw_response" event of
        Just (String resp) -> TIO.putStrLn resp
        _ -> TIO.putStrLn "(none)"

      -- Show token usage
      let inputTokens = case KM.lookup "input_tokens" event of
            Just (Number n) -> Just (round n :: Int)
            _ -> Nothing
      let outputTokens = case KM.lookup "output_tokens" event of
            Just (Number n) -> Just (round n :: Int)
            _ -> Nothing
      case (inputTokens, outputTokens) of
        (Just inp, Just out) ->
          TIO.putStrLn $ "[tokens: " <> showT inp <> " in, " <> showT out <> " out]"
        _ -> return ()

    "tool_requested" -> do
      TIO.putStrLn ""
      TIO.putStrLn "───── tool call ─────"
      let toolName = case KM.lookup "tool_name" event of
            Just (String s) -> s
            _ -> "?"
      TIO.putStrLn $ "tool: " <> toolName
      case KM.lookup "tool_args" event of
        Just v -> TIO.putStrLn $ "args: " <> decodeUtf8 (BL.toStrict (encode v))
        _ -> return ()

    "tool_succeeded" -> do
      TIO.putStrLn ""
      TIO.putStrLn "───── tool result ─────"
      let toolName = case KM.lookup "tool_name" event of
            Just (String s) -> s
            _ -> "?"
      TIO.putStrLn $ "tool: " <> toolName <> " ✓"
      case KM.lookup "result" event of
        Just v -> TIO.putStrLn $ decodeUtf8 $ BL.toStrict $ encode v
        _ -> return ()

    "tool_failed" -> do
      TIO.putStrLn ""
      TIO.putStrLn "───── tool error ─────"
      let toolName = case KM.lookup "tool_name" event of
            Just (String s) -> s
            _ -> "?"
      let err = case KM.lookup "error" event of
            Just (String s) -> s
            _ -> "?"
      TIO.putStrLn $ "tool: " <> toolName <> " ✗"
      TIO.putStrLn $ "error: " <> err

    "input" -> return ()  -- Skip input events (covered by user_prompt in llm_called)

    _ -> return ()  -- Skip other events

showConversationEvent _ _ = return ()

--------------------------------------------------------------------------------
-- Tenant Commands
--------------------------------------------------------------------------------

runTenant :: TenantCommand -> IO ()
runTenant TenantList = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/api/tenants"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "tenants" obj of
      Just (Array tenants) -> do
        TIO.putStrLn "Tenants"
        TIO.putStrLn "======="
        TIO.putStrLn ""
        if null tenants
          then TIO.putStrLn "No tenants found. Create one with: wisp tenant create <name>"
          else mapM_ showTenantBrief (toList tenants)
      _ -> TIO.putStrLn "No tenants found"
    _ -> TIO.putStrLn "Failed to fetch tenants"

runTenant (TenantCreate name) = do
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/api/tenants"
  let reqBody = object ["createTenantName" .= name]
  let req = initialReq
        { method = "POST"
        , requestHeaders = [("Content-Type", "application/json")]
        , requestBody = RequestBodyLBS (encode reqBody)
        }
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "tenant" obj of
      Just (Object tenant) -> do
        let getId = case KM.lookup "id" tenant of
              Just (String s) -> s
              _ -> "?"
        let getName = case KM.lookup "name" tenant of
              Just (String s) -> s
              _ -> "?"
        TIO.putStrLn $ "✅ Created tenant: " <> getName
        TIO.putStrLn $ "   ID: " <> getId
        TIO.putStrLn ""
        TIO.putStrLn "Next steps:"
        TIO.putStrLn "  1. Link accounts to this tenant during OAuth"
        TIO.putStrLn "  2. Run seeds to create agents and skills"
      _ -> case KM.lookup "error" obj of
        Just (String err) -> TIO.putStrLn $ "❌ Error: " <> err
        _ -> TIO.putStrLn "Tenant created"
    _ -> TIO.putStrLn "❌ Failed to create tenant"

runTenant (TenantShow tid) = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/api/tenants/" <> unpack tid
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object tenant) -> case KM.lookup "error" tenant of
      Just (String err) -> TIO.putStrLn $ "Error: " <> err
      _ -> showTenantDetail tenant
    _ -> TIO.putStrLn $ "Failed to fetch tenant: " <> tid

showTenantBrief :: Value -> IO ()
showTenantBrief (Object tenant) = do
  let getId = case KM.lookup "id" tenant of
        Just (String s) -> s
        _ -> "?"
  let getName = case KM.lookup "name" tenant of
        Just (String s) -> s
        _ -> "?"
  TIO.putStrLn $ "  " <> getId <> "  " <> getName
showTenantBrief _ = return ()

showTenantDetail :: KM.KeyMap Value -> IO ()
showTenantDetail tenant = do
  let getId = case KM.lookup "id" tenant of
        Just (String s) -> s
        _ -> "?"
  let getName = case KM.lookup "name" tenant of
        Just (String s) -> s
        _ -> "?"
  let getCreated = case KM.lookup "created_at" tenant of
        Just (String s) -> s
        _ -> "?"
  TIO.putStrLn $ "Tenant: " <> getName
  TIO.putStrLn "========"
  TIO.putStrLn ""
  TIO.putStrLn $ "ID:         " <> getId
  TIO.putStrLn $ "Created:    " <> getCreated

--------------------------------------------------------------------------------
-- Project Commands
--------------------------------------------------------------------------------

runProject :: ProjectCommand -> IO ()
runProject ProjectList = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/api/projects"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just val -> case val of
      Object obj -> case KM.lookup "projects" obj of
        Just (Array projects) -> do
          TIO.putStrLn "Active Projects"
          TIO.putStrLn "==============="
          if null projects
            then TIO.putStrLn "No projects found."
            else mapM_ showProject (toList projects)
        _ -> TIO.putStrLn "No projects found"
      _ -> TIO.putStrLn "Failed to parse projects"
    _ -> TIO.putStrLn "Failed to fetch projects"

runProject (ProjectCreate name projType) = do
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/api/projects"
  let reqBody = object ["name" .= name, "type" .= projType]
  let req = initialReq
        { method = "POST"
        , requestHeaders = [("Content-Type", "application/json")]
        , requestBody = RequestBodyLBS (encode reqBody)
        }
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just val -> case val of
      Object obj -> case KM.lookup "id" obj of
        Just (String pid) -> TIO.putStrLn $ "Created project: " <> pid
        _ -> TIO.putStrLn "Project created"
      _ -> TIO.putStrLn "Project created"
    _ -> TIO.putStrLn "Failed to create project"

runProject (ProjectArchive pid) = do
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/api/projects/" <> unpack pid <> "/archive"
  let req = initialReq { method = "POST" }
  _ <- httpLbs req manager
  TIO.putStrLn $ "Archived project: " <> pid

showProject :: Value -> IO ()
showProject (Object p) = do
  let getId = case KM.lookup "id" p of
        Just (String s) -> s
        _ -> "?"
  let getData = KM.lookup "data" p
  let name = case getData of
        Just (Object d) -> case KM.lookup "name" d of
          Just (String n) -> n
          _ -> "?"
        _ -> "?"
  let ptype = case getData of
        Just (Object d) -> case KM.lookup "type" d of
          Just (String t) -> t
          _ -> "?"
        _ -> "?"
  TIO.putStrLn $ "  [" <> T.take 8 getId <> "] " <> name <> " (" <> ptype <> ")"
showProject _ = pure ()

--------------------------------------------------------------------------------
-- Note Commands
--------------------------------------------------------------------------------

runNote :: NoteCommand -> IO ()
runNote (NoteList _mTag) = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/api/notes"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just val -> case val of
      Object obj -> case KM.lookup "notes" obj of
        Just (Array notes) -> do
          TIO.putStrLn "Notes"
          TIO.putStrLn "====="
          if null notes
            then TIO.putStrLn "No notes found."
            else mapM_ showNote (toList notes)
        _ -> TIO.putStrLn "No notes found"
      _ -> TIO.putStrLn "Failed to parse notes"
    _ -> TIO.putStrLn "Failed to fetch notes"

runNote (NoteCreate title mContent tags) = do
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/api/notes"
  let reqBody = object $
        [ "title" .= title ]
        <> maybe [] (\c -> ["content" .= c]) mContent
        <> if null tags then [] else ["tags" .= tags]
  let req = initialReq
        { method = "POST"
        , requestHeaders = [("Content-Type", "application/json")]
        , requestBody = RequestBodyLBS (encode reqBody)
        }
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just val -> case val of
      Object obj -> case KM.lookup "id" obj of
        Just (String nid) -> TIO.putStrLn $ "Created note: " <> nid
        _ -> TIO.putStrLn "Note created"
      _ -> TIO.putStrLn "Note created"
    _ -> TIO.putStrLn "Failed to create note"

showNote :: Value -> IO ()
showNote (Object n) = do
  let getId = case KM.lookup "id" n of
        Just (String s) -> T.take 8 s
        _ -> "?"
  let getData = KM.lookup "data" n
  let title = case getData of
        Just (Object d) -> case KM.lookup "title" d of
          Just (String t) -> t
          _ -> "?"
        _ -> "?"
  TIO.putStrLn $ "  [" <> getId <> "] " <> title
showNote _ = pure ()

--------------------------------------------------------------------------------
-- Pref Commands
--------------------------------------------------------------------------------

runPref :: PrefCommand -> IO ()
runPref PrefList = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/api/preferences"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just val -> case val of
      Object obj -> case KM.lookup "preferences" obj of
        Just (Array prefList) -> do
          TIO.putStrLn "Preferences"
          TIO.putStrLn "==========="
          if null prefList
            then TIO.putStrLn "No preferences set."
            else mapM_ showPref (toList prefList)
        _ -> TIO.putStrLn "No preferences found"
      _ -> TIO.putStrLn "Failed to parse preferences"
    _ -> TIO.putStrLn "Failed to fetch preferences"

runPref (PrefSet prefKey prefValue mContext) = do
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/api/preferences"
  let reqBody = object $
        [ "key" .= prefKey, "value" .= prefValue ]
        <> maybe [] (\c -> ["context" .= c]) mContext
  let req = initialReq
        { method = "POST"
        , requestHeaders = [("Content-Type", "application/json")]
        , requestBody = RequestBodyLBS (encode reqBody)
        }
  _ <- httpLbs req manager
  TIO.putStrLn $ "Set preference: " <> prefKey <> " = " <> prefValue

showPref :: Value -> IO ()
showPref (Object p) = do
  let getData = KM.lookup "data" p
  let prefKey = case getData of
        Just (Object d) -> case KM.lookup "key" d of
          Just (String k) -> k
          _ -> "?"
        _ -> "?"
  let prefValue = case getData of
        Just (Object d) -> case KM.lookup "value" d of
          Just (String v) -> v
          _ -> "?"
        _ -> "?"
  TIO.putStrLn $ "  " <> prefKey <> " = " <> prefValue
showPref _ = pure ()
