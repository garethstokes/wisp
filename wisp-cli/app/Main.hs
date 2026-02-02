-- wisp-cli/app/Main.hs
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Aeson (Value(..), decode)
import Data.Foldable (toList)
import Data.Text (Text, pack)
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
  | Help
  deriving (Show)

-- Parse commands
commandParser :: Parser Command
commandParser = subparser
  ( command "auth" (info (pure Auth) (progDesc "Start OAuth flow"))
  <> command "status" (info (pure Status) (progDesc "Quick status overview"))
  <> command "poll" (info (pure Poll) (progDesc "Trigger a poll cycle"))
  <> command "help" (info (pure Help) (progDesc "Show help"))
  )

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
    Help -> runHelp

runHelp :: IO ()
runHelp = do
  TIO.putStrLn "wisp - your autonomy-preserving assistant"
  TIO.putStrLn ""
  TIO.putStrLn "Commands:"
  TIO.putStrLn "  auth    Start OAuth flow with Google"
  TIO.putStrLn "  status  Show server and auth status"
  TIO.putStrLn "  poll    Trigger a poll cycle"
  TIO.putStrLn "  help    Show this help"
  TIO.putStrLn ""
  TIO.putStrLn "Use --help for more details"

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
