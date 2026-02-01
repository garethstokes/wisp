-- wisp-cli/app/Main.hs
module Main where

import Data.Aeson (Value, decode)
import qualified Data.Text.IO as TIO
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
import System.Process (callCommand)

-- CLI Command types
data Command
  = Auth
  | Status
  deriving (Show)

-- Parse commands
commandParser :: Parser Command
commandParser = subparser
  ( command "auth" (info (pure Auth) (progDesc "Start OAuth flow"))
  <> command "status" (info (pure Status) (progDesc "Quick status overview"))
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

runAuth :: IO ()
runAuth = do
  TIO.putStrLn "Starting OAuth flow..."
  TIO.putStrLn "Opening browser for Google authentication..."
  let authUrl = baseUrl <> "/auth/google"
  -- Open browser (cross-platform)
  callCommand $ "xdg-open '" <> authUrl <> "' 2>/dev/null || open '" <> authUrl <> "' 2>/dev/null || start '' '" <> authUrl <> "'"
  TIO.putStrLn "\nAfter authenticating, the callback will complete the flow."

runStatus :: IO ()
runStatus = do
  TIO.putStrLn "Wisp Status"
  TIO.putStrLn "==========="
  manager <- newManager tlsManagerSettings
  req <- parseRequest $ baseUrl <> "/health"
  response <- httpLbs req manager
  case decode (responseBody response) :: Maybe Value of
    Just _ -> TIO.putStrLn "Server: online"
    Nothing -> TIO.putStrLn "Server: offline or error"
