-- src/Services/Scheduler.hs
module Services.Scheduler
  ( startPolling
  , runPollCycle
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.Log.FastLogger (pushLogStrLn, toLogStr)
import App.Config (Config(..), PollingConfig(..))
import App.Monad (App, Env(..), runApp, getConfig, getLogger)
import Services.GmailPoller (pollGmail)
import Services.CalendarPoller (pollCalendar)

-- Helper to log within App monad
logInfo :: T.Text -> App ()
logInfo msg = do
  loggerSet <- getLogger
  liftIO $ pushLogStrLn loggerSet $ toLogStr $ encodeUtf8 $ "[INFO] " <> msg

-- Run a single poll cycle
runPollCycle :: App ()
runPollCycle = do
  logInfo "Starting poll cycle"

  -- Poll Gmail
  gmailResult <- pollGmail
  case gmailResult of
    Left err -> logInfo $ "Gmail poll error: " <> err
    Right count -> logInfo $ "Gmail: imported " <> T.pack (show count) <> " messages"

  -- Poll Calendar
  calResult <- pollCalendar
  case calResult of
    Left err -> logInfo $ "Calendar poll error: " <> err
    Right count -> logInfo $ "Calendar: imported " <> T.pack (show count) <> " events"

  logInfo "Poll cycle complete"

-- Start background polling
startPolling :: Env -> IO ()
startPolling env = do
  -- Run initial poll immediately
  runApp env $ logInfo "Running initial poll..."
  runApp env runPollCycle

  -- Get poll interval from config
  cfg <- runApp env getConfig
  let pollingCfg = polling cfg
      intervalMinutes = pollingCfg.intervalMinutes
      intervalMicros = intervalMinutes * 60 * 1000000

  runApp env $ logInfo $ "Starting background polling every " <> T.pack (show intervalMinutes) <> " minutes"

  -- Start background polling thread
  pollThread <- async $ forever $ do
    threadDelay intervalMicros
    runApp env runPollCycle

  -- Link thread so exceptions propagate
  link pollThread
