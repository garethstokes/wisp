-- src/Services/Scheduler.hs
module Services.Scheduler
  ( startPolling
  , runPollCycle
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import App.Config (Config(..), PollingConfig(..))
import App.Monad (App, Env(..), runApp, getConfig)
import Services.GmailPoller (pollGmail)
import Services.CalendarPoller (pollCalendar)

-- Run a single poll cycle
runPollCycle :: App ()
runPollCycle = do
  liftIO $ putStrLn "\n=== Starting poll cycle ==="

  -- Poll Gmail
  gmailResult <- pollGmail
  case gmailResult of
    Left err -> liftIO $ putStrLn $ "Gmail poll error: " <> show err
    Right count -> liftIO $ putStrLn $ "Gmail: imported " <> show count <> " messages"

  -- Poll Calendar
  calResult <- pollCalendar
  case calResult of
    Left err -> liftIO $ putStrLn $ "Calendar poll error: " <> show err
    Right count -> liftIO $ putStrLn $ "Calendar: imported " <> show count <> " events"

  liftIO $ putStrLn "=== Poll cycle complete ===\n"

-- Start background polling
startPolling :: Env -> IO ()
startPolling env = do
  -- Run initial poll immediately
  putStrLn "Running initial poll..."
  runApp env runPollCycle

  -- Get poll interval from config
  cfg <- runApp env getConfig
  let pollingCfg = polling cfg
      intervalMinutes = pollingCfg.intervalMinutes
      intervalMicros = intervalMinutes * 60 * 1000000

  putStrLn $ "Starting background polling every " <> show intervalMinutes <> " minutes"

  -- Start background polling thread
  pollThread <- async $ forever $ do
    threadDelay intervalMicros
    runApp env runPollCycle

  -- Link thread so exceptions propagate
  link pollThread
