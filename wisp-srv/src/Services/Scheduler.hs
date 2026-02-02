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
import App.Monad (App, Env(..), runApp, getConfig, getLogger, getClassificationQueue)
import Domain.Activity (Activity(..), ActivityStatus(..))
import Services.GmailPoller (pollAllGmail)
import Services.CalendarPoller (pollAllCalendar)
import Services.ClassificationQueue (enqueueActivities)
import Infra.Db.Activity (getActivitiesByStatus)

-- Helper to log within App monad
logInfo :: T.Text -> App ()
logInfo msg = do
  loggerSet <- getLogger
  liftIO $ pushLogStrLn loggerSet $ toLogStr $ encodeUtf8 $ "[INFO] " <> msg

-- Run a single poll cycle (polls all accounts)
runPollCycle :: App ()
runPollCycle = do
  logInfo "Starting poll cycle"

  -- Poll Gmail for all accounts
  gmailResults <- pollAllGmail
  case gmailResults of
    [] -> logInfo "Gmail: no accounts configured"
    _ -> do
      let totalMsgs = sum [n | (_, Right n) <- gmailResults]
      logInfo $ "Gmail: imported " <> T.pack (show totalMsgs) <> " messages total"
      -- Log per-account results
      mapM_ logAccountResult $ map (\(e, r) -> ("Gmail", e, r)) gmailResults

  -- Poll Calendar for all accounts
  calResults <- pollAllCalendar
  case calResults of
    [] -> logInfo "Calendar: no accounts configured"
    _ -> do
      let totalEvents = sum [n | (_, Right n) <- calResults]
      logInfo $ "Calendar: imported " <> T.pack (show totalEvents) <> " events total"
      -- Log per-account results
      mapM_ logAccountResult $ map (\(e, r) -> ("Calendar", e, r)) calResults

  -- Enqueue pending activities for classification workers
  enqueueCount <- enqueuePendingActivities
  case enqueueCount of
    0 -> pure ()
    n -> logInfo $ "Enqueued " <> T.pack (show n) <> " activities for classification"

  logInfo "Poll cycle complete"

-- Enqueue all pending activities for classification
enqueuePendingActivities :: App Int
enqueuePendingActivities = do
  queue <- getClassificationQueue
  -- Get all pending activities (use a large limit to get them all)
  pending <- getActivitiesByStatus Pending 10000
  let activityIds = map activityId pending
  liftIO $ enqueueActivities queue activityIds
  pure $ length activityIds

-- Log result for a single account
logAccountResult :: (T.Text, T.Text, Either T.Text Int) -> App ()
logAccountResult (service, email, result) = case result of
  Left err -> logInfo $ "  " <> service <> " [" <> email <> "]: error - " <> err
  Right 0 -> pure ()  -- Don't log zero imports to reduce noise
  Right n -> logInfo $ "  " <> service <> " [" <> email <> "]: " <> T.pack (show n) <> " new items"

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
