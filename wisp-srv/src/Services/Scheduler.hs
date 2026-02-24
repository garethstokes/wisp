-- src/Services/Scheduler.hs
module Services.Scheduler
  ( startPolling
  , runPollCycle
  , runLibrarianTask
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Monad (forever, forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.Log.FastLogger (pushLogStrLn, toLogStr)
import App.Config (Config(..), PollingConfig(..))
import App.Monad (App, Env(..), runApp, getConfig, getLogger, getClassificationQueue)
import Domain.Id (EntityId)
import Services.GmailPoller (pollAllGmail)
import Services.CalendarPoller (pollAllCalendar)
import Services.GitHubPoller (pollAllGitHub)
import Services.ClassificationQueue (enqueueActivities)
import qualified Skills.Librarian as Librarian

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
  gmailIds <- case gmailResults of
    [] -> do
      logInfo "Gmail: no accounts configured"
      pure []
    _ -> do
      let allIds = concat [ids | (_, Right ids) <- gmailResults]
      logInfo $ "Gmail: imported " <> T.pack (show (length allIds)) <> " messages total"
      -- Log per-account results
      mapM_ logAccountResult $ map (\(e, r) -> ("Gmail", e, fmap length r)) gmailResults
      pure allIds

  -- Poll Calendar for all accounts
  calResults <- pollAllCalendar
  calIds <- case calResults of
    [] -> do
      logInfo "Calendar: no accounts configured"
      pure []
    _ -> do
      let allIds = concat [ids | (_, Right ids) <- calResults]
      logInfo $ "Calendar: imported " <> T.pack (show (length allIds)) <> " events total"
      -- Log per-account results
      mapM_ logAccountResult $ map (\(e, r) -> ("Calendar", e, fmap length r)) calResults
      pure allIds

  -- Poll GitHub for all accounts
  githubResults <- pollAllGitHub
  githubIds <- case githubResults of
    [] -> do
      logInfo "GitHub: no accounts configured"
      pure []
    _ -> do
      let allIds = concat [ids | (_, Right ids) <- githubResults]
      logInfo $ "GitHub: imported " <> T.pack (show (length allIds)) <> " events total"
      mapM_ logAccountResult $ map (\(u, r) -> ("GitHub", u, fmap length r)) githubResults
      pure allIds

  -- Enqueue newly imported activities for classification
  let newIds = gmailIds ++ calIds ++ githubIds
  enqueueForClassification newIds

  logInfo "Poll cycle complete"

-- Enqueue specific activity IDs for classification
enqueueForClassification :: [EntityId] -> App ()
enqueueForClassification [] = pure ()
enqueueForClassification ids = do
  queue <- getClassificationQueue
  liftIO $ enqueueActivities queue ids
  logInfo $ "Enqueued " <> T.pack (show (length ids)) <> " activities for classification"

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

-- | Run the Librarian skill to maintain project knowledge
runLibrarianTask :: App ()
runLibrarianTask = do
  logInfo "Starting Librarian task"
  results <- Librarian.runLibrarian
  forM_ results $ \r -> do
    let projectName = Librarian.lrProjectName r
        updatedCount = length (Librarian.lrUpdatedDocs r)
        skippedCount = length (Librarian.lrSkippedDocs r)
    logInfo $ "  Librarian [" <> projectName <> "]: "
           <> T.pack (show updatedCount) <> " docs updated, "
           <> T.pack (show skippedCount) <> " skipped"
  logInfo $ "Librarian task complete: processed " <> T.pack (show (length results)) <> " projects"
