module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Monad (forever, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Conc (getNumCapabilities)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, (</>))
import System.Log.FastLogger (pushLogStrLn, toLogStr)
import App.Config (loadConfig, Config(..), ClassificationConfig(..))
import App.Env (buildEnv)
import App.Monad (App, runApp, Env(..), getLogger, getClassificationQueue)
import Domain.Activity (Activity(..), ActivityStatus(..))
import Http.Server (startServer)
import Infra.Db.Activity (getActivitiesByStatus, getActivity)
import Infra.Db.Migrations (runMigrations)
import Services.Scheduler (startPolling)
import Services.ClassificationQueue (enqueueActivities, dequeueActivity)
import Services.Pipeline (processActivity)

main :: IO ()
main = do
  args <- getArgs
  let configPath = case args of
        [p] -> p
        _ -> "wisp.yaml"
      configDir = takeDirectory configPath
      migrationsPath = if null configDir
                       then "migrations"
                       else configDir </> "migrations"

  putStrLn "Loading configuration..."
  config <- loadConfig configPath

  putStrLn "Building environment..."
  env <- buildEnv config

  putStrLn "Running migrations..."
  runApp env $ runMigrations migrationsPath

  -- Initialize classification queue with pending activities
  putStrLn "Loading pending activities into classification queue..."
  runApp env loadPendingIntoQueue

  -- Determine worker count
  cpuCount <- getNumCapabilities
  let configuredWorkers = workerCount (classification config)
      numWorkers = fromMaybe cpuCount configuredWorkers
  putStrLn $ "Starting " <> show numWorkers <> " classification workers..."

  -- Start classification workers
  forM_ [1..numWorkers] $ \workerId -> do
    workerThread <- async $ classificationWorker env workerId
    link workerThread

  -- Start background polling
  putStrLn "Starting background polling..."
  pollingThread <- async $ startPolling env
  link pollingThread

  putStrLn "Starting wisp-srv..."
  runApp env startServer

-- Load all pending activities into the classification queue on startup
loadPendingIntoQueue :: App ()
loadPendingIntoQueue = do
  queue <- getClassificationQueue
  pending <- getActivitiesByStatus Pending 10000
  let activityIds = map activityId pending
  liftIO $ enqueueActivities queue activityIds
  logInfo $ "Loaded " <> T.pack (show (length activityIds)) <> " pending activities into queue"
  where
    logInfo msg = do
      lgr <- getLogger
      liftIO $ pushLogStrLn lgr $ toLogStr $ encodeUtf8 $ "[INFO] " <> msg

-- Classification worker that processes activities from the queue
classificationWorker :: Env -> Int -> IO ()
classificationWorker env workerId = forever $ do
  let queue = classificationQueue env
  -- Block until an activity is available
  aid <- dequeueActivity queue

  -- Process the activity
  runApp env $ do
    mActivity <- getActivity aid
    case mActivity of
      Nothing -> pure ()  -- Activity was deleted, skip
      Just activity -> do
        result <- processActivity activity
        case result of
          Left err -> logWorker workerId $ "Failed " <> T.pack (show aid) <> ": " <> err
          Right status -> logWorker workerId $ "Classified " <> T.pack (show aid) <> " -> " <> T.pack (show status)

  -- Small delay between items to avoid hammering the API
  threadDelay 200000  -- 200ms
  where
    logWorker wid msg = do
      lgr <- getLogger
      liftIO $ pushLogStrLn lgr $ toLogStr $ encodeUtf8 $
        "[INFO] [Worker " <> T.pack (show wid) <> "] " <> msg
