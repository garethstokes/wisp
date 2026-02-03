module Services.NotificationLoop
  ( startNotificationLoop
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.Log.FastLogger (pushLogStrLn, toLogStr)
import Domain.Activity (activityId)
import Infra.Db.Notification (updateLastNotificationTime, markActivitiesNotified)
import Services.Notification (gatherNotifiableItems, shouldNotify, generateSummary, deliverNotification)
import App.Monad (App, runApp, Env(..), getLogger)

-- Run the notification loop (checks every 15 minutes)
startNotificationLoop :: Env -> IO ()
startNotificationLoop env = forever $ do
  runApp env notificationCycle
  -- Sleep 15 minutes
  threadDelay (15 * 60 * 1000000)

-- Single notification cycle
notificationCycle :: App ()
notificationCycle = do
  shouldSend <- shouldNotify
  when shouldSend $ do
    items <- gatherNotifiableItems
    if null items
      then pure ()
      else do
        logInfo $ "Generating notification for " <> T.pack (show (length items)) <> " items"
        result <- generateSummary items
        case result of
          Left err -> logInfo $ "Failed to generate summary: " <> err
          Right summary -> do
            logInfo $ "Sending notification: " <> summary
            deliverNotification summary
            -- Mark items as notified
            let ids = map activityId items
            markActivitiesNotified ids
            -- Update last notification time
            now <- liftIO getCurrentTime
            updateLastNotificationTime now
            logInfo "Notification sent and items marked"
  where
    logInfo msg = do
      lgr <- getLogger
      liftIO $ pushLogStrLn lgr $ toLogStr $ encodeUtf8 $ "[NOTIFY] " <> msg
