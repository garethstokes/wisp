module Services.Notification
  ( gatherNotifiableItems
  , generateSummary
  , deliverNotification
  , shouldNotify
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)
import System.Process (callCommand)
import Domain.Activity (Activity(..))
import Infra.Db.Notification (getNotifiableActivities, getLastNotificationTime)
import Infra.Claude.Client (callClaudeWithSystem)
import App.Monad (App, Env(..))
import App.Config (Config(..), NotificationConfig(..), ClaudeConfig(..))

-- Gather items that need notification
gatherNotifiableItems :: App [Activity]
gatherNotifiableItems = do
  mNotifyCfg <- asks (notifications . config)
  let vips = maybe [] vipEmails mNotifyCfg
  getNotifiableActivities vips

-- Check if we should send a notification now
shouldNotify :: App Bool
shouldNotify = do
  mNotifyCfg <- asks (notifications . config)
  case mNotifyCfg of
    Nothing -> pure False
    Just cfg | not (enabled cfg) -> pure False
    Just cfg -> do
      items <- gatherNotifiableItems
      if null items
        then pure False
        else do
          mLastTime <- getLastNotificationTime
          now <- liftIO getCurrentTime
          let intervalHours = if length items >= urgentThresholdCount cfg
                              then urgentIntervalHours cfg
                              else defaultIntervalHours cfg
              intervalSeconds = fromIntegral intervalHours * 3600
          case mLastTime of
            Nothing -> pure True  -- Never notified, do it now
            Just lastTime ->
              let elapsed = nominalDiffTimeToSeconds (diffUTCTime now lastTime)
              in pure (elapsed >= intervalSeconds)

-- Generate natural language summary using Claude
generateSummary :: [Activity] -> App (Either Text Text)
generateSummary [] = pure $ Right "Nothing new to report."
generateSummary activities = do
  claudeCfg <- asks (claude . config)
  let itemList = T.unlines $ map formatItem activities
      prompt = "Items waiting:\n" <> itemList
      systemPrompt = T.unlines
        [ "Write a brief, casual observation about what's waiting."
        , "No pressure, no \"you should\" - just friendly info."
        , "Keep it under 3 sentences."
        , "Don't use bullet points. Write conversationally."
        ]
  liftIO $ callClaudeWithSystem
    (apiKey claudeCfg)
    (model claudeCfg)
    systemPrompt
    prompt
  where
    formatItem a = "- " <> fromMaybe "(no title)" (activityTitle a)
      <> maybe "" (\s -> " (from: " <> s <> ")") (activitySenderEmail a)
      <> " [" <> T.pack (show (activityStatus a)) <> "]"

-- Deliver via notify-send
deliverNotification :: Text -> App ()
deliverNotification message = liftIO $ do
  let escaped = T.replace "\"" "\\\"" message
      cmd = "notify-send -t 30000 \"Wisp\" \"" <> T.unpack escaped <> "\""
  callCommand cmd
