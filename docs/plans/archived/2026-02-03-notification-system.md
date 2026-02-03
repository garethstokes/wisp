# Notification System Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement a PDA-friendly batch notification system that periodically summarizes pending items via desktop notifications.

**Architecture:** Background loop checks every 15 minutes, sends LLM-generated summaries at adaptive intervals (4hr default, 2hr when urgent). Uses notify-send for desktop delivery.

**Tech Stack:** Haskell, PostgreSQL, Claude API, notify-send

---

## Task 1: Add NotificationConfig to App.Config

**Files:**
- Modify: `wisp-srv/src/App/Config.hs`

**Step 1: Write failing test**

```haskell
-- wisp-srv/test/App/ConfigSpec.hs (add to existing)

    it "parses notification config" $ do
      let yaml = [r|
notifications:
  enabled: true
  default_interval_hours: 4
  urgent_interval_hours: 2
  urgent_threshold_count: 3
  quiet_hours_start: "22:00"
  quiet_hours_end: "08:00"
  vip_emails:
    - "vip@example.com"
|]
      -- This will fail until we add the type
      pendingWith "NotificationConfig type not yet defined"
```

**Step 2: Add NotificationConfig type to App/Config.hs**

Add after `ClaudeConfig`:

```haskell
data NotificationConfig = NotificationConfig
  { enabled :: Bool
  , defaultIntervalHours :: Int
  , urgentIntervalHours :: Int
  , urgentThresholdCount :: Int
  , quietHoursStart :: Text
  , quietHoursEnd :: Text
  , vipEmails :: [Text]
  } deriving (Generic, Show)

instance FromJSON NotificationConfig where
  parseJSON = withObject "NotificationConfig" $ \v -> NotificationConfig
    <$> v .:? "enabled" .!= False
    <*> v .:? "default_interval_hours" .!= 4
    <*> v .:? "urgent_interval_hours" .!= 2
    <*> v .:? "urgent_threshold_count" .!= 3
    <*> v .:? "quiet_hours_start" .!= "22:00"
    <*> v .:? "quiet_hours_end" .!= "08:00"
    <*> v .:? "vip_emails" .!= []
```

**Step 3: Add to Config record and exports**

Update exports:
```haskell
module App.Config
  ( Config(..)
  , ServerConfig(..)
  , DatabaseConfig(..)
  , GoogleConfig(..)
  , PollingConfig(..)
  , ClassificationConfig(..)
  , ClaudeConfig(..)
  , NotificationConfig(..)  -- ADD
  , loadConfig
  ) where
```

Update imports (add):
```haskell
import Data.Aeson (FromJSON, withObject, (.:), (.:?), (.!=))
```

Update Config record:
```haskell
data Config = Config
  { server :: ServerConfig
  , database :: DatabaseConfig
  , google :: GoogleConfig
  , polling :: PollingConfig
  , classification :: ClassificationConfig
  , claude :: ClaudeConfig
  , notifications :: Maybe NotificationConfig  -- ADD (Maybe for backwards compat)
  } deriving (Generic, Show)
```

**Step 4: Run build to verify**

Run: `cabal build wisp-srv`
Expected: Success (no errors)

**Step 5: Add to wisp.yaml example**

Add to `wisp-srv/wisp.yaml`:
```yaml
notifications:
  enabled: true
  default_interval_hours: 4
  urgent_interval_hours: 2
  urgent_threshold_count: 3
  quiet_hours_start: "22:00"
  quiet_hours_end: "08:00"
  vip_emails: []
```

**Step 6: Commit**

```bash
git add wisp-srv/src/App/Config.hs wisp-srv/wisp.yaml
git commit -m "feat: add NotificationConfig to config system"
```

---

## Task 2: Add notified_at column to activities

**Files:**
- Create: `wisp-srv/migrations/007_add_notified_at.sql`

**Step 1: Create migration file**

```sql
-- 007_add_notified_at.sql
ALTER TABLE activities ADD COLUMN IF NOT EXISTS notified_at TIMESTAMPTZ;
```

**Step 2: Run migration to verify syntax**

Run: `cabal run wisp-srv -- wisp-srv/wisp.yaml` (starts server, runs migrations)
Expected: Migration applies successfully, then Ctrl+C

**Step 3: Verify column exists**

Run: `psql $DATABASE_URL -c "\d activities" | grep notified_at`
Expected: Shows `notified_at | timestamp with time zone`

**Step 4: Commit**

```bash
git add wisp-srv/migrations/007_add_notified_at.sql
git commit -m "feat: add notified_at column to activities"
```

---

## Task 3: Create notification_state table

**Files:**
- Create: `wisp-srv/migrations/008_notification_state.sql`

**Step 1: Create migration file**

```sql
-- 008_notification_state.sql
CREATE TABLE IF NOT EXISTS notification_state (
  id TEXT PRIMARY KEY DEFAULT 'singleton',
  last_notification_at TIMESTAMPTZ
);

-- Insert initial row
INSERT INTO notification_state (id, last_notification_at)
VALUES ('singleton', NULL)
ON CONFLICT (id) DO NOTHING;
```

**Step 2: Run migration**

Run: `cabal run wisp-srv -- wisp-srv/wisp.yaml`
Expected: Migration applies, then Ctrl+C

**Step 3: Verify table**

Run: `psql $DATABASE_URL -c "SELECT * FROM notification_state;"`
Expected: Shows singleton row with NULL timestamp

**Step 4: Commit**

```bash
git add wisp-srv/migrations/008_notification_state.sql
git commit -m "feat: add notification_state table"
```

---

## Task 4: Create Infra.Db.Notification module

**Files:**
- Create: `wisp-srv/src/Infra/Db/Notification.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create the module**

```haskell
module Infra.Db.Notification
  ( getLastNotificationTime
  , updateLastNotificationTime
  , getNotifiableActivities
  , markActivitiesNotified
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Domain.Activity (Activity(..), ActivityStatus(..))
import Domain.Id (EntityId(..))
import App.Monad (App, getConn)
import Infra.Db.Activity (DbActivity(..))

-- Get last notification timestamp
getLastNotificationTime :: App (Maybe UTCTime)
getLastNotificationTime = do
  conn <- getConn
  results <- liftIO $ query_ conn
    "SELECT last_notification_at FROM notification_state WHERE id = 'singleton'"
  pure $ case results of
    [Only t] -> t
    _ -> Nothing

-- Update last notification timestamp
updateLastNotificationTime :: UTCTime -> App ()
updateLastNotificationTime t = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "UPDATE notification_state SET last_notification_at = ? WHERE id = 'singleton'"
    (Only t)
  pure ()

-- Get activities that should be notified:
-- - Status = Surfaced OR urgency = 'high' OR sender in VIP list
-- - AND notified_at IS NULL
getNotifiableActivities :: [Text] -> App [Activity]
getNotifiableActivities vipEmails = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \FROM activities \
    \WHERE notified_at IS NULL \
    \  AND (status = 'surfaced' \
    \       OR urgency = 'high' \
    \       OR sender_email = ANY(?)) \
    \ORDER BY created_at DESC \
    \LIMIT 100"
    (Only (PGArray vipEmails))
  pure $ map unDbActivity results

-- Mark activities as notified
markActivitiesNotified :: [EntityId] -> App ()
markActivitiesNotified [] = pure ()
markActivitiesNotified ids = do
  conn <- getConn
  let idTexts = map unEntityId ids
  _ <- liftIO $ execute conn
    "UPDATE activities SET notified_at = NOW() WHERE id = ANY(?)"
    (Only (PGArray idTexts))
  pure ()
```

**Step 2: Export DbActivity from Infra.Db.Activity**

Modify `wisp-srv/src/Infra/Db/Activity.hs` exports:
```haskell
module Infra.Db.Activity
  ( insertActivity
  , insertConversation
  , activityExists
  , activityExistsForAccount
  , getActivity
  , getActivitiesByStatus
  , countActivitiesByStatus
  , getActivitiesForToday
  , getRecentActivities
  , getTodaysCalendarEvents
  , getPendingEmails
  , updateActivityStatus
  , updateActivityClassification
  , DbActivity(..)  -- ADD for Notification module
  ) where
```

**Step 3: Add to cabal file**

Add to `other-modules` in wisp-srv.cabal (after `Infra.Db.Migrations`):
```
        Infra.Db.Notification
```

**Step 4: Add PGArray import to Notification module**

Already included in the code above:
```haskell
import Database.PostgreSQL.Simple.Types (PGArray(..))
```

Wait - we need to add that import. Update the module:
```haskell
import Database.PostgreSQL.Simple.Types (PGArray(..))
```

**Step 5: Build to verify**

Run: `cabal build wisp-srv`
Expected: Success

**Step 6: Commit**

```bash
git add wisp-srv/src/Infra/Db/Notification.hs wisp-srv/src/Infra/Db/Activity.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Infra.Db.Notification module"
```

---

## Task 5: Create Services.Notification module

**Files:**
- Create: `wisp-srv/src/Services/Notification.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create the module**

```haskell
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
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)
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
```

**Step 2: Add to cabal file**

Add to `other-modules` (after `Services.Chat`):
```
        Services.Notification
```

**Step 3: Build to verify**

Run: `cabal build wisp-srv`
Expected: Success

**Step 4: Commit**

```bash
git add wisp-srv/src/Services/Notification.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Services.Notification module"
```

---

## Task 6: Create Services.NotificationLoop module

**Files:**
- Create: `wisp-srv/src/Services/NotificationLoop.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create the module**

```haskell
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
```

**Step 2: Add to cabal file**

Add to `other-modules`:
```
        Services.NotificationLoop
```

**Step 3: Build to verify**

Run: `cabal build wisp-srv`
Expected: Success

**Step 4: Commit**

```bash
git add wisp-srv/src/Services/NotificationLoop.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Services.NotificationLoop module"
```

---

## Task 7: Wire up notification loop in Main.hs

**Files:**
- Modify: `wisp-srv/app/Main.hs`

**Step 1: Add imports**

Add to imports:
```haskell
import App.Config (loadConfig, Config(..), ClassificationConfig(..), NotificationConfig(..))
import Services.NotificationLoop (startNotificationLoop)
import Data.Maybe (fromMaybe, isJust)
```

**Step 2: Add notification loop startup**

After the polling thread startup, add:

```haskell
  -- Start notification loop if enabled
  case notifications config of
    Just notifyCfg | enabled notifyCfg -> do
      putStrLn "Starting notification loop..."
      notifyThread <- async $ startNotificationLoop env
      link notifyThread
    _ -> putStrLn "Notifications disabled"
```

**Step 3: Build and test startup**

Run: `cabal build wisp-srv && cabal run wisp-srv -- wisp-srv/wisp.yaml`
Expected: See "Starting notification loop..." in output

**Step 4: Commit**

```bash
git add wisp-srv/app/Main.hs
git commit -m "feat: wire notification loop into server startup"
```

---

## Task 8: Add notification module to test suite

**Files:**
- Modify: `wisp-srv/wisp-srv.cabal` (test section)
- Create: `wisp-srv/test/Services/NotificationSpec.hs`

**Step 1: Add module to cabal test other-modules**

In the test-suite section, add:
```
        Services.Notification
        Services.NotificationLoop
        Infra.Db.Notification
```

**Step 2: Create basic spec**

```haskell
module Services.NotificationSpec where

import Test.Hspec
import qualified Data.Text as T
import Domain.Activity (Activity(..), ActivitySource(..), ActivityStatus(..))
import Domain.Id (EntityId(..))
import Data.Time (getCurrentTime)
import Data.Aeson (object)

spec :: Spec
spec = describe "Notification" $ do
  describe "formatItem" $ do
    it "formats activity with title and sender" $ do
      -- This is a unit test placeholder
      -- Real integration tests would require database
      pendingWith "Integration test - requires database"
```

**Step 3: Run tests**

Run: `cabal test wisp-srv-test`
Expected: All tests pass (with pending notification test)

**Step 4: Commit**

```bash
git add wisp-srv/wisp-srv.cabal wisp-srv/test/Services/NotificationSpec.hs
git commit -m "test: add notification module to test suite"
```

---

## Task 9: Manual integration test

**Step 1: Ensure config has notifications enabled**

Verify `wisp-srv/wisp.yaml` has:
```yaml
notifications:
  enabled: true
  default_interval_hours: 4
  urgent_interval_hours: 2
  urgent_threshold_count: 3
  quiet_hours_start: "22:00"
  quiet_hours_end: "08:00"
  vip_emails: []
```

**Step 2: Start server**

Run: `cabal run wisp-srv -- wisp-srv/wisp.yaml`

**Step 3: Verify notification loop is running**

Look for: "Starting notification loop..." in output

**Step 4: Trigger a test notification (optional)**

If you have surfaced activities, wait 15 minutes or temporarily modify the loop delay for testing.

Alternatively, add a CLI command (future task) or manually call the notification functions via REPL.

**Step 5: Verify notify-send works**

Run: `notify-send "Test" "Hello from wisp"`
Expected: Desktop notification appears

---

## Summary

After completing all tasks:

1. ✅ NotificationConfig added to config system
2. ✅ notified_at column added to activities
3. ✅ notification_state table created
4. ✅ Infra.Db.Notification module for database queries
5. ✅ Services.Notification for gathering items, generating summaries, delivering
6. ✅ Services.NotificationLoop for background processing
7. ✅ Main.hs wired up to start the loop
8. ✅ Test suite updated

The notification system will:
- Check every 15 minutes
- Send notifications at adaptive intervals (4hr default, 2hr if urgent)
- Use Claude to generate natural, PDA-friendly summaries
- Deliver via notify-send
- Track what's been notified to avoid repeats
