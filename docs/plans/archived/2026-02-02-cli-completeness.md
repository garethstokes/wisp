# CLI Completeness Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make wisp usable from the command line by adding commands to view, manage, and interact with classified activities and people.

**Architecture:** Add HTTP endpoints to wisp-srv for querying activities by various filters and managing their status, then add corresponding CLI commands that call these endpoints. The CLI remains a thin HTTP client - all business logic stays in wisp-srv.

**Tech Stack:** Haskell, scotty (HTTP), optparse-applicative (CLI), aeson (JSON), http-client

---

## Task 1: Add `getSurfacedActivities` and `getQuarantinedActivities` DB functions

**Files:**
- Modify: `wisp-srv/src/Infra/Db/Activity.hs`

**Step 1: Add new query functions**

Add to `wisp-srv/src/Infra/Db/Activity.hs` exports:

```haskell
module Infra.Db.Activity
  ( insertActivity
  , activityExists
  , activityExistsForAccount
  , getActivity
  , getActivitiesByStatus
  , getActivitiesForToday       -- NEW
  , updateActivityStatus
  , updateActivityClassification
  ) where
```

**Step 2: Add `getActivitiesForToday` function**

Add after `getActivitiesByStatus`:

```haskell
-- Get activities needing attention today (surfaced + high urgency pending)
getActivitiesForToday :: App [Activity]
getActivitiesForToday = do
  conn <- getConn
  liftIO $ query_ conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at, \
    \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
    \from activities \
    \where status = 'surfaced' \
    \   or (status = 'pending' and urgency = 'high') \
    \   or (status = 'quarantined') \
    \order by \
    \  case when urgency = 'high' then 0 \
    \       when urgency = 'normal' then 1 \
    \       else 2 end, \
    \  created_at desc \
    \limit 50"
```

**Step 3: Run to verify it compiles**

Run: `cd wisp-srv && cabal build 2>&1 | tail -5`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Infra/Db/Activity.hs
git commit -m "feat: add getActivitiesForToday query"
```

---

## Task 2: Add Today HTTP endpoint

**Files:**
- Modify: `wisp-srv/src/Http/Handlers/Activities.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`

**Step 1: Update Activities handler exports**

Update `wisp-srv/src/Http/Handlers/Activities.hs`:

```haskell
module Http.Handlers.Activities
  ( getActivities
  , getActivityById
  , getToday              -- NEW
  , triggerPoll
  ) where
```

**Step 2: Add import**

Add to imports:

```haskell
import Infra.Db.Activity (getActivitiesByStatus, getActivity, getActivitiesForToday)
```

**Step 3: Add `activityToJsonFull` function**

Replace the existing `activityToJson` with a more complete version:

```haskell
-- Convert Activity to JSON (full details)
activityToJson :: Activity -> Value
activityToJson a = object
  [ "id" .= unEntityId (activityId a)
  , "account_id" .= unEntityId (activityAccountId a)
  , "source" .= activitySource a
  , "source_id" .= activitySourceId a
  , "status" .= activityStatus a
  , "title" .= activityTitle a
  , "summary" .= activitySummary a
  , "sender_email" .= activitySenderEmail a
  , "starts_at" .= activityStartsAt a
  , "ends_at" .= activityEndsAt a
  , "created_at" .= activityCreatedAt a
  , "personas" .= activityPersonas a
  , "activity_type" .= activityType a
  , "urgency" .= activityUrgency a
  , "autonomy_tier" .= activityAutonomyTier a
  , "confidence" .= activityConfidence a
  , "person_id" .= fmap unEntityId (activityPersonId a)
  ]
```

**Step 4: Add getToday handler**

Add after `getActivityById`:

```haskell
-- GET /today
getToday :: ActionT (ReaderT Env IO) ()
getToday = do
  activities <- lift getActivitiesForToday
  let surfaced = filter (\a -> activityStatus a == Surfaced) activities
  let quarantined = filter (\a -> activityStatus a == Quarantined) activities
  let pending = filter (\a -> activityStatus a == Pending) activities
  json $ object
    [ "surfaced" .= map activityToJson surfaced
    , "quarantined" .= map activityToJson quarantined
    , "pending_urgent" .= map activityToJson pending
    , "total" .= length activities
    ]
```

**Step 5: Add route**

Add to `wisp-srv/src/Http/Routes.hs`:

```haskell
import Http.Handlers.Activities (getActivities, getActivityById, getToday, triggerPoll)
```

And add the route:

```haskell
  -- Today's view
  get "/today" getToday
```

**Step 6: Run to verify it compiles**

Run: `cd wisp-srv && cabal build 2>&1 | tail -5`
Expected: Build succeeds

**Step 7: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Activities.hs wisp-srv/src/Http/Routes.hs
git commit -m "feat: add /today HTTP endpoint"
```

---

## Task 3: Add Activity Status Management endpoints

**Files:**
- Modify: `wisp-srv/src/Http/Handlers/Activities.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`

**Step 1: Update exports**

```haskell
module Http.Handlers.Activities
  ( getActivities
  , getActivityById
  , getToday
  , postApprove        -- NEW
  , postDismiss        -- NEW
  , triggerPoll
  ) where
```

**Step 2: Add approve handler**

Add after `getToday`:

```haskell
-- POST /activities/:id/approve - Move from quarantined to processed
postApprove :: ActionT (ReaderT Env IO) ()
postApprove = do
  aid <- pathParam "id"
  mActivity <- lift $ getActivity (EntityId aid)
  case mActivity of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Activity not found" :: Text)]
    Just activity -> do
      lift $ updateActivityStatus (activityId activity) Processed
      json $ object
        [ "status" .= ("approved" :: Text)
        , "activity_id" .= aid
        , "new_status" .= ("processed" :: Text)
        ]
```

**Step 3: Add dismiss handler**

Add after `postApprove`:

```haskell
-- POST /activities/:id/dismiss - Archive an activity
postDismiss :: ActionT (ReaderT Env IO) ()
postDismiss = do
  aid <- pathParam "id"
  mActivity <- lift $ getActivity (EntityId aid)
  case mActivity of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Activity not found" :: Text)]
    Just activity -> do
      lift $ updateActivityStatus (activityId activity) Archived
      json $ object
        [ "status" .= ("dismissed" :: Text)
        , "activity_id" .= aid
        , "new_status" .= ("archived" :: Text)
        ]
```

**Step 4: Add import for updateActivityStatus**

```haskell
import Infra.Db.Activity (getActivitiesByStatus, getActivity, getActivitiesForToday, updateActivityStatus)
```

**Step 5: Add routes**

Add to `wisp-srv/src/Http/Routes.hs`:

```haskell
import Http.Handlers.Activities (getActivities, getActivityById, getToday, postApprove, postDismiss, triggerPoll)
```

And add the routes:

```haskell
  -- Activity management
  post "/activities/:id/approve" postApprove
  post "/activities/:id/dismiss" postDismiss
```

**Step 6: Run to verify it compiles**

Run: `cd wisp-srv && cabal build 2>&1 | tail -5`
Expected: Build succeeds

**Step 7: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Activities.hs wisp-srv/src/Http/Routes.hs
git commit -m "feat: add approve and dismiss HTTP endpoints"
```

---

## Task 4: Add People HTTP endpoints

**Files:**
- Create: `wisp-srv/src/Http/Handlers/People.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create People handler**

Create `wisp-srv/src/Http/Handlers/People.hs`:

```haskell
module Http.Handlers.People
  ( getPeople
  , getPersonById
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=), Value)
import Data.Text (Text)
import Network.HTTP.Types.Status (status404)
import Web.Scotty.Trans (ActionT, json, status, pathParam)
import App.Monad (Env)
import Domain.Id (EntityId(..))
import Domain.Person (Person(..))
import Infra.Db.Person (getAllPeople, getPersonById)
import qualified Infra.Db.Person as DbPerson

-- Convert Person to JSON
personToJson :: Person -> Value
personToJson p = object
  [ "id" .= unEntityId (personId p)
  , "email" .= personEmail p
  , "display_name" .= personDisplayName p
  , "personas" .= personPersonas p
  , "relationship" .= personRelationship p
  , "organisation" .= personOrganisation p
  , "notes" .= personNotes p
  , "first_contact" .= personFirstContact p
  , "last_contact" .= personLastContact p
  , "contact_count" .= personContactCount p
  ]

-- GET /people
getPeople :: ActionT (ReaderT Env IO) ()
getPeople = do
  people <- lift getAllPeople
  json $ object
    [ "people" .= map personToJson people
    , "count" .= length people
    ]

-- GET /people/:id
getPersonById :: ActionT (ReaderT Env IO) ()
getPersonById = do
  pid <- pathParam "id"
  mPerson <- lift $ DbPerson.getPersonById (EntityId pid)
  case mPerson of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Person not found" :: Text)]
    Just person -> json $ personToJson person
```

**Step 2: Update cabal file**

Add `Http.Handlers.People` to `other-modules` in both executable and test-suite sections.

**Step 3: Add routes**

Add to `wisp-srv/src/Http/Routes.hs`:

```haskell
import Http.Handlers.People (getPeople, getPersonById)
```

And add the routes:

```haskell
  -- People
  get "/people" getPeople
  get "/people/:id" getPersonById
```

**Step 4: Run to verify it compiles**

Run: `cd wisp-srv && cabal build 2>&1 | tail -5`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add wisp-srv/src/Http/Handlers/People.hs wisp-srv/src/Http/Routes.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add people HTTP endpoints"
```

---

## Task 5: Add Receipts/Logs HTTP endpoint

**Files:**
- Modify: `wisp-srv/src/Http/Handlers/Activities.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`

**Step 1: Add imports**

Add to `wisp-srv/src/Http/Handlers/Activities.hs`:

```haskell
import Domain.Receipt (Receipt(..))
import Infra.Db.Receipt (getReceiptsForActivity)
```

**Step 2: Add receipt JSON helper**

```haskell
-- Convert Receipt to JSON
receiptToJson :: Receipt -> Value
receiptToJson r = object
  [ "id" .= unEntityId (receiptId r)
  , "activity_id" .= unEntityId (receiptActivityId r)
  , "action_taken" .= receiptActionTaken r
  , "action_detail" .= receiptActionDetail r
  , "confidence" .= receiptConfidence r
  , "created_at" .= receiptCreatedAt r
  ]
```

**Step 3: Update exports and add handler**

```haskell
module Http.Handlers.Activities
  ( getActivities
  , getActivityById
  , getActivityLogs    -- NEW
  , getToday
  , postApprove
  , postDismiss
  , triggerPoll
  ) where
```

Add the handler:

```haskell
-- GET /activities/:id/logs
getActivityLogs :: ActionT (ReaderT Env IO) ()
getActivityLogs = do
  aid <- pathParam "id"
  mActivity <- lift $ getActivity (EntityId aid)
  case mActivity of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Activity not found" :: Text)]
    Just activity -> do
      receipts <- lift $ getReceiptsForActivity (activityId activity)
      json $ object
        [ "activity" .= activityToJson activity
        , "logs" .= map receiptToJson receipts
        , "count" .= length receipts
        ]
```

**Step 4: Add route**

Add to `wisp-srv/src/Http/Routes.hs`:

```haskell
import Http.Handlers.Activities (getActivities, getActivityById, getActivityLogs, getToday, postApprove, postDismiss, triggerPoll)
```

And add the route:

```haskell
  get "/activities/:id/logs" getActivityLogs
```

**Step 5: Run to verify it compiles**

Run: `cd wisp-srv && cabal build 2>&1 | tail -5`
Expected: Build succeeds

**Step 6: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Activities.hs wisp-srv/src/Http/Routes.hs
git commit -m "feat: add activity logs HTTP endpoint"
```

---

## Task 6: Add `wisp today` CLI command

**Files:**
- Modify: `wisp-cli/app/Main.hs`

**Step 1: Add Today to Command type**

```haskell
data Command
  = Auth
  | Status
  | Poll
  | Classify
  | Today         -- NEW
  | Help
  deriving (Show)
```

**Step 2: Add to command parser**

```haskell
commandParser :: Parser Command
commandParser = subparser
  ( command "auth" (info (pure Auth) (progDesc "Start OAuth flow"))
  <> command "status" (info (pure Status) (progDesc "Quick status overview"))
  <> command "poll" (info (pure Poll) (progDesc "Trigger a poll cycle"))
  <> command "classify" (info (pure Classify) (progDesc "Run classification pipeline"))
  <> command "today" (info (pure Today) (progDesc "Show items needing attention"))
  <> command "help" (info (pure Help) (progDesc "Show help"))
  )
```

**Step 3: Add to main dispatch**

```haskell
main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Auth -> runAuth
    Status -> runStatus
    Poll -> runPoll
    Classify -> runClassify
    Today -> runToday
    Help -> runHelp
```

**Step 4: Add runToday function**

```haskell
runToday :: IO ()
runToday = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/today"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> do
      TIO.putStrLn "Today's Items"
      TIO.putStrLn "============="
      TIO.putStrLn ""

      -- Show surfaced items
      case KM.lookup "surfaced" obj of
        Just (Array items) | not (null items) -> do
          TIO.putStrLn $ "🔴 NEEDS ATTENTION (" <> showT (length items) <> ")"
          mapM_ showActivityBrief (toList items)
          TIO.putStrLn ""
        _ -> pure ()

      -- Show quarantined items
      case KM.lookup "quarantined" obj of
        Just (Array items) | not (null items) -> do
          TIO.putStrLn $ "🟡 QUARANTINED (" <> showT (length items) <> ")"
          mapM_ showActivityBrief (toList items)
          TIO.putStrLn ""
        _ -> pure ()

      -- Show urgent pending
      case KM.lookup "pending_urgent" obj of
        Just (Array items) | not (null items) -> do
          TIO.putStrLn $ "🟠 URGENT PENDING (" <> showT (length items) <> ")"
          mapM_ showActivityBrief (toList items)
          TIO.putStrLn ""
        _ -> pure ()

      case KM.lookup "total" obj of
        Just (Number n) | n == 0 -> TIO.putStrLn "✅ All clear! Nothing needs your attention."
        _ -> pure ()
    _ -> TIO.putStrLn "Failed to fetch today's items"

showActivityBrief :: Value -> IO ()
showActivityBrief (Object a) = do
  let getId = case KM.lookup "id" a of
        Just (String s) -> s
        _ -> "?"
  let getTitle = case KM.lookup "title" a of
        Just (String s) -> s
        Just Null -> "(no title)"
        _ -> "(no title)"
  let getSummary = case KM.lookup "summary" a of
        Just (String s) -> s
        Just Null -> ""
        _ -> ""
  let getSender = case KM.lookup "sender_email" a of
        Just (String s) -> s
        Just Null -> ""
        _ -> ""
  let getUrgency = case KM.lookup "urgency" a of
        Just (String "high") -> "⚡"
        Just (String "normal") -> ""
        Just (String "low") -> "📉"
        _ -> ""
  TIO.putStrLn $ "  " <> getUrgency <> " [" <> getId <> "] " <> getTitle
  when (getSummary /= "") $ TIO.putStrLn $ "    " <> getSummary
  when (getSender /= "") $ TIO.putStrLn $ "    From: " <> getSender
showActivityBrief _ = pure ()
```

**Step 5: Update help**

```haskell
runHelp :: IO ()
runHelp = do
  TIO.putStrLn "wisp - your autonomy-preserving assistant"
  TIO.putStrLn ""
  TIO.putStrLn "Commands:"
  TIO.putStrLn "  auth      Start OAuth flow with Google"
  TIO.putStrLn "  status    Show server and auth status"
  TIO.putStrLn "  poll      Trigger a poll cycle"
  TIO.putStrLn "  classify  Run classification pipeline"
  TIO.putStrLn "  today     Show items needing attention"
  TIO.putStrLn "  help      Show this help"
  TIO.putStrLn ""
  TIO.putStrLn "Use --help for more details"
```

**Step 6: Add `when` import if not present**

Ensure this import exists:

```haskell
import Control.Monad (when)
```

**Step 7: Run to verify it compiles**

Run: `cd wisp-cli && cabal build 2>&1 | tail -5`
Expected: Build succeeds

**Step 8: Commit**

```bash
git add wisp-cli/app/Main.hs
git commit -m "feat: add wisp today CLI command"
```

---

## Task 7: Add `wisp approve` and `wisp dismiss` CLI commands

**Files:**
- Modify: `wisp-cli/app/Main.hs`

**Step 1: Add to Command type**

```haskell
data Command
  = Auth
  | Status
  | Poll
  | Classify
  | Today
  | Approve Text   -- NEW: takes activity ID
  | Dismiss Text   -- NEW: takes activity ID
  | Help
  deriving (Show)
```

**Step 2: Add to command parser**

```haskell
commandParser :: Parser Command
commandParser = subparser
  ( command "auth" (info (pure Auth) (progDesc "Start OAuth flow"))
  <> command "status" (info (pure Status) (progDesc "Quick status overview"))
  <> command "poll" (info (pure Poll) (progDesc "Trigger a poll cycle"))
  <> command "classify" (info (pure Classify) (progDesc "Run classification pipeline"))
  <> command "today" (info (pure Today) (progDesc "Show items needing attention"))
  <> command "approve" (info (Approve <$> argument str (metavar "ID")) (progDesc "Approve a quarantined item"))
  <> command "dismiss" (info (Dismiss <$> argument str (metavar "ID")) (progDesc "Dismiss/archive an item"))
  <> command "help" (info (pure Help) (progDesc "Show help"))
  )
```

**Step 3: Add to main dispatch**

```haskell
main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Auth -> runAuth
    Status -> runStatus
    Poll -> runPoll
    Classify -> runClassify
    Today -> runToday
    Approve aid -> runApprove aid
    Dismiss aid -> runDismiss aid
    Help -> runHelp
```

**Step 4: Add runApprove function**

```haskell
runApprove :: Text -> IO ()
runApprove actId = do
  TIO.putStrLn $ "Approving activity " <> actId <> "..."
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/activities/" <> unpack actId <> "/approve"
  let req = initialReq { method = "POST" }
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "status" obj of
      Just (String "approved") -> TIO.putStrLn "✅ Activity approved and marked as processed."
      _ -> TIO.putStrLn "Request sent."
    _ -> TIO.putStrLn "Failed to approve activity."
```

**Step 5: Add runDismiss function**

```haskell
runDismiss :: Text -> IO ()
runDismiss actId = do
  TIO.putStrLn $ "Dismissing activity " <> actId <> "..."
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/activities/" <> unpack actId <> "/dismiss"
  let req = initialReq { method = "POST" }
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "status" obj of
      Just (String "dismissed") -> TIO.putStrLn "✅ Activity dismissed and archived."
      _ -> TIO.putStrLn "Request sent."
    _ -> TIO.putStrLn "Failed to dismiss activity."
```

**Step 6: Add imports**

```haskell
import Data.Text (Text, pack, unpack)
```

**Step 7: Update help**

```haskell
runHelp :: IO ()
runHelp = do
  TIO.putStrLn "wisp - your autonomy-preserving assistant"
  TIO.putStrLn ""
  TIO.putStrLn "Commands:"
  TIO.putStrLn "  auth         Start OAuth flow with Google"
  TIO.putStrLn "  status       Show server and auth status"
  TIO.putStrLn "  poll         Trigger a poll cycle"
  TIO.putStrLn "  classify     Run classification pipeline"
  TIO.putStrLn "  today        Show items needing attention"
  TIO.putStrLn "  approve ID   Approve a quarantined item"
  TIO.putStrLn "  dismiss ID   Dismiss/archive an item"
  TIO.putStrLn "  help         Show this help"
  TIO.putStrLn ""
  TIO.putStrLn "Use --help for more details"
```

**Step 8: Run to verify it compiles**

Run: `cd wisp-cli && cabal build 2>&1 | tail -5`
Expected: Build succeeds

**Step 9: Commit**

```bash
git add wisp-cli/app/Main.hs
git commit -m "feat: add wisp approve and dismiss CLI commands"
```

---

## Task 8: Add `wisp people` CLI command

**Files:**
- Modify: `wisp-cli/app/Main.hs`

**Step 1: Add to Command type**

```haskell
data Command
  = Auth
  | Status
  | Poll
  | Classify
  | Today
  | Approve Text
  | Dismiss Text
  | People           -- NEW
  | Help
  deriving (Show)
```

**Step 2: Add to command parser**

```haskell
  <> command "people" (info (pure People) (progDesc "List known contacts"))
```

**Step 3: Add to main dispatch**

```haskell
    People -> runPeople
```

**Step 4: Add runPeople function**

```haskell
runPeople :: IO ()
runPeople = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/people"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> do
      TIO.putStrLn "Known Contacts"
      TIO.putStrLn "=============="
      case KM.lookup "people" obj of
        Just (Array people) | not (null people) -> mapM_ showPersonBrief (toList people)
        _ -> TIO.putStrLn "No contacts yet."
      case KM.lookup "count" obj of
        Just (Number n) -> TIO.putStrLn $ "\nTotal: " <> showT (round n :: Int) <> " contacts"
        _ -> pure ()
    _ -> TIO.putStrLn "Failed to fetch contacts."

showPersonBrief :: Value -> IO ()
showPersonBrief (Object p) = do
  let getId = case KM.lookup "id" p of
        Just (String s) -> s
        _ -> "?"
  let getEmail = case KM.lookup "email" p of
        Just (String s) -> s
        _ -> ""
  let getName = case KM.lookup "display_name" p of
        Just (String s) -> s
        Just Null -> ""
        _ -> ""
  let getCount = case KM.lookup "contact_count" p of
        Just (Number n) -> round n :: Int
        _ -> 0
  let displayName = if getName /= "" then getName <> " <" <> getEmail <> ">" else getEmail
  TIO.putStrLn $ "  [" <> getId <> "] " <> displayName <> " (" <> showT getCount <> " interactions)"
showPersonBrief _ = pure ()
```

**Step 5: Update help**

Add to help:

```haskell
  TIO.putStrLn "  people       List known contacts"
```

**Step 6: Run to verify it compiles**

Run: `cd wisp-cli && cabal build 2>&1 | tail -5`
Expected: Build succeeds

**Step 7: Commit**

```bash
git add wisp-cli/app/Main.hs
git commit -m "feat: add wisp people CLI command"
```

---

## Task 9: Add `wisp activity ID` CLI command

**Files:**
- Modify: `wisp-cli/app/Main.hs`

**Step 1: Add to Command type**

```haskell
data Command
  = Auth
  | Status
  | Poll
  | Classify
  | Today
  | Approve Text
  | Dismiss Text
  | People
  | Activity Text    -- NEW: show single activity details
  | Help
  deriving (Show)
```

**Step 2: Add to command parser**

```haskell
  <> command "activity" (info (Activity <$> argument str (metavar "ID")) (progDesc "Show activity details"))
```

**Step 3: Add to main dispatch**

```haskell
    Activity aid -> runActivity aid
```

**Step 4: Add runActivity function**

```haskell
runActivity :: Text -> IO ()
runActivity actId = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/activities/" <> unpack actId
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object a) -> do
      TIO.putStrLn "Activity Details"
      TIO.putStrLn "================"
      showField "ID" $ KM.lookup "id" a
      showField "Status" $ KM.lookup "status" a
      showField "Source" $ KM.lookup "source" a
      showField "Title" $ KM.lookup "title" a
      showField "Summary" $ KM.lookup "summary" a
      showField "From" $ KM.lookup "sender_email" a
      showField "Type" $ KM.lookup "activity_type" a
      showField "Urgency" $ KM.lookup "urgency" a
      showField "Tier" $ KM.lookup "autonomy_tier" a
      showField "Confidence" $ KM.lookup "confidence" a
      showField "Created" $ KM.lookup "created_at" a
    _ -> TIO.putStrLn "Activity not found."

showField :: Text -> Maybe Value -> IO ()
showField label mVal = case mVal of
  Just (String s) -> TIO.putStrLn $ label <> ": " <> s
  Just (Number n) -> TIO.putStrLn $ label <> ": " <> showT n
  Just Null -> pure ()  -- Don't show null fields
  Nothing -> pure ()
  Just v -> TIO.putStrLn $ label <> ": " <> pack (show v)
```

**Step 5: Update help**

Add to help:

```haskell
  TIO.putStrLn "  activity ID  Show activity details"
```

**Step 6: Run to verify it compiles**

Run: `cd wisp-cli && cabal build 2>&1 | tail -5`
Expected: Build succeeds

**Step 7: Commit**

```bash
git add wisp-cli/app/Main.hs
git commit -m "feat: add wisp activity CLI command"
```

---

## Task 10: Add `wisp logs ID` CLI command

**Files:**
- Modify: `wisp-cli/app/Main.hs`

**Step 1: Add to Command type**

```haskell
data Command
  = Auth
  | Status
  | Poll
  | Classify
  | Today
  | Approve Text
  | Dismiss Text
  | People
  | Activity Text
  | Logs Text        -- NEW: show activity logs/receipts
  | Help
  deriving (Show)
```

**Step 2: Add to command parser**

```haskell
  <> command "logs" (info (Logs <$> argument str (metavar "ID")) (progDesc "Show activity processing logs"))
```

**Step 3: Add to main dispatch**

```haskell
    Logs aid -> runLogs aid
```

**Step 4: Add runLogs function**

```haskell
runLogs :: Text -> IO ()
runLogs actId = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ baseUrl <> "/activities/" <> unpack actId <> "/logs"
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> do
      -- Show activity summary
      case KM.lookup "activity" obj of
        Just (Object a) -> do
          let getTitle = case KM.lookup "title" a of
                Just (String s) -> s
                _ -> "(no title)"
          TIO.putStrLn $ "Logs for: " <> getTitle
          TIO.putStrLn "========================"
        _ -> TIO.putStrLn "Activity Logs"

      -- Show logs
      case KM.lookup "logs" obj of
        Just (Array logs) | not (null logs) -> do
          TIO.putStrLn ""
          mapM_ showLogEntry (toList logs)
        _ -> TIO.putStrLn "\nNo processing logs yet."
    _ -> TIO.putStrLn "Activity not found."

showLogEntry :: Value -> IO ()
showLogEntry (Object l) = do
  let getAction = case KM.lookup "action_taken" l of
        Just (String s) -> s
        _ -> "?"
  let getDetail = case KM.lookup "action_detail" l of
        Just (String s) -> s
        Just Null -> ""
        _ -> ""
  let getTime = case KM.lookup "created_at" l of
        Just (String s) -> s
        _ -> ""
  let getConf = case KM.lookup "confidence" l of
        Just (Number n) -> " (conf: " <> showT n <> ")"
        Just Null -> ""
        _ -> ""
  TIO.putStrLn $ "  [" <> getAction <> "]" <> getConf <> " " <> getTime
  when (getDetail /= "") $ TIO.putStrLn $ "    " <> getDetail
showLogEntry _ = pure ()
```

**Step 5: Update help**

Add to help:

```haskell
  TIO.putStrLn "  logs ID      Show activity processing logs"
```

**Step 6: Run to verify it compiles**

Run: `cd wisp-cli && cabal build 2>&1 | tail -5`
Expected: Build succeeds

**Step 7: Commit**

```bash
git add wisp-cli/app/Main.hs
git commit -m "feat: add wisp logs CLI command"
```

---

## Task 11: Run Full Test Suite

**Step 1: Run server tests**

Run: `cd wisp-srv && cabal test 2>&1 | tail -20`
Expected: All tests pass

**Step 2: Build CLI**

Run: `cd wisp-cli && cabal build 2>&1 | tail -5`
Expected: Build succeeds

**Step 3: Final commit (if any unstaged changes)**

```bash
git status
git add -A
git commit -m "chore: cleanup after CLI completeness phase"
```

---

## Summary

After completing all tasks, you'll have these new commands:

| Command | Description |
|---------|-------------|
| `wisp today` | Show surfaced, quarantined, and urgent items |
| `wisp approve ID` | Approve a quarantined item (moves to processed) |
| `wisp dismiss ID` | Dismiss any item (moves to archived) |
| `wisp people` | List all known contacts |
| `wisp activity ID` | Show full details of an activity |
| `wisp logs ID` | Show processing history for an activity |

And these new HTTP endpoints:

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/today` | GET | Items needing attention |
| `/activities/:id/approve` | POST | Approve activity |
| `/activities/:id/dismiss` | POST | Dismiss activity |
| `/activities/:id/logs` | GET | Activity processing logs |
| `/people` | GET | List all people |
| `/people/:id` | GET | Get person details |
