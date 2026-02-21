# GitHub PushEvent Diffs Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Enrich GitHub PushEvent activities with full commit diffs fetched from GitHub's commit API.

**Architecture:** During polling, when a PushEvent is detected, fetch the diff for each commit via `GET /repos/{owner}/{repo}/commits/{sha}` with diff accept header. Inject a `commits_with_diffs` array into the raw JSON payload before storing. Add a backfill endpoint to enrich existing PushEvents.

**Tech Stack:** Haskell, http-client, aeson, PostgreSQL JSONB

---

### Task 1: Create Commits Module with fetchCommitDiff

**Files:**
- Create: `wisp-srv/src/Infra/GitHub/Commits.hs`
- Modify: `wisp-srv/wisp-srv.cabal` (add module to exposed-modules)

**Step 1: Create the module file**

Create `wisp-srv/src/Infra/GitHub/Commits.hs`:

```haskell
module Infra.GitHub.Commits
  ( fetchCommitDiff
  , CommitWithDiff(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), withObject)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

-- | A commit with its diff content
data CommitWithDiff = CommitWithDiff
  { cwdSha :: Text
  , cwdMessage :: Text
  , cwdAuthor :: Text
  , cwdUrl :: Text
  , cwdDiff :: Maybe Text
  , cwdDiffError :: Maybe Text
  } deriving (Show, Eq)

instance ToJSON CommitWithDiff where
  toJSON c = object
    [ "sha" .= cwdSha c
    , "message" .= cwdMessage c
    , "author" .= cwdAuthor c
    , "url" .= cwdUrl c
    , "diff" .= cwdDiff c
    , "diff_error" .= cwdDiffError c
    ]

instance FromJSON CommitWithDiff where
  parseJSON = withObject "CommitWithDiff" $ \v -> CommitWithDiff
    <$> v .: "sha"
    <*> v .: "message"
    <*> v .: "author"
    <*> v .: "url"
    <*> v .: "diff"
    <*> v .: "diff_error"

-- | Fetch the diff for a single commit
-- Returns the raw diff text or an error message
fetchCommitDiff
  :: Text  -- ^ Repository owner
  -> Text  -- ^ Repository name
  -> Text  -- ^ Commit SHA
  -> Text  -- ^ Access token
  -> IO (Either Text Text)
fetchCommitDiff owner repo sha accessToken = do
  manager <- newManager tlsManagerSettings
  let url = "https://api.github.com/repos/" <> T.unpack owner <> "/" <> T.unpack repo <> "/commits/" <> T.unpack sha
  req <- parseRequest url
  let headers =
        [ ("Authorization", "Bearer " <> TE.encodeUtf8 accessToken)
        , ("User-Agent", "wisp-srv")
        , ("Accept", "application/vnd.github.diff")
        ]
  let authReq = req { requestHeaders = headers }
  response <- httpLbs authReq manager

  let status = statusCode (responseStatus response)
  case status of
    200 -> pure $ Right $ TE.decodeUtf8 $ LBS.toStrict $ responseBody response
    404 -> pure $ Left "404: Commit not found (may have been force-pushed)"
    403 -> pure $ Left "403: Rate limited or access denied"
    _ -> pure $ Left $ "GitHub API error: " <> T.pack (show status)
```

**Step 2: Add module to cabal file**

In `wisp-srv/wisp-srv.cabal`, find the `exposed-modules` section (around line 91-92 where `Infra.GitHub.Auth` and `Infra.GitHub.Events` are listed) and add:

```
        Infra.GitHub.Commits
```

**Step 3: Verify it compiles**

Run: `cabal build wisp-srv`
Expected: Build succeeds with no errors

**Step 4: Commit**

```bash
git add wisp-srv/src/Infra/GitHub/Commits.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(github): add fetchCommitDiff for retrieving commit diffs"
```

---

### Task 2: Add enrichPushEventWithDiffs Function

**Files:**
- Modify: `wisp-srv/src/Services/GitHubPoller.hs`

**Step 1: Add imports and helper function**

At the top of `GitHubPoller.hs`, add to imports:

```haskell
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson (Value(..), Array)
import qualified Data.Vector as V
import Infra.GitHub.Commits (fetchCommitDiff, CommitWithDiff(..))
```

**Step 2: Add the enrichPushEventWithDiffs function**

Add after the `buildActivityFromEvent` function:

```haskell
-- | Enrich a PushEvent with commit diffs
-- Returns the modified event with commits_with_diffs added to payload
enrichPushEventWithDiffs :: Text -> GitHubEvent -> IO GitHubEvent
enrichPushEventWithDiffs accessToken event
  | ghEventType event /= "PushEvent" = pure event
  | otherwise = do
      let payload = ghEventPayload event
          repoName = ghEventRepo event  -- format: "owner/repo"
          (owner, repo) = splitRepoName repoName

      -- Extract commits from payload
      commits <- case payload of
        Object obj -> case KM.lookup "commits" obj of
          Just (Array arr) -> pure $ V.toList arr
          _ -> pure []
        _ -> pure []

      -- Fetch diff for each commit
      commitsWithDiffs <- mapM (fetchDiffForCommit owner repo accessToken) commits

      -- Add commits_with_diffs to the payload
      let enrichedPayload = case payload of
            Object obj -> Object $ KM.insert "commits_with_diffs" (toJSON commitsWithDiffs) obj
            other -> other

      pure $ event { ghEventPayload = enrichedPayload }

-- | Split "owner/repo" into (owner, repo)
splitRepoName :: Text -> (Text, Text)
splitRepoName name = case T.splitOn "/" name of
  [owner, repo] -> (owner, repo)
  _ -> (name, name)  -- fallback

-- | Fetch diff for a single commit value
fetchDiffForCommit :: Text -> Text -> Text -> Value -> IO CommitWithDiff
fetchDiffForCommit owner repo token commitVal = do
  let (sha, message, author) = extractCommitInfo commitVal
      url = "https://github.com/" <> owner <> "/" <> repo <> "/commit/" <> sha

  result <- fetchCommitDiff owner repo sha token
  pure $ case result of
    Right diff -> CommitWithDiff sha message author url (Just diff) Nothing
    Left err -> CommitWithDiff sha message author url Nothing (Just err)

-- | Extract commit info from a commit JSON value
extractCommitInfo :: Value -> (Text, Text, Text)
extractCommitInfo (Object obj) =
  let sha = case KM.lookup "sha" obj of
        Just (String s) -> s
        _ -> ""
      message = case KM.lookup "message" obj of
        Just (String s) -> s
        _ -> ""
      author = case KM.lookup "author" obj of
        Just (Object a) -> case KM.lookup "name" a of
          Just (String n) -> n
          _ -> ""
        _ -> ""
  in (sha, message, author)
extractCommitInfo _ = ("", "", "")
```

**Step 3: Add missing imports at the top**

Add `String` to the `Data.Aeson` import:

```haskell
import Data.Aeson (toJSON, Value(..), Array)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
```

**Step 4: Verify it compiles**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add wisp-srv/src/Services/GitHubPoller.hs
git commit -m "feat(github): add enrichPushEventWithDiffs function"
```

---

### Task 3: Integrate Diff Fetching into Polling Flow

**Files:**
- Modify: `wisp-srv/src/Services/GitHubPoller.hs`

**Step 1: Update processEvents to enrich PushEvents**

Replace the `processEvents` function:

```haskell
-- | Process a list of events, inserting as activities
processEvents :: EntityId -> Text -> [GitHubEvent] -> App [EntityId]
processEvents accId accessToken events = do
  results <- forM events $ \event -> do
    exists <- activityExistsForAccount accId Activity.GitHubEvent (ghEventId event)
    if exists
      then pure Nothing
      else do
        -- Enrich PushEvents with commit diffs
        enrichedEvent <- liftIO $ enrichPushEventWithDiffs accessToken event
        let activity = buildActivityFromEvent accId enrichedEvent
        insertActivity activity
  pure [aid | Just aid <- results]
```

**Step 2: Update pollGitHubForAccount to pass token**

In `pollGitHubForAccount`, change the call to `processEvents` (around line 74):

```haskell
              -- Process events
              newIds <- processEvents (accountId acc) token (eventsData response)
```

**Step 3: Update the export list**

The `processEvents` signature changed, but it's not exported so no changes needed there.

**Step 4: Verify it compiles**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add wisp-srv/src/Services/GitHubPoller.hs
git commit -m "feat(github): integrate diff fetching into polling flow"
```

---

### Task 4: Add Backfill Handler and Endpoint

**Files:**
- Modify: `wisp-srv/src/Http/Handlers/Activities.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`

**Step 1: Add backfill handler to Activities.hs**

Add to imports in `Activities.hs`:

```haskell
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Data.Aeson (Value(..), Array)
import Infra.Db.Account (getAccountsByProvider)
import Domain.Account (Account(..), AccountProvider(..))
import Infra.GitHub.Commits (fetchCommitDiff, CommitWithDiff(..))
import Infra.Db.Activity (updateActivityRaw, getActivitiesPaginated)
```

Add the export:

```haskell
module Http.Handlers.Activities
  ( getActivities
  , getActivityStats
  , getActivityById
  , getActivityLogs
  , getInbox
  , getReview
  , approveActivity
  , dismissActivity
  , triggerPoll
  , backfillGitHubDiffs  -- ADD THIS
  ) where
```

Add the handler function at the end of the file:

```haskell
-- POST /admin/backfill-github-diffs - Backfill diffs for existing PushEvents
backfillGitHubDiffs :: ActionT (ReaderT Env IO) ()
backfillGitHubDiffs = do
  -- Get all GitHub accounts
  accounts <- lift $ getAccountsByProvider GitHub

  results <- forM accounts $ \acc -> do
    case getAccessTokenFromAccount acc of
      Nothing -> pure (0, 0)
      Just token -> do
        -- Get PushEvent activities without diffs
        activities <- lift $ getPushEventsWithoutDiffs
        updated <- forM activities $ \activity -> do
          case enrichActivityWithDiffs token activity of
            Nothing -> pure False
            Just enriched -> do
              lift $ updateActivityRaw (activityId activity) (activityRaw enriched)
              pure True
        pure (length activities, length $ filter id updated)

  let (total, updated) = foldl (\(t, u) (t', u') -> (t + t', u + u')) (0, 0) results
  json $ object
    [ "status" .= ("backfill complete" :: Text)
    , "total_found" .= total
    , "updated" .= updated
    ]

-- Helper: get access token from account
getAccessTokenFromAccount :: Account -> Maybe Text
getAccessTokenFromAccount acc = case accountDetails acc of
  Object obj -> case KM.lookup "access_token" obj of
    Just (String t) -> Just t
    _ -> Nothing
  _ -> Nothing

-- Helper: get PushEvents that don't have commits_with_diffs
getPushEventsWithoutDiffs :: App [Activity]
getPushEventsWithoutDiffs = do
  -- Get GitHub activities
  allActivities <- getActivitiesPaginated 1000 0
  pure $ filter isPushEventWithoutDiffs allActivities
  where
    isPushEventWithoutDiffs a =
      activitySource a == Activity.GitHubEvent
      && maybe False (T.isPrefixOf "PushEvent") (activityTitle a)
      && not (hasCommitsWithDiffs (activityRaw a))

    hasCommitsWithDiffs (Object obj) = KM.member "commits_with_diffs" obj
    hasCommitsWithDiffs _ = False

-- Helper: enrich a single activity with diffs
enrichActivityWithDiffs :: Text -> Activity -> Maybe Activity
enrichActivityWithDiffs token activity = do
  -- This is a simplified version - in practice we'd need IO
  -- For the actual implementation, this should be in IO
  Nothing  -- TODO: implement properly in IO context
```

**Step 2: Actually, let's simplify the backfill**

The backfill is complex because we need IO. Let's make it simpler by creating a dedicated module. Replace the above with a simpler inline approach:

In `Activities.hs`, add this simpler handler:

```haskell
-- POST /admin/backfill-github-diffs - Trigger backfill for existing PushEvents
backfillGitHubDiffs :: ActionT (ReaderT Env IO) ()
backfillGitHubDiffs = do
  result <- lift runBackfill
  json $ object
    [ "status" .= ("backfill triggered" :: Text)
    , "result" .= result
    ]

runBackfill :: App Text
runBackfill = do
  -- Import from Services.GitHubPoller
  -- This will be implemented there
  backfillPushEventDiffs
```

**Step 3: Add route in Routes.hs**

Add import:

```haskell
import Http.Handlers.Activities (getActivities, getActivityStats, getActivityById, getActivityLogs, getInbox, getReview, approveActivity, dismissActivity, triggerPoll, backfillGitHubDiffs)
```

Add route after the other activity routes (around line 60):

```haskell
  -- Admin
  post "/admin/backfill-github-diffs" backfillGitHubDiffs
```

**Step 4: Verify it compiles**

Run: `cabal build wisp-srv`
Expected: May have errors - we need to implement backfillPushEventDiffs

**Step 5: Commit (partial)**

```bash
git add wisp-srv/src/Http/Handlers/Activities.hs wisp-srv/src/Http/Routes.hs
git commit -m "feat(github): add backfill endpoint structure"
```

---

### Task 5: Implement Backfill Logic in GitHubPoller

**Files:**
- Modify: `wisp-srv/src/Services/GitHubPoller.hs`

**Step 1: Add backfillPushEventDiffs to exports**

```haskell
module Services.GitHubPoller
  ( pollGitHubForAccount
  , pollAllGitHub
  , buildActivityFromEvent
  , backfillPushEventDiffs  -- ADD THIS
  ) where
```

**Step 2: Add necessary imports**

```haskell
import Domain.Activity (Activity(..), NewActivity(..))
import Infra.Db.Activity (insertActivity, activityExistsForAccount, getActivitiesPaginated, updateActivityRaw)
```

**Step 3: Add backfill function**

```haskell
-- | Backfill diffs for existing PushEvent activities
backfillPushEventDiffs :: App Text
backfillPushEventDiffs = do
  -- Get GitHub accounts
  accounts <- getAccountsByProvider GitHub

  totalUpdated <- fmap sum $ forM accounts $ \acc -> do
    case (accountIdentifier acc, getAccessToken acc) of
      (Just _username, Just token) -> do
        -- Get all activities (up to 1000)
        activities <- getActivitiesPaginated 1000 0
        let pushEvents = filter needsBackfill activities

        -- Enrich each one
        updated <- forM pushEvents $ \activity -> do
          enriched <- liftIO $ enrichActivityRaw token activity
          case enriched of
            Just newRaw -> do
              updateActivityRaw (activityId activity) newRaw
              pure (1 :: Int)
            Nothing -> pure 0
        pure $ sum updated
      _ -> pure 0

  pure $ "Updated " <> T.pack (show totalUpdated) <> " activities"

-- | Check if activity needs backfill
needsBackfill :: Activity -> Bool
needsBackfill a =
  activitySource a == Activity.GitHubEvent
  && maybe False (T.isPrefixOf "PushEvent") (activityTitle a)
  && not (hasCommitsWithDiffs (activityRaw a))
  where
    hasCommitsWithDiffs (Object obj) = KM.member "commits_with_diffs" obj
    hasCommitsWithDiffs _ = False

-- | Enrich an activity's raw JSON with diffs
enrichActivityRaw :: Text -> Activity -> IO (Maybe Value)
enrichActivityRaw token activity = do
  let raw = activityRaw activity
  case raw of
    Object obj -> do
      let repoName = case KM.lookup "repo" obj of
            Just (Object r) -> case KM.lookup "name" r of
              Just (String n) -> n
              _ -> ""
            _ -> ""
          (owner, repo) = splitRepoName repoName
          commits = case KM.lookup "payload" obj of
            Just (Object p) -> case KM.lookup "commits" p of
              Just (Array arr) -> V.toList arr
              _ -> []
            _ -> []

      if null commits || T.null owner
        then pure Nothing
        else do
          commitsWithDiffs <- mapM (fetchDiffForCommit owner repo token) commits
          let newPayload = case KM.lookup "payload" obj of
                Just (Object p) -> Object $ KM.insert "commits_with_diffs" (toJSON commitsWithDiffs) p
                other -> other
          pure $ Just $ Object $ KM.insert "payload" newPayload obj
    _ -> pure Nothing
```

**Step 4: Verify it compiles**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add wisp-srv/src/Services/GitHubPoller.hs
git commit -m "feat(github): implement backfillPushEventDiffs"
```

---

### Task 6: Fix Remaining Compilation Issues and Test

**Files:**
- Modify: `wisp-srv/src/Http/Handlers/Activities.hs`

**Step 1: Update Activities handler imports**

Add to imports:

```haskell
import Services.GitHubPoller (backfillPushEventDiffs)
```

Remove unused imports we added earlier (the KM, V, etc. that aren't needed in this file).

**Step 2: Simplify backfill handler**

```haskell
-- POST /admin/backfill-github-diffs - Backfill diffs for existing PushEvents
backfillGitHubDiffs :: ActionT (ReaderT Env IO) ()
backfillGitHubDiffs = do
  result <- lift backfillPushEventDiffs
  json $ object
    [ "status" .= ("complete" :: Text)
    , "result" .= result
    ]
```

**Step 3: Build and test**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Manual test**

1. Start the server: `cabal run wisp-srv`
2. Trigger a poll: `curl -X POST http://localhost:5812/poll`
3. Check activities have diffs: `curl http://localhost:5812/activities?limit=5 | jq '.activities[].raw.payload.commits_with_diffs'`
4. Test backfill: `curl -X POST http://localhost:5812/admin/backfill-github-diffs`

**Step 5: Commit**

```bash
git add -A
git commit -m "feat(github): complete PushEvent diff integration"
```

---

### Task 7: Final Cleanup and Documentation

**Files:**
- Modify: `docs/plans/2025-02-22-github-push-event-diffs-design.md`

**Step 1: Update design doc with implementation notes**

Add a "## Implementation Notes" section at the end of the design doc documenting any deviations or learnings.

**Step 2: Final commit**

```bash
git add -A
git commit -m "docs: update design doc with implementation notes"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Create Commits module | `Infra/GitHub/Commits.hs`, `wisp-srv.cabal` |
| 2 | Add enrichPushEventWithDiffs | `Services/GitHubPoller.hs` |
| 3 | Integrate into polling | `Services/GitHubPoller.hs` |
| 4 | Add backfill endpoint | `Http/Handlers/Activities.hs`, `Http/Routes.hs` |
| 5 | Implement backfill logic | `Services/GitHubPoller.hs` |
| 6 | Fix compilation & test | Various |
| 7 | Documentation | Design doc |
