module Services.GitHubPoller
  ( pollGitHubForAccount
  , pollAllGitHub
  , buildActivityFromEvent
  , buildCommitActivity
  , buildPushEventParent
  , backfillPushEventDiffs
  , splitExistingPushEvents
  ) where

import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON, Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import App.Monad (App)
import Domain.Id (EntityId)
import Domain.Account (Account(..), AccountProvider(..), accountIdentifier)
import Domain.Activity (NewActivity(..))
import qualified Domain.Activity as Activity
import Infra.Db.Account (getAccountsByProvider)
import Infra.Db.Activity (insertActivity, insertActivityWithParent, activityExistsForAccount, getActivitiesPaginated, updateActivityRaw)
import Infra.Db.PollState (getPollStateForAccount, updatePollStateForAccount, ensurePollStateExists, PollState(..))
import Infra.GitHub.Events (GitHubEvent(..), EventsResponse(..), listEvents, extractEventTitle, CommitInfo(..), extractCommitsFromPayload)
import Data.Time (UTCTime)
import Infra.GitHub.Commits (fetchCommitDiff, fetchCompareDiff, fetchCompareCommits, PushDiff(..), CompareCommit(..))

-- | Build a NewActivity from a GitHubEvent
buildActivityFromEvent :: EntityId -> GitHubEvent -> NewActivity
buildActivityFromEvent accId event = NewActivity
  { newActivityAccountId = accId
  , newActivitySource = Activity.GitHubEvent
  , newActivitySourceId = ghEventId event
  , newActivityRaw = toJSON event
  , newActivityTitle = Just $ extractEventTitle event
  , newActivitySenderEmail = Nothing  -- GitHub uses usernames, not emails
  , newActivityStartsAt = Just $ ghEventCreatedAt event
  , newActivityEndsAt = Nothing
  }

-- | Build a child activity for an individual commit
buildCommitActivity
  :: EntityId      -- ^ Account ID
  -> EntityId      -- ^ Parent PushEvent activity ID
  -> Text          -- ^ Repository name (owner/repo)
  -> CommitInfo    -- ^ Commit info
  -> Maybe Text    -- ^ Diff content (if fetched successfully)
  -> UTCTime       -- ^ Event timestamp
  -> NewActivity
buildCommitActivity accId _parentId repoName commit mDiff eventTime =
  let -- Take first line of commit message for title
      firstLine = T.takeWhile (/= '\n') (commitMessage commit)
      title = "Commit to " <> repoName <> ": " <> T.take 80 firstLine

      -- Build raw JSON with commit details
      rawJson = object
        [ "commit_sha" .= commitSha commit
        , "commit_message" .= commitMessage commit
        , "commit_author" .= commitAuthor commit
        , "commit_url" .= commitUrl commit
        , "repo" .= repoName
        , "diff" .= mDiff
        ]
  in NewActivity
    { newActivityAccountId = accId
    , newActivitySource = Activity.GitHubEvent
    , newActivitySourceId = commitSha commit  -- Use SHA as source_id
    , newActivityRaw = rawJson
    , newActivityTitle = Just title
    , newActivitySenderEmail = Nothing
    , newActivityStartsAt = Just eventTime
    , newActivityEndsAt = Nothing
    }

-- | Build a parent activity for a PushEvent (metadata only, no diff)
-- The payload is stripped of the push_diff field to keep it small
buildPushEventParent :: EntityId -> GitHubEvent -> NewActivity
buildPushEventParent accId event =
  let -- Strip any existing push_diff from payload (keep just metadata)
      strippedPayload = case ghEventPayload event of
        Object obj -> Object $ KM.delete "push_diff" obj
        other -> other
      eventWithoutDiff = event { ghEventPayload = strippedPayload }
  in NewActivity
    { newActivityAccountId = accId
    , newActivitySource = Activity.GitHubEvent
    , newActivitySourceId = ghEventId event
    , newActivityRaw = toJSON eventWithoutDiff
    , newActivityTitle = Just $ extractEventTitle event
    , newActivitySenderEmail = Nothing
    , newActivityStartsAt = Just $ ghEventCreatedAt event
    , newActivityEndsAt = Nothing
    }

-- | Enrich a PushEvent with the diff between before and head commits
-- Uses GitHub's compare API to get the full diff
enrichPushEventWithDiffs :: Text -> GitHubEvent -> IO GitHubEvent
enrichPushEventWithDiffs accessToken event
  | ghEventType event /= "PushEvent" = pure event
  | otherwise = do
      let payload = ghEventPayload event
          repoName = ghEventRepo event  -- format: "owner/repo"
          (owner, repo) = splitRepoName repoName

      -- Extract before and head SHAs from payload
      let (baseSha, headSha) = extractPushShas payload

      -- Fetch the compare diff
      pushDiff <- if T.null baseSha || T.null headSha
        then pure $ PushDiff baseSha headSha Nothing (Just "Missing before/head SHA") 0
        else do
          result <- fetchCompareDiff owner repo baseSha headSha accessToken
          pure $ case result of
            Right diff -> PushDiff baseSha headSha (Just diff) Nothing 1
            Left err -> PushDiff baseSha headSha Nothing (Just err) 0

      -- Add push_diff to the payload
      let enrichedPayload = case payload of
            Object obj -> Object $ KM.insert "push_diff" (toJSON pushDiff) obj
            other -> other

      pure $ event { ghEventPayload = enrichedPayload }

-- | Split "owner/repo" into (owner, repo)
splitRepoName :: Text -> (Text, Text)
splitRepoName name = case T.splitOn "/" name of
  [owner, repo] -> (owner, repo)
  _ -> (name, name)  -- fallback

-- | Extract before and head SHAs from PushEvent payload
extractPushShas :: Value -> (Text, Text)
extractPushShas (Object obj) =
  let baseSha = case KM.lookup "before" obj of
        Just (String s) -> s
        _ -> ""
      headSha = case KM.lookup "head" obj of
        Just (String s) -> s
        _ -> ""
  in (baseSha, headSha)
extractPushShas _ = ("", "")

-- | Get access token from account details
getAccessToken :: Account -> Maybe Text
getAccessToken acc = case accountDetails acc of
  Aeson.Object obj -> case KM.lookup "access_token" obj of
    Just (Aeson.String t) -> Just t
    _ -> Nothing
  _ -> Nothing

-- | Poll GitHub for ALL GitHub accounts
pollAllGitHub :: App [(Text, Either Text [EntityId])]
pollAllGitHub = do
  accounts <- getAccountsByProvider GitHub
  forM accounts $ \acc -> do
    result <- pollGitHubForAccount acc
    let identifier = maybe "unknown" id (accountIdentifier acc)
    pure (identifier, result)

-- | Poll GitHub for a specific account
pollGitHubForAccount :: Account -> App (Either Text [EntityId])
pollGitHubForAccount acc = do
  case (accountIdentifier acc, getAccessToken acc) of
    (Nothing, _) -> pure $ Left "No username in account details"
    (_, Nothing) -> pure $ Left "No access token in account details"
    (Just username, Just token) -> do
      -- Ensure poll state exists
      ensurePollStateExists (accountId acc) "github"
      mPollState <- getPollStateForAccount (accountId acc) "github"
      let mETag = mPollState >>= pollCursor

      -- Fetch events with ETag
      result <- liftIO $ listEvents username token mETag
      case result of
        Left err -> pure $ Left err
        Right response
          | eventsNotModified response -> pure $ Right []  -- Nothing new
          | otherwise -> do
              -- Process events
              newIds <- processEvents (accountId acc) token (eventsData response)
              -- Update ETag in poll state
              updatePollStateForAccount (accountId acc) "github" (eventsETag response)
              pure $ Right newIds

-- | Process a list of events, inserting as activities
processEvents :: EntityId -> Text -> [GitHubEvent] -> App [EntityId]
processEvents accId accessToken events = do
  results <- forM events $ \event ->
    if ghEventType event == "PushEvent"
      then processPushEvent accId accessToken event
      else processOtherEvent accId accessToken event
  pure [aid | Just aid <- results]

-- | Process a PushEvent: create parent activity + child activities for each commit
processPushEvent :: EntityId -> Text -> GitHubEvent -> App (Maybe EntityId)
processPushEvent accId accessToken event = do
  -- Check if we already have this event
  exists <- activityExistsForAccount accId Activity.GitHubEvent (ghEventId event)
  if exists
    then pure Nothing
    else do
      -- 1. Insert parent PushEvent (no diff)
      let parentActivity = buildPushEventParent accId event
      mParentId <- insertActivity parentActivity

      case mParentId of
        Nothing -> pure Nothing  -- duplicate (race condition)
        Just parentId -> do
          -- 2. Extract commits from payload
          let commits = extractCommitsFromPayload (ghEventPayload event)
              (owner, repo) = splitRepoName (ghEventRepo event)

          -- 3. Create child activity for each commit (fetch individual diffs)
          forM_ commits $ \commit -> do
            mDiff <- liftIO $ fetchCommitDiff owner repo (commitSha commit) accessToken
            let diff = either (const Nothing) Just mDiff
                childActivity = buildCommitActivity accId parentId (ghEventRepo event) commit diff (ghEventCreatedAt event)
            void $ insertActivityWithParent childActivity (Just parentId)

          pure (Just parentId)

-- | Process non-PushEvent events (existing logic)
processOtherEvent :: EntityId -> Text -> GitHubEvent -> App (Maybe EntityId)
processOtherEvent accId accessToken event = do
  exists <- activityExistsForAccount accId Activity.GitHubEvent (ghEventId event)
  if exists
    then pure Nothing
    else do
      -- Enrich PushEvents with commit diffs (legacy - won't be called for PushEvents now)
      enrichedEvent <- liftIO $ enrichPushEventWithDiffs accessToken event
      let activity = buildActivityFromEvent accId enrichedEvent
      insertActivity activity

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
              updateActivityRaw (Activity.activityId activity) newRaw
              pure (1 :: Int)
            Nothing -> pure 0
        pure $ sum updated
      _ -> pure 0

  pure $ "Updated " <> T.pack (show totalUpdated) <> " activities"

-- | Check if activity needs backfill
-- Note: Raw JSON uses ghEventPayload (from GitHubEvent ToJSON instance)
needsBackfill :: Activity.Activity -> Bool
needsBackfill a =
  Activity.activitySource a == Activity.GitHubEvent
  && maybe False (T.isPrefixOf "PushEvent") (Activity.activityTitle a)
  && not (hasPushDiff (Activity.activityRaw a))
  where
    hasPushDiff (Object obj) = case KM.lookup "ghEventPayload" obj of
      Just (Object p) -> KM.member "push_diff" p
      _ -> False
    hasPushDiff _ = False

-- | Enrich an activity's raw JSON with diffs
-- Note: Raw JSON uses ghEventRepo/ghEventPayload (from GitHubEvent ToJSON instance)
enrichActivityRaw :: Text -> Activity.Activity -> IO (Maybe Value)
enrichActivityRaw token activity = do
  case Activity.activityRaw activity of
    Object obj -> do
      -- ghEventRepo is a Text like "owner/repo", not an object
      let repoName = case KM.lookup "ghEventRepo" obj of
            Just (String n) -> n
            _ -> ""
          (owner, repo) = splitRepoName repoName
          -- Extract before/head SHAs from payload
          (baseSha, headSha) = case KM.lookup "ghEventPayload" obj of
            Just payload -> extractPushShas payload
            _ -> ("", "")

      if T.null owner || T.null baseSha || T.null headSha
        then pure Nothing
        else do
          result <- fetchCompareDiff owner repo baseSha headSha token
          let pushDiff = case result of
                Right diff -> PushDiff baseSha headSha (Just diff) Nothing 1
                Left err -> PushDiff baseSha headSha Nothing (Just err) 0
          let newPayload = case KM.lookup "ghEventPayload" obj of
                Just (Object p) -> Object $ KM.insert "push_diff" (toJSON pushDiff) p
                _ -> Object KM.empty
          pure $ Just $ Object $ KM.insert "ghEventPayload" newPayload obj
    _ -> pure Nothing

-- | Split existing large PushEvents into parent + child commit activities
-- This backfills existing data to use the new split approach
-- minSizeBytes: only process PushEvents with raw JSON larger than this (e.g., 100000 for 100KB)
splitExistingPushEvents :: Int -> App Text
splitExistingPushEvents minSizeBytes = do
  -- Get GitHub accounts
  accounts <- getAccountsByProvider GitHub

  totalSplit <- fmap sum $ forM accounts $ \acc -> do
    case (accountIdentifier acc, getAccessToken acc) of
      (Just _username, Just token) -> do
        -- Get all activities (up to 1000)
        activities <- getActivitiesPaginated 1000 0
        let largePushEvents = filter (needsSplit minSizeBytes) activities

        -- Process each large PushEvent
        splitCount <- forM largePushEvents $ \activity -> do
          result <- splitPushEventActivity token activity
          case result of
            Left err -> do
              liftIO $ putStrLn $ "Error splitting " <> T.unpack (Activity.activitySourceId activity) <> ": " <> T.unpack err
              pure (0 :: Int)
            Right childCount -> do
              liftIO $ putStrLn $ "Split " <> T.unpack (Activity.activitySourceId activity) <> " into " <> show childCount <> " child activities"
              pure 1
        pure $ sum splitCount
      _ -> pure 0

  pure $ "Split " <> T.pack (show totalSplit) <> " PushEvent activities"

-- | Check if a PushEvent activity needs to be split (is large and has push_diff)
needsSplit :: Int -> Activity.Activity -> Bool
needsSplit minSizeBytes a =
  Activity.activitySource a == Activity.GitHubEvent
  && maybe False (T.isPrefixOf "PushEvent") (Activity.activityTitle a)
  && hasPushDiff (Activity.activityRaw a)
  && rawSize (Activity.activityRaw a) >= minSizeBytes
  where
    hasPushDiff (Object obj) = case KM.lookup "ghEventPayload" obj of
      Just (Object p) -> KM.member "push_diff" p
      _ -> False
    hasPushDiff _ = False

    rawSize :: Value -> Int
    rawSize v = T.length $ T.pack $ show v  -- Rough size estimate

-- | Split a single PushEvent activity into parent + children
splitPushEventActivity :: Text -> Activity.Activity -> App (Either Text Int)
splitPushEventActivity token activity = do
  case Activity.activityRaw activity of
    Object obj -> do
      -- Extract repo and SHAs
      let repoName = case KM.lookup "ghEventRepo" obj of
            Just (String n) -> n
            _ -> ""
          (owner, repo) = splitRepoName repoName
          (baseSha, headSha) = case KM.lookup "ghEventPayload" obj of
            Just payload -> extractPushShas payload
            _ -> ("", "")
          eventTime = Activity.activityCreatedAt activity

      if T.null owner || T.null baseSha || T.null headSha
        then pure $ Left "Missing repo or SHA info"
        else do
          -- Fetch commits from GitHub compare API
          mCommits <- liftIO $ fetchCompareCommits owner repo baseSha headSha token
          case mCommits of
            Left err -> pure $ Left err
            Right commits -> do
              -- Create child activities for each commit
              let accId = Activity.activityAccountId activity
                  parentId = Activity.activityId activity

              forM_ commits $ \commit -> do
                -- Convert CompareCommit to CommitInfo
                let commitInfo = CommitInfo
                      { commitSha = ccSha commit
                      , commitMessage = ccMessage commit
                      , commitAuthor = ccAuthorName commit
                      , commitUrl = ccUrl commit
                      }

                -- Fetch individual diff
                mDiff <- liftIO $ fetchCommitDiff owner repo (ccSha commit) token
                let diff = either (const Nothing) Just mDiff
                    childActivity = buildCommitActivity accId parentId repoName commitInfo diff eventTime

                -- Insert child (skip if already exists)
                void $ insertActivityWithParent childActivity (Just parentId)

              -- Strip push_diff from parent activity
              let strippedPayload = case KM.lookup "ghEventPayload" obj of
                    Just (Object p) -> Object $ KM.delete "push_diff" p
                    Just other -> other
                    Nothing -> Object KM.empty
                  newRaw = Object $ KM.insert "ghEventPayload" strippedPayload obj
              updateActivityRaw parentId newRaw

              pure $ Right (length commits)
    _ -> pure $ Left "Activity raw is not an object"
