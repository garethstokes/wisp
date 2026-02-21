module Services.GitHubPoller
  ( pollGitHubForAccount
  , pollAllGitHub
  , buildActivityFromEvent
  , backfillPushEventDiffs
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON, Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import App.Monad (App)
import Domain.Id (EntityId)
import Domain.Account (Account(..), AccountProvider(..), accountIdentifier)
import Domain.Activity (NewActivity(..))
import qualified Domain.Activity as Activity
import Infra.Db.Account (getAccountsByProvider)
import Infra.Db.Activity (insertActivity, activityExistsForAccount, getActivitiesPaginated, updateActivityRaw)
import Infra.Db.PollState (getPollStateForAccount, updatePollStateForAccount, ensurePollStateExists, PollState(..))
import Infra.GitHub.Events (GitHubEvent(..), EventsResponse(..), listEvents, extractEventTitle)
import Infra.GitHub.Commits (fetchCommitDiff, CommitWithDiff(..))

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
needsBackfill :: Activity.Activity -> Bool
needsBackfill a =
  Activity.activitySource a == Activity.GitHubEvent
  && maybe False (T.isPrefixOf "PushEvent") (Activity.activityTitle a)
  && not (hasCommitsWithDiffs (Activity.activityRaw a))
  where
    hasCommitsWithDiffs (Object obj) = case KM.lookup "payload" obj of
      Just (Object p) -> KM.member "commits_with_diffs" p
      _ -> False
    hasCommitsWithDiffs _ = False

-- | Enrich an activity's raw JSON with diffs
enrichActivityRaw :: Text -> Activity.Activity -> IO (Maybe Value)
enrichActivityRaw token activity = do
  case Activity.activityRaw activity of
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
                _ -> Object KM.empty
          pure $ Just $ Object $ KM.insert "payload" newPayload obj
    _ -> pure Nothing
