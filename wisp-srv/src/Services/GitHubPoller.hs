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

import App.Monad (App)
import Domain.Id (EntityId)
import Domain.Account (Account(..), AccountProvider(..), accountIdentifier)
import Domain.Activity (NewActivity(..))
import qualified Domain.Activity as Activity
import Infra.Db.Account (getAccountsByProvider)
import Infra.Db.Activity (insertActivity, activityExistsForAccount, getActivitiesPaginated, updateActivityRaw)
import Infra.Db.PollState (getPollStateForAccount, updatePollStateForAccount, ensurePollStateExists, PollState(..))
import Infra.GitHub.Events (GitHubEvent(..), EventsResponse(..), listEvents, extractEventTitle)
import Infra.GitHub.Commits (fetchCompareDiff, PushDiff(..))

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
