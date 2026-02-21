module Services.GitHubPoller
  ( pollGitHubForAccount
  , pollAllGitHub
  , buildActivityFromEvent
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
import Infra.Db.Activity (insertActivity, activityExistsForAccount)
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
              newIds <- processEvents (accountId acc) (eventsData response)
              -- Update ETag in poll state
              updatePollStateForAccount (accountId acc) "github" (eventsETag response)
              pure $ Right newIds

-- | Process a list of events, inserting as activities
processEvents :: EntityId -> [GitHubEvent] -> App [EntityId]
processEvents accId events = do
  results <- forM events $ \event -> do
    exists <- activityExistsForAccount accId Activity.GitHubEvent (ghEventId event)
    if exists
      then pure Nothing
      else do
        let activity = buildActivityFromEvent accId event
        insertActivity activity
  pure [aid | Just aid <- results]
