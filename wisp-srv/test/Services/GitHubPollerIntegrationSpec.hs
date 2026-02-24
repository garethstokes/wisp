module Services.GitHubPollerIntegrationSpec where

import Test.Hspec

-- Note: These imports will be needed when the test is fully implemented:
-- import TestEnv (withTestEnv)
-- import Services.GitHubPoller (processEvents)  -- not currently exported
-- import Infra.GitHub.Events (GitHubEvent(..))
-- import Infra.Db.Activity (getActivity)
-- import Domain.Activity (Activity(..), activityParentId)
-- import Domain.Id (EntityId(..))
-- import Data.Aeson (object, (.=))
-- import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

spec :: Spec
spec = describe "GitHubPoller Integration" $ do
  describe "processEvents with PushEvent" $ do
    it "creates parent activity and child activities for each commit" $ do
      -- Note: This test will fail at HTTP level since we don't have a real token
      -- In real integration test, we'd mock the HTTP layer or use fixtures
      -- For now, this documents the expected behavior
      --
      -- Test setup would look like:
      --   let testTime = posixSecondsToUTCTime 1704067200
      --       payload = object
      --         [ "before" .= ("abc123" :: String)
      --         , "head" .= ("def789" :: String)
      --         , "commits" .=
      --           [ object
      --             [ "sha" .= ("commit1sha" :: String)
      --             , "message" .= ("feat: first commit" :: String)
      --             , "author" .= object ["name" .= ("Dev" :: String)]
      --             , "url" .= ("https://github.com/o/r/commit/commit1sha" :: String)
      --             ]
      --           , object
      --             [ "sha" .= ("commit2sha" :: String)
      --             , "message" .= ("fix: second commit" :: String)
      --             , "author" .= object ["name" .= ("Dev" :: String)]
      --             , "url" .= ("https://github.com/o/r/commit/commit2sha" :: String)
      --             ]
      --           ]
      --         ]
      --       event = GitHubEvent
      --         { ghEventId = "push-12345"
      --         , ghEventType = "PushEvent"
      --         , ghEventActor = "dev"
      --         , ghEventRepo = "owner/repo"
      --         , ghEventPayload = payload
      --         , ghEventCreatedAt = testTime
      --         }
      --       accountId = EntityId "test-account"
      --
      -- Expected behavior:
      -- 1. Call processEvents with the event
      -- 2. Verify parent PushEvent activity created
      -- 3. Verify child activities created for each commit
      -- 4. Verify child activities have parent_id set
      pendingWith "requires HTTP mocking for fetchCommitDiff"
