module Services.GitHubPollerSpec where

import Test.Hspec
import Services.GitHubPoller (buildActivityFromEvent, buildCommitActivity, buildPushEventParent)
import Infra.GitHub.Events (CommitInfo(..))
import qualified Infra.GitHub.Events as GH
import qualified Domain.Activity as Activity
import Domain.Activity (NewActivity(..))
import Domain.Id (EntityId(..))
import Data.Aeson (object, (.=), Value)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

spec :: Spec
spec = describe "GitHubPoller" $ do
  describe "buildActivityFromEvent" $ do
    it "creates NewActivity from GitHubEvent" $ do
      let testTime = posixSecondsToUTCTime 1704067200
          event = GH.GitHubEvent
            { GH.ghEventId = "12345"
            , GH.ghEventType = "PushEvent"
            , GH.ghEventActor = "garethstokes"
            , GH.ghEventRepo = "org/repo"
            , GH.ghEventPayload = object []
            , GH.ghEventCreatedAt = testTime
            }
          accountId = EntityId "acc123"
          activity = buildActivityFromEvent accountId event

      newActivitySource activity `shouldBe` Activity.GitHubEvent
      newActivitySourceId activity `shouldBe` "12345"
      newActivityTitle activity `shouldBe` Just "PushEvent to org/repo"
      newActivityStartsAt activity `shouldBe` Just testTime

  describe "buildCommitActivity" $ do
    it "creates child activity with parent_id and commit details" $ do
      let parentId = EntityId "parent-push-event-id"
          accountId = EntityId "acc123"
          repoName = "owner/repo"
          commit = CommitInfo
            { commitSha = "abc123def456"
            , commitMessage = "feat: add cool feature\n\nThis adds a cool feature."
            , commitAuthor = "garethstokes"
            , commitUrl = "https://github.com/owner/repo/commit/abc123def456"
            }
          diff = Just "diff --git a/file.hs..."
          testTime = posixSecondsToUTCTime 1704067200

      let activity = buildCommitActivity accountId parentId repoName commit diff testTime

      newActivitySource activity `shouldBe` Activity.GitHubEvent
      newActivitySourceId activity `shouldBe` "abc123def456"
      newActivityTitle activity `shouldBe` Just "Commit to owner/repo: feat: add cool feature"

  describe "buildPushEventParent" $ do
    it "creates parent PushEvent activity without diff" $ do
      let testTime = posixSecondsToUTCTime 1704067200
          payload = object
            [ "before" .= ("abc123" :: String)
            , "head" .= ("def456" :: String)
            , "commits" .= ([] :: [Value])
            ]
          event = GH.GitHubEvent
            { GH.ghEventId = "push-event-12345"
            , GH.ghEventType = "PushEvent"
            , GH.ghEventActor = "garethstokes"
            , GH.ghEventRepo = "owner/repo"
            , GH.ghEventPayload = payload
            , GH.ghEventCreatedAt = testTime
            }
          accountId = EntityId "acc123"

      let activity = buildPushEventParent accountId event

      newActivitySource activity `shouldBe` Activity.GitHubEvent
      newActivitySourceId activity `shouldBe` "push-event-12345"
      newActivityTitle activity `shouldBe` Just "PushEvent to owner/repo"
