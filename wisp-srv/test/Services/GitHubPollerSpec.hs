module Services.GitHubPollerSpec where

import Test.Hspec
import Services.GitHubPoller (buildActivityFromEvent)
import qualified Infra.GitHub.Events as GH
import qualified Domain.Activity as Activity
import Domain.Activity (NewActivity(..))
import Domain.Id (EntityId(..))
import Data.Aeson (object)
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
