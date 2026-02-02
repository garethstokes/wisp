module Services.ChatSpec where

import Test.Hspec
import qualified Data.Text as T
import Domain.Chat (ChatContext(..))
import Services.Chat (buildSystemPrompt)

spec :: Spec
spec = describe "Chat" $ do
  describe "buildSystemPrompt" $ do
    it "includes Wisp identity and rules" $ do
      let ctx = ChatContext
            { contextCalendarEvents = []
            , contextRecentActivities = []
            , contextQuarantineCount = 0
            , contextSurfacedCount = 0
            , contextMentionedPeople = []
            }
      let prompt = buildSystemPrompt ctx
      prompt `shouldSatisfy` T.isInfixOf "Wisp"
      prompt `shouldSatisfy` T.isInfixOf "Never say"
      prompt `shouldSatisfy` T.isInfixOf "No events scheduled today"

    it "includes context counts" $ do
      let ctx = ChatContext
            { contextCalendarEvents = []
            , contextRecentActivities = []
            , contextQuarantineCount = 5
            , contextSurfacedCount = 3
            , contextMentionedPeople = []
            }
      let prompt = buildSystemPrompt ctx
      prompt `shouldSatisfy` T.isInfixOf "5 items in quarantine"
      prompt `shouldSatisfy` T.isInfixOf "3 items surfaced"
