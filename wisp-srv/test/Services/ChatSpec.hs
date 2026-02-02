module Services.ChatSpec where

import Test.Hspec
import qualified Data.Text as T
import Domain.Chat (ChatContext(..))
import Services.Chat (buildPrompt)

spec :: Spec
spec = describe "Chat" $ do
  describe "buildPrompt" $ do
    it "includes system prompt and query" $ do
      let ctx = ChatContext
            { contextCalendarEvents = []
            , contextRecentActivities = []
            , contextQuarantineCount = 0
            , contextSurfacedCount = 0
            , contextMentionedPeople = []
            }
      let prompt = buildPrompt ctx "what's on today?"
      prompt `shouldSatisfy` T.isInfixOf "Wisp"
      prompt `shouldSatisfy` T.isInfixOf "what's on today?"
      prompt `shouldSatisfy` T.isInfixOf "No events scheduled today"

    it "includes context counts in prompt" $ do
      let ctx = ChatContext
            { contextCalendarEvents = []
            , contextRecentActivities = []
            , contextQuarantineCount = 5
            , contextSurfacedCount = 3
            , contextMentionedPeople = []
            }
      let prompt = buildPrompt ctx "test"
      prompt `shouldSatisfy` T.isInfixOf "5 items in quarantine"
      prompt `shouldSatisfy` T.isInfixOf "3 items surfaced"
