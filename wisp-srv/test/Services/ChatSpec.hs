module Services.ChatSpec where

import Test.Hspec
import qualified Data.Text as T
import Domain.Chat (ChatContext(..))
import Services.Chat (buildSystemPrompt)

spec :: Spec
spec = describe "Chat" $ do
  describe "buildSystemPrompt" $ do
    it "includes Wisp identity and action format" $ do
      let ctx = emptyContext
      let prompt = buildSystemPrompt ctx
      prompt `shouldSatisfy` T.isInfixOf "Wisp"
      prompt `shouldSatisfy` T.isInfixOf "Never say"
      prompt `shouldSatisfy` T.isInfixOf "No events scheduled today"
      prompt `shouldSatisfy` T.isInfixOf "action"
      prompt `shouldSatisfy` T.isInfixOf "mark_complete"

    it "includes context counts" $ do
      let ctx = emptyContext
      let prompt = buildSystemPrompt ctx
      -- Now shows counts in headers like "Surfaced (0 items needing attention)"
      prompt `shouldSatisfy` T.isInfixOf "Surfaced (0 items"
      prompt `shouldSatisfy` T.isInfixOf "Quarantined (0 items"

emptyContext :: ChatContext
emptyContext = ChatContext
  { contextCalendarEvents = []
  , contextRecentActivities = []
  , contextPendingEmails = []
  , contextQuarantined = []
  , contextSurfaced = []
  , contextNeedsReview = []
  , contextMentionedPeople = []
  }
