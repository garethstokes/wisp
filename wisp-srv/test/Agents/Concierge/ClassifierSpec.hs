module Agents.Concierge.ClassifierSpec where

import Test.Hspec
import Agents.Concierge.Classifier (buildClassificationPrompt, parseClassificationResponse)
import Domain.Classification (Classification(..), ActivityType(..))
import Data.Aeson (object, (.=))
import qualified Data.Text as T

spec :: Spec
spec = describe "Classifier" $ do
  describe "buildClassificationPrompt" $ do
    it "includes email subject in prompt" $ do
      let raw = object ["snippet" .= ("Meeting tomorrow" :: String)]
      let prompt = buildClassificationPrompt "email" (Just "Re: Project Update") raw
      prompt `shouldSatisfy` \p -> "Project Update" `T.isInfixOf` p

  describe "parseClassificationResponse" $ do
    it "parses valid JSON classification" $ do
      let json = "{\"personas\":[\"work\"],\"activity_type\":\"request\",\
                 \\"urgency\":\"normal\",\"autonomy_tier\":2,\
                 \\"confidence\":0.85,\"summary\":\"Meeting request\",\
                 \\"reasoning\":\"Work meeting\",\
                 \\"suggested_actions\":[\"Accept\"],\
                 \\"option_framing\":null}"
      case parseClassificationResponse json of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right c -> do
          classificationActivityType c `shouldBe` Request
          classificationConfidence c `shouldBe` 0.85
          classificationReasoning c `shouldBe` "Work meeting"

    it "fails on invalid JSON" $ do
      let json = "not valid json"
      case parseClassificationResponse json of
        Left _ -> pure ()  -- Expected
        Right _ -> expectationFailure "Should have failed"
