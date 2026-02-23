module Skills.Concierge.ClassifierSpec where

import Test.Hspec
import Skills.Concierge.Classifier (buildClassificationPrompt, buildClassificationPromptWithProjects, parseClassificationResponse)
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

  describe "buildClassificationPromptWithProjects" $ do
    it "includes known projects section when projects provided" $ do
      let raw = object ["snippet" .= ("Meeting tomorrow" :: String)]
      let projects = ["wisp", "lune", "super-it"]
      let prompt = buildClassificationPromptWithProjects "email" (Just "Re: Wisp Update") raw projects
      prompt `shouldSatisfy` \p -> "Known Projects" `T.isInfixOf` p
      prompt `shouldSatisfy` \p -> "wisp" `T.isInfixOf` p
      prompt `shouldSatisfy` \p -> "lune" `T.isInfixOf` p
      prompt `shouldSatisfy` \p -> "super-it" `T.isInfixOf` p

    it "includes projects field in JSON schema" $ do
      let raw = object ["snippet" .= ("Hello" :: String)]
      let prompt = buildClassificationPromptWithProjects "email" Nothing raw ["wisp"]
      prompt `shouldSatisfy` \p -> "\"projects\"" `T.isInfixOf` p

    it "includes project assignment guidelines" $ do
      let raw = object []
      let prompt = buildClassificationPromptWithProjects "email" Nothing raw ["wisp"]
      prompt `shouldSatisfy` \p -> "Project Assignment Guidelines" `T.isInfixOf` p

    it "still works with empty projects list" $ do
      let raw = object ["subject" .= ("Test" :: String)]
      let prompt = buildClassificationPromptWithProjects "email" (Just "Test") raw []
      -- Should still be a valid prompt, just without projects section
      prompt `shouldSatisfy` \p -> "classifying an incoming email" `T.isInfixOf` p

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
