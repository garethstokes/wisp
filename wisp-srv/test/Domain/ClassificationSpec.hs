module Domain.ClassificationSpec where

import Test.Hspec
import Domain.Classification
import Data.Aeson (decode, encode)

spec :: Spec
spec = describe "Classification" $ do
  describe "JSON parsing" $ do
    it "parses a valid classification response" $ do
      let json = "{\"personas\":[\"work\"],\"activity_type\":\"request\",\
                 \\"urgency\":\"normal\",\"autonomy_tier\":2,\
                 \\"confidence\":0.85,\"summary\":\"Meeting request\",\
                 \\"reasoning\":\"Work-related meeting request\",\
                 \\"suggested_actions\":[\"Accept\",\"Decline\"],\
                 \\"option_framing\":null}"
      case decode json :: Maybe Classification of
        Nothing -> expectationFailure "Failed to parse Classification"
        Just c -> do
          classificationPersonas c `shouldBe` ["work"]
          classificationActivityType c `shouldBe` Request
          classificationUrgency c `shouldBe` Normal
          classificationAutonomyTier c `shouldBe` 2
          classificationConfidence c `shouldBe` 0.85
          classificationSummary c `shouldBe` "Meeting request"
          classificationReasoning c `shouldBe` "Work-related meeting request"
          classificationSuggestedActions c `shouldBe` ["Accept", "Decline"]
          classificationOptionFraming c `shouldBe` Nothing

  describe "ActivityType" $ do
    it "parses all activity types" $ do
      (decode "\"request\"" :: Maybe ActivityType) `shouldBe` Just Request
      (decode "\"information\"" :: Maybe ActivityType) `shouldBe` Just Information
      (decode "\"action_required\"" :: Maybe ActivityType) `shouldBe` Just ActionRequired
      (decode "\"fyi\"" :: Maybe ActivityType) `shouldBe` Just FYI
      (decode "\"event\"" :: Maybe ActivityType) `shouldBe` Just Event

  describe "Urgency" $ do
    it "parses all urgency levels" $ do
      (decode "\"high\"" :: Maybe Urgency) `shouldBe` Just High
      (decode "\"normal\"" :: Maybe Urgency) `shouldBe` Just Normal
      (decode "\"low\"" :: Maybe Urgency) `shouldBe` Just Low
