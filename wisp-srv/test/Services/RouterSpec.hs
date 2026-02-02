module Services.RouterSpec where

import Test.Hspec
import Services.Router (determineStatus)
import Domain.Activity (ActivityStatus(..))
import Domain.Classification (Classification(..), ActivityType(..), Urgency(..))

spec :: Spec
spec = describe "Router" $ do
  describe "determineStatus" $ do
    it "quarantines low confidence items" $ do
      let c = mkClassification 0.3 2
      determineStatus 0.5 c `shouldBe` Quarantined

    it "processes tier 1 silently" $ do
      let c = mkClassification 0.9 1
      determineStatus 0.5 c `shouldBe` Processed

    it "processes tier 2 as noted" $ do
      let c = mkClassification 0.9 2
      determineStatus 0.5 c `shouldBe` Processed

    it "keeps tier 3 pending for review" $ do
      let c = mkClassification 0.9 3
      determineStatus 0.5 c `shouldBe` Pending

    it "surfaces tier 4 items" $ do
      let c = mkClassification 0.9 4
      determineStatus 0.5 c `shouldBe` Surfaced

mkClassification :: Double -> Int -> Classification
mkClassification conf tier = Classification
  { classificationPersonas = ["work"]
  , classificationActivityType = Request
  , classificationUrgency = Normal
  , classificationAutonomyTier = tier
  , classificationConfidence = conf
  , classificationSummary = "Test"
  }
