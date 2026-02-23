module Skills.ConciergeSpec where

import Test.Hspec
import Skills.Concierge (agentInfo)
import Domain.Agent

spec :: Spec
spec = describe "Concierge" $ do
  describe "agentInfo" $ do
    it "has correct id" $ do
      agentId agentInfo `shouldBe` "concierge"

    it "is marked as implemented" $ do
      agentImplemented agentInfo `shouldBe` True

    it "has expected tools" $ do
      length (agentTools agentInfo) `shouldBe` 3
      map toolName (agentTools agentInfo) `shouldBe`
        ["update_activities", "query_activities", "query_people"]
