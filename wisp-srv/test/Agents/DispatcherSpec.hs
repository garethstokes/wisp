module Agents.DispatcherSpec where

import Test.Hspec
import Agents.Dispatcher (allAgents, getAgent)
import Domain.Agent (agentId, agentImplemented)

spec :: Spec
spec = describe "Dispatcher" $ do
  describe "allAgents" $ do
    it "returns all four agents" $ do
      length allAgents `shouldBe` 4

    it "includes concierge as implemented" $ do
      let concierge = head [a | a <- allAgents, agentId a == "wisp/concierge"]
      agentImplemented concierge `shouldBe` True

  describe "getAgent" $ do
    it "finds concierge by id" $ do
      case getAgent "wisp/concierge" of
        Nothing -> expectationFailure "Should find concierge"
        Just a -> agentId a `shouldBe` "wisp/concierge"

    it "returns Nothing for unknown agent" $ do
      getAgent "wisp/unknown" `shouldBe` Nothing
