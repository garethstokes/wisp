module Domain.AgentSpec where

import Test.Hspec
import Data.Aeson (encode, decode)
import Domain.Agent

spec :: Spec
spec = describe "Agent" $ do
  describe "ToolType" $ do
    it "serializes to JSON" $ do
      encode Deterministic `shouldBe` "\"deterministic\""
      encode Decision `shouldBe` "\"decision\""

  describe "ToolInfo" $ do
    it "serializes to JSON" $ do
      let tool = ToolInfo "update_activities" Decision
      let json = encode tool
      decode json `shouldBe` Just tool

  describe "AgentInfo" $ do
    it "serializes to JSON" $ do
      let agent = AgentInfo
            { agentId = "wisp/concierge"
            , agentDescription = "Intake, classification, routing"
            , agentTools = [ToolInfo "update_activities" Decision]
            , agentWorkflows = ["classify-pending", "quarantine-interview"]
            , agentImplemented = True
            }
      let json = encode agent
      decode json `shouldBe` Just agent
