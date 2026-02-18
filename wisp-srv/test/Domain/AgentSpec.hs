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

  describe "AgentConfig" $ do
    it "parses agent name from tag" $ do
      parseAgentTag "agent:jarvis" `shouldBe` Just "jarvis"
      parseAgentTag "other" `shouldBe` Nothing

    it "round-trips AgentConfig through JSON" $ do
      let config = AgentConfig
            { agentPersonalitySeed = "Formal, concise"
            , agentActiveSkill = Just "concierge"
            }
      decode (encode config) `shouldBe` Just config

    it "parses AgentConfig with null active_skill" $ do
      let config = AgentConfig
            { agentPersonalitySeed = "Helpful"
            , agentActiveSkill = Nothing
            }
      decode (encode config) `shouldBe` Just config
