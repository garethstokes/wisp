module Infra.Claude.ClientSpec where

import Test.Hspec
import Infra.Claude.Client (ClaudeResponse(..), responseText)
import Data.Aeson (decode)

spec :: Spec
spec = describe "Claude Client" $ do
  describe "parseClaudeResponse" $ do
    it "extracts text from Claude API response" $ do
      let json = "{\"content\":[{\"type\":\"text\",\"text\":\"Hello world\"}],\
                 \\"model\":\"claude-sonnet-4-20250514\",\"stop_reason\":\"end_turn\"}"
      case decode json :: Maybe ClaudeResponse of
        Nothing -> expectationFailure "Failed to parse Claude response"
        Just resp -> responseText resp `shouldBe` Just "Hello world"

    it "handles empty content" $ do
      let json = "{\"content\":[],\"model\":\"claude-sonnet-4-20250514\",\"stop_reason\":\"end_turn\"}"
      case decode json :: Maybe ClaudeResponse of
        Nothing -> expectationFailure "Failed to parse Claude response"
        Just resp -> responseText resp `shouldBe` Nothing
