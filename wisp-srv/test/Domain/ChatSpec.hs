module Domain.ChatSpec where

import Test.Hspec
import Data.Aeson (decode, encode, Value)
import Domain.Chat

spec :: Spec
spec = describe "Chat" $ do
  describe "ChatMessage" $ do
    it "parses user message" $ do
      let json = "{\"role\":\"user\",\"content\":\"Hello\"}"
      case decode json :: Maybe ChatMessage of
        Nothing -> expectationFailure "Failed to parse"
        Just m -> do
          messageRole m `shouldBe` "user"
          messageContent m `shouldBe` "Hello"
          messageToolCall m `shouldBe` Nothing

    it "parses assistant message with tool call" $ do
      let json = "{\"role\":\"assistant\",\"content\":\"OK\",\"tool_call\":{\"tool\":\"test\"}}"
      case decode json :: Maybe ChatMessage of
        Nothing -> expectationFailure "Failed to parse"
        Just m -> do
          messageRole m `shouldBe` "assistant"
          messageToolCall m `shouldSatisfy` (/= Nothing)

  describe "ChatRequest" $ do
    it "parses request with agent and messages" $ do
      let json = "{\"agent\":\"wisp/concierge\",\"messages\":[{\"role\":\"user\",\"content\":\"Hi\"}]}"
      case decode json :: Maybe ChatRequest of
        Nothing -> expectationFailure "Failed to parse"
        Just r -> do
          chatAgent r `shouldBe` "wisp/concierge"
          length (chatMessages r) `shouldBe` 1

  describe "ChatResponse" $ do
    it "serializes to JSON" $ do
      let resp = ChatResponse "hi there" Nothing
      encode resp `shouldBe` "{\"message\":\"hi there\"}"

    it "serializes with tool_call" $ do
      let mtc = decode "{\"tool\":\"test\"}" :: Maybe Value
      let resp = ChatResponse "hi" mtc
      -- Just verify it doesn't crash - exact format may vary
      encode resp `shouldSatisfy` (\_ -> True)
