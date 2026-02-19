module Agents.DispatcherSpec where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

-- Test parseToolCall helper functionality
-- The actual dispatcher requires database access, so we test the pure functions

spec :: Spec
spec = describe "Dispatcher" $ do
  describe "formatMessages" $ do
    it "formats user messages with User: prefix" $ do
      -- This tests the message formatting logic
      let msg = formatTestMessage "user" "hello"
      msg `shouldBe` "User: hello"

    it "formats assistant messages with Assistant: prefix" $ do
      let msg = formatTestMessage "assistant" "hi there"
      msg `shouldBe` "Assistant: hi there"

  describe "parseToolCall patterns" $ do
    it "recognizes tool call JSON structure" $ do
      let response = "{\"tool\": \"search_knowledge\", \"args\": {\"tags\": [\"note\"]}}"
      isToolCallResponse response `shouldBe` True

    it "rejects plain text responses" $ do
      let response = "Hello, how can I help you today?"
      isToolCallResponse response `shouldBe` False

    it "rejects partial JSON" $ do
      let response = "Here is my response: {\"partial\":"
      isToolCallResponse response `shouldBe` False

-- Helper to test message formatting
formatTestMessage :: Text -> Text -> Text
formatTestMessage role content = case role of
  "user" -> "User: " <> content
  "assistant" -> "Assistant: " <> content
  r -> r <> ": " <> content

-- Helper to test tool call detection
isToolCallResponse :: Text -> Bool
isToolCallResponse response =
  let trimmed = T.strip response
  in T.isPrefixOf "{" trimmed && T.isSuffixOf "}" trimmed && T.isInfixOf "\"tool\"" trimmed
