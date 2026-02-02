module Domain.ChatSpec where

import Test.Hspec
import Data.Aeson (decode, encode)
import Domain.Chat

spec :: Spec
spec = describe "Chat" $ do
  describe "ChatRequest" $ do
    it "parses from JSON" $ do
      let json = "{\"message\": \"hello\"}"
      let result = decode json :: Maybe ChatRequest
      fmap chatMessage result `shouldBe` Just "hello"

  describe "ChatResponse" $ do
    it "serializes to JSON" $ do
      let resp = ChatResponse "hi there"
      encode resp `shouldBe` "{\"message\":\"hi there\"}"
