module Domain.ActivitySpec where

import Test.Hspec
import Domain.Activity
import Data.Aeson (encode, decode, toJSON, object, (.=))

spec :: Spec
spec = describe "Activity" $ do
  describe "ActivitySource" $ do
    it "serializes Email to JSON" $ do
      encode Email `shouldBe` "\"email\""

    it "serializes Calendar to JSON" $ do
      encode Calendar `shouldBe` "\"calendar\""

    it "includes Note as a valid source" $ do
      toJSON Note `shouldBe` "note"

    it "parses note from JSON" $ do
      decode "\"note\"" `shouldBe` Just Note

  describe "ActivityStatus" $ do
    it "round-trips through JSON" $ do
      decode (encode Pending) `shouldBe` Just Pending
      decode (encode Processed) `shouldBe` Just Processed

  describe "Activity tags" $ do
    it "normalizes tag names to lowercase" $ do
      normalizeTag "SuperIT" `shouldBe` "superit"
      normalizeTag "ALICE" `shouldBe` "alice"
