module Domain.ActivitySpec where

import Test.Hspec
import Domain.Activity
import Data.Aeson (encode, decode, object, (.=))

spec :: Spec
spec = describe "Activity" $ do
  describe "ActivitySource" $ do
    it "serializes Email to JSON" $ do
      encode Email `shouldBe` "\"email\""

    it "serializes Calendar to JSON" $ do
      encode Calendar `shouldBe` "\"calendar\""

  describe "ActivityStatus" $ do
    it "round-trips through JSON" $ do
      decode (encode Pending) `shouldBe` Just Pending
      decode (encode Processed) `shouldBe` Just Processed
