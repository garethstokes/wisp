module Domain.ActivityDocumentSpec where

import Test.Hspec
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Domain.ActivityDocument

spec :: Spec
spec = describe "ActivityDocument" $ do
  describe "LinkSource" $ do
    it "serializes Classifier to 'classifier'" $ do
      LBS.unpack (encode Classifier) `shouldBe` "\"classifier\""

    it "deserializes 'user' to User" $ do
      decode "\"user\"" `shouldBe` Just User

  describe "Relationship" $ do
    it "serializes Project to 'project'" $ do
      LBS.unpack (encode Project) `shouldBe` "\"project\""
