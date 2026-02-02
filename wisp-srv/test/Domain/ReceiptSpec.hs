module Domain.ReceiptSpec where

import Test.Hspec
import Domain.Receipt (ReceiptAction(..))
import Data.Aeson (toJSON)

spec :: Spec
spec = describe "Receipt" $ do
  describe "ReceiptAction" $ do
    it "serializes actions to JSON" $ do
      toJSON Classified `shouldBe` "classified"
      toJSON Quarantined `shouldBe` "quarantined"
      toJSON Processed `shouldBe` "processed"
      toJSON Surfaced `shouldBe` "surfaced"
