module Domain.SchemaSpec where

import Test.Hspec
import Domain.Schema
import Data.Aeson (encode, decode)

spec :: Spec
spec = describe "Schema" $ do
  it "serializes to expected JSON structure" $ do
    let s = Schema "activity" 1
    encode s `shouldBe` "{\"name\":\"activity\",\"version\":1}"

  it "round-trips through JSON" $ do
    let s = Schema "person" 2
    decode (encode s) `shouldBe` Just s
