module Services.PeopleResolverSpec where

import Test.Hspec
import Services.PeopleResolver (extractDisplayName)

spec :: Spec
spec = describe "PeopleResolver" $ do
  describe "extractDisplayName" $ do
    it "extracts name from 'Name <email>' format" $ do
      extractDisplayName "John Doe <john@example.com>" `shouldBe` Just "John Doe"

    it "extracts name with quotes" $ do
      extractDisplayName "\"Jane Smith\" <jane@example.com>" `shouldBe` Just "Jane Smith"

    it "returns Nothing for plain email" $ do
      extractDisplayName "john@example.com" `shouldBe` Nothing

    it "handles empty name before angle brackets" $ do
      extractDisplayName "<john@example.com>" `shouldBe` Nothing
