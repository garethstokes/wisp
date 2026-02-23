module Domain.SoulSpec (spec) where

import Test.Hspec
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Domain.Soul

spec :: Spec
spec = describe "Soul" $ do
  it "serializes Soul to JSON" $ do
    let soul = Soul
          { soulAgentId = "wisp"
          , soulPersonality = "Concise, uses bullet points"
          , soulInsights = ["Prefers morning meetings", "Dislikes preambles"]
          , soulUpdatedAt = read "2026-02-18 10:00:00 UTC"
          }
    unpack (encode soul) `shouldContain` "\"personality\":\"Concise, uses bullet points\""
    unpack (encode soul) `shouldContain` "\"Prefers morning meetings\""

  it "starts with empty soul" $ do
    let empty = emptySoul "wisp"
    soulPersonality empty `shouldBe` ""
    soulInsights empty `shouldBe` []
