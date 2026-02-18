module Domain.SummarySpec (spec) where

import Test.Hspec
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Domain.Summary

spec :: Spec
spec = describe "Summary" $ do
  it "serializes Summary to JSON" $ do
    let summary = Summary
          { summaryId = SummaryId "sum-123"
          , summaryAgentId = "wisp/scheduler"
          , summarySessionIds = ["sess-1", "sess-2"]
          , summaryContent = "Discussed scheduling preferences"
          , summaryCreatedAt = read "2026-02-18 12:00:00 UTC"
          }
    unpack (encode summary) `shouldContain` "\"content\":\"Discussed scheduling preferences\""
