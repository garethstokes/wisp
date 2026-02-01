module Services.CalendarPollerSpec where

import Test.Hspec
import Services.CalendarPoller (parseEventTime)
import Infra.Google.Calendar (CalendarDateTime(..))

spec :: Spec
spec = describe "CalendarPoller" $ do
  describe "parseEventTime" $ do
    it "parses ISO8601 datetime with Z suffix" $ do
      let dt = CalendarDateTime (Just "2026-02-01T10:00:00Z") Nothing Nothing
      case parseEventTime dt of
        Nothing -> expectationFailure "Failed to parse"
        Just _ -> True `shouldBe` True

    it "parses ISO8601 datetime with timezone offset" $ do
      let dt = CalendarDateTime (Just "2026-02-01T10:00:00+00:00") Nothing Nothing
      case parseEventTime dt of
        Nothing -> expectationFailure "Failed to parse"
        Just _ -> True `shouldBe` True

    it "returns Nothing for date-only (all-day) events" $ do
      let dt = CalendarDateTime Nothing (Just "2026-02-01") Nothing
      parseEventTime dt `shouldBe` Nothing

    it "returns Nothing when both dateTime and date are Nothing" $ do
      let dt = CalendarDateTime Nothing Nothing Nothing
      parseEventTime dt `shouldBe` Nothing
