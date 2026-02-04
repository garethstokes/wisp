module Agents.SchedulerSpec (spec) where

import Test.Hspec
import Data.Aeson (decode)

import Agents.Scheduler
import Domain.Agent (agentId, agentImplemented)

spec :: Spec
spec = do
  describe "Scheduler" $ do
    describe "agentInfo" $ do
      it "has correct agent ID" $ do
        agentId agentInfo `shouldBe` "wisp/scheduler"

      it "is marked as implemented" $ do
        agentImplemented agentInfo `shouldBe` True

    describe "CalendarQuery parsing" $ do
      it "parses query_calendar with days" $ do
        let json = "{\"tool\": \"query_calendar\", \"days\": 14}"
        let result = decode json :: Maybe SchedulerToolCall
        case result of
          Just (QueryCalendar q) -> calendarDays q `shouldBe` Just 14
          _ -> expectationFailure "Expected QueryCalendar"

      it "parses query_calendar with date" $ do
        let json = "{\"tool\": \"query_calendar\", \"date\": \"2026-02-10\"}"
        let result = decode json :: Maybe SchedulerToolCall
        case result of
          Just (QueryCalendar q) -> calendarDate q `shouldBe` Just "2026-02-10"
          _ -> expectationFailure "Expected QueryCalendar"

    describe "FreeSlotQuery parsing" $ do
      it "parses find_free_slots with all parameters" $ do
        let json = "{\"tool\": \"find_free_slots\", \"days\": 5, \"duration_minutes\": 30, \"start_hour\": 10, \"end_hour\": 16}"
        let result = decode json :: Maybe SchedulerToolCall
        case result of
          Just (FindFreeSlots q) -> do
            slotDays q `shouldBe` Just 5
            slotDuration q `shouldBe` Just 30
            slotStartHour q `shouldBe` Just 10
            slotEndHour q `shouldBe` Just 16
          _ -> expectationFailure "Expected FindFreeSlots"

      it "parses find_free_slots with defaults" $ do
        let json = "{\"tool\": \"find_free_slots\"}"
        let result = decode json :: Maybe SchedulerToolCall
        case result of
          Just (FindFreeSlots q) -> do
            slotDays q `shouldBe` Nothing
            slotDuration q `shouldBe` Nothing
          _ -> expectationFailure "Expected FindFreeSlots"
