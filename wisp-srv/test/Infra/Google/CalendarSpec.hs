module Infra.Google.CalendarSpec where

import Test.Hspec
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Infra.Google.Calendar

spec :: Spec
spec = describe "Calendar API" $ do
  describe "CalendarEvent parsing" $ do
    it "parses a minimal event response" $ do
      let json = LBS.pack "{\"id\":\"evt123\",\"status\":\"confirmed\"}"
      case decode json :: Maybe CalendarEvent of
        Nothing -> expectationFailure "Failed to parse minimal event"
        Just evt -> do
          eventId evt `shouldBe` "evt123"
          eventStatus evt `shouldBe` Just "confirmed"

    it "parses an event with optional fields" $ do
      let json = LBS.pack $ concat
            [ "{\"id\":\"evt123\",\"status\":\"confirmed\""
            , ",\"summary\":\"Team Meeting\""
            , ",\"description\":\"Weekly sync\""
            , ",\"htmlLink\":\"https://calendar.google.com/event?eid=xxx\""
            , ",\"updated\":\"2024-01-15T10:00:00.000Z\""
            , "}"
            ]
      case decode json :: Maybe CalendarEvent of
        Nothing -> expectationFailure "Failed to parse event with optional fields"
        Just evt -> do
          eventId evt `shouldBe` "evt123"
          eventSummary evt `shouldBe` Just "Team Meeting"
          eventDescription evt `shouldBe` Just "Weekly sync"
          eventHtmlLink evt `shouldBe` Just "https://calendar.google.com/event?eid=xxx"
          eventUpdated evt `shouldBe` Just "2024-01-15T10:00:00.000Z"

  describe "CalendarDateTime parsing" $ do
    it "parses a timed event datetime" $ do
      let json = LBS.pack "{\"dateTime\":\"2024-01-15T10:00:00-05:00\",\"timeZone\":\"America/New_York\"}"
      case decode json :: Maybe CalendarDateTime of
        Nothing -> expectationFailure "Failed to parse timed datetime"
        Just dt -> do
          dateTimeValue dt `shouldBe` Just "2024-01-15T10:00:00-05:00"
          dateValue dt `shouldBe` Nothing
          timeZone dt `shouldBe` Just "America/New_York"

    it "parses an all-day event date" $ do
      let json = LBS.pack "{\"date\":\"2024-01-15\"}"
      case decode json :: Maybe CalendarDateTime of
        Nothing -> expectationFailure "Failed to parse all-day date"
        Just dt -> do
          dateTimeValue dt `shouldBe` Nothing
          dateValue dt `shouldBe` Just "2024-01-15"
          timeZone dt `shouldBe` Nothing

  describe "CalendarPerson parsing" $ do
    it "parses a person with all fields" $ do
      let json = LBS.pack $ concat
            [ "{\"email\":\"test@example.com\""
            , ",\"displayName\":\"Test User\""
            , ",\"self\":true"
            , ",\"responseStatus\":\"accepted\""
            , "}"
            ]
      case decode json :: Maybe CalendarPerson of
        Nothing -> expectationFailure "Failed to parse person"
        Just person -> do
          personEmail person `shouldBe` Just "test@example.com"
          personDisplayName person `shouldBe` Just "Test User"
          personSelf person `shouldBe` Just True
          personResponseStatus person `shouldBe` Just "accepted"

    it "parses a minimal person" $ do
      let json = LBS.pack "{\"email\":\"test@example.com\"}"
      case decode json :: Maybe CalendarPerson of
        Nothing -> expectationFailure "Failed to parse minimal person"
        Just person -> do
          personEmail person `shouldBe` Just "test@example.com"
          personDisplayName person `shouldBe` Nothing
          personSelf person `shouldBe` Nothing

  describe "CalendarEventList parsing" $ do
    it "parses an empty event list" $ do
      let json = LBS.pack "{}"
      case decode json :: Maybe CalendarEventList of
        Nothing -> expectationFailure "Failed to parse empty list"
        Just lst -> do
          events lst `shouldBe` Nothing
          nextPageToken lst `shouldBe` Nothing
          nextSyncToken lst `shouldBe` Nothing

    it "parses an event list with events" $ do
      let json = LBS.pack $ concat
            [ "{\"items\":[{\"id\":\"e1\",\"status\":\"confirmed\"}]"
            , ",\"nextPageToken\":\"token123\""
            , ",\"nextSyncToken\":\"sync456\""
            , "}"
            ]
      case decode json :: Maybe CalendarEventList of
        Nothing -> expectationFailure "Failed to parse event list"
        Just lst -> do
          case events lst of
            Nothing -> expectationFailure "Expected events"
            Just evts -> do
              length evts `shouldBe` 1
              eventId (head evts) `shouldBe` "e1"
          nextPageToken lst `shouldBe` Just "token123"
          nextSyncToken lst `shouldBe` Just "sync456"

  describe "CalendarEvent with start/end parsing" $ do
    it "parses an event with start and end times" $ do
      let json = LBS.pack $ concat
            [ "{\"id\":\"evt123\""
            , ",\"start\":{\"dateTime\":\"2024-01-15T10:00:00-05:00\",\"timeZone\":\"America/New_York\"}"
            , ",\"end\":{\"dateTime\":\"2024-01-15T11:00:00-05:00\",\"timeZone\":\"America/New_York\"}"
            , ",\"creator\":{\"email\":\"creator@example.com\",\"displayName\":\"Creator\"}"
            , ",\"organizer\":{\"email\":\"organizer@example.com\",\"self\":true}"
            , "}"
            ]
      case decode json :: Maybe CalendarEvent of
        Nothing -> expectationFailure "Failed to parse event with times"
        Just evt -> do
          eventId evt `shouldBe` "evt123"
          case eventStart evt of
            Nothing -> expectationFailure "Expected start time"
            Just start -> dateTimeValue start `shouldBe` Just "2024-01-15T10:00:00-05:00"
          case eventEnd evt of
            Nothing -> expectationFailure "Expected end time"
            Just end -> dateTimeValue end `shouldBe` Just "2024-01-15T11:00:00-05:00"
          case eventCreator evt of
            Nothing -> expectationFailure "Expected creator"
            Just creator -> personEmail creator `shouldBe` Just "creator@example.com"
          case eventOrganizer evt of
            Nothing -> expectationFailure "Expected organizer"
            Just organizer -> personSelf organizer `shouldBe` Just True

  describe "CalendarEvent with attendees" $ do
    it "parses an event with attendees list" $ do
      let json = LBS.pack $ concat
            [ "{\"id\":\"evt123\""
            , ",\"attendees\":["
            , "{\"email\":\"a1@example.com\",\"responseStatus\":\"accepted\"}"
            , ",{\"email\":\"a2@example.com\",\"responseStatus\":\"declined\"}"
            , "]"
            , "}"
            ]
      case decode json :: Maybe CalendarEvent of
        Nothing -> expectationFailure "Failed to parse event with attendees"
        Just evt -> do
          eventId evt `shouldBe` "evt123"
          case eventAttendees evt of
            Nothing -> expectationFailure "Expected attendees"
            Just attendees -> do
              length attendees `shouldBe` 2
              personEmail (head attendees) `shouldBe` Just "a1@example.com"
              personResponseStatus (head attendees) `shouldBe` Just "accepted"
              personEmail (attendees !! 1) `shouldBe` Just "a2@example.com"
              personResponseStatus (attendees !! 1) `shouldBe` Just "declined"
