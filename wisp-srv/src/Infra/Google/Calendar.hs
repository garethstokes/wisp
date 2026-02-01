module Infra.Google.Calendar
  ( CalendarEvent(..)
  , CalendarEventList(..)
  , CalendarDateTime(..)
  , CalendarPerson(..)
  , listEvents
  , getEvent
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), withObject, object, (.=), Value)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Aeson as Aeson

-- | A calendar event
data CalendarEvent = CalendarEvent
  { eventId :: Text
  , eventStatus :: Maybe Text
  , eventSummary :: Maybe Text
  , eventDescription :: Maybe Text
  , eventStart :: Maybe CalendarDateTime
  , eventEnd :: Maybe CalendarDateTime
  , eventCreator :: Maybe CalendarPerson
  , eventOrganizer :: Maybe CalendarPerson
  , eventAttendees :: Maybe [CalendarPerson]
  , eventHtmlLink :: Maybe Text
  , eventUpdated :: Maybe Text
  , eventRaw :: Maybe Value
  } deriving (Show, Eq)

instance ToJSON CalendarEvent where
  toJSON evt = object
    [ "id" .= eventId evt
    , "status" .= eventStatus evt
    , "summary" .= eventSummary evt
    , "description" .= eventDescription evt
    , "start" .= eventStart evt
    , "end" .= eventEnd evt
    , "creator" .= eventCreator evt
    , "organizer" .= eventOrganizer evt
    , "attendees" .= eventAttendees evt
    , "htmlLink" .= eventHtmlLink evt
    , "updated" .= eventUpdated evt
    , "raw" .= eventRaw evt
    ]

instance FromJSON CalendarEvent where
  parseJSON = withObject "CalendarEvent" $ \v -> CalendarEvent
    <$> v .: "id"
    <*> v .:? "status"
    <*> v .:? "summary"
    <*> v .:? "description"
    <*> v .:? "start"
    <*> v .:? "end"
    <*> v .:? "creator"
    <*> v .:? "organizer"
    <*> v .:? "attendees"
    <*> v .:? "htmlLink"
    <*> v .:? "updated"
    <*> pure Nothing  -- eventRaw is set separately when needed

-- | DateTime for calendar events (supports both timed and all-day events)
data CalendarDateTime = CalendarDateTime
  { dateTimeValue :: Maybe Text  -- For timed events (ISO 8601)
  , dateValue :: Maybe Text      -- For all-day events (YYYY-MM-DD)
  , timeZone :: Maybe Text
  } deriving (Show, Eq)

instance ToJSON CalendarDateTime where
  toJSON dt = object
    [ "dateTime" .= dateTimeValue dt
    , "date" .= dateValue dt
    , "timeZone" .= timeZone dt
    ]

instance FromJSON CalendarDateTime where
  parseJSON = withObject "CalendarDateTime" $ \v -> CalendarDateTime
    <$> v .:? "dateTime"
    <*> v .:? "date"
    <*> v .:? "timeZone"

-- | A person (creator, organizer, or attendee)
data CalendarPerson = CalendarPerson
  { personEmail :: Maybe Text
  , personDisplayName :: Maybe Text
  , personSelf :: Maybe Bool
  , personResponseStatus :: Maybe Text
  } deriving (Show, Eq)

instance ToJSON CalendarPerson where
  toJSON p = object
    [ "email" .= personEmail p
    , "displayName" .= personDisplayName p
    , "self" .= personSelf p
    , "responseStatus" .= personResponseStatus p
    ]

instance FromJSON CalendarPerson where
  parseJSON = withObject "CalendarPerson" $ \v -> CalendarPerson
    <$> v .:? "email"
    <*> v .:? "displayName"
    <*> v .:? "self"
    <*> v .:? "responseStatus"

-- | Response from events.list endpoint
data CalendarEventList = CalendarEventList
  { events :: Maybe [CalendarEvent]
  , nextPageToken :: Maybe Text
  , nextSyncToken :: Maybe Text
  } deriving (Show, Eq)

instance FromJSON CalendarEventList where
  parseJSON = withObject "CalendarEventList" $ \v -> CalendarEventList
    <$> v .:? "items"
    <*> v .:? "nextPageToken"
    <*> v .:? "nextSyncToken"

-- | Base URL for Calendar API
baseUrl :: String
baseUrl = "https://www.googleapis.com/calendar/v3/calendars/primary"

-- | List events in the user's calendar
listEvents :: Text -> Maybe Text -> Maybe Text -> IO (Either Text CalendarEventList)
listEvents accessToken syncToken pageToken = do
  manager <- newManager tlsManagerSettings
  let params = case (syncToken, pageToken) of
        (Just st, _) -> "?syncToken=" <> T.unpack st
        (_, Just pt) -> "?pageToken=" <> T.unpack pt
        (Nothing, Nothing) -> ""
  let url = baseUrl <> "/events" <> params
  initialReq <- parseRequest url
  let req = initialReq
        { requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 accessToken)]
        }
  response <- httpLbs req manager
  pure $ case Aeson.decode (responseBody response) of
    Just evtList -> Right evtList
    Nothing -> Left $ "Failed to parse events list: " <> T.pack (show $ responseBody response)

-- | Get a single event by ID
getEvent :: Text -> Text -> IO (Either Text CalendarEvent)
getEvent accessToken eventId = do
  manager <- newManager tlsManagerSettings
  let url = baseUrl <> "/events/" <> T.unpack eventId
  initialReq <- parseRequest url
  let req = initialReq
        { requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 accessToken)]
        }
  response <- httpLbs req manager
  pure $ case Aeson.decode (responseBody response) of
    Just evt -> Right evt
    Nothing -> Left $ "Failed to parse event: " <> T.pack (show $ responseBody response)
