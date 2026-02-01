-- src/Services/CalendarPoller.hs
module Services.CalendarPoller
  ( pollCalendar
  , parseEventTime
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

import App.Monad (App)
import Domain.Activity (NewActivity(..), ActivitySource(..))
import Infra.Db.Activity (insertActivity)
import Infra.Db.PollState (getPollState, updatePollState, PollState(..))
import Infra.Google.Calendar
  ( CalendarEvent(..)
  , CalendarEventList(..)
  , CalendarDateTime(..)
  , CalendarPerson(..)
  , listEvents
  )
import Infra.Google.TokenManager (getValidToken, TokenError(..))

-- | Poll calendar for events and import them as activities.
-- Returns the count of successfully imported events.
pollCalendar :: App (Either Text Int)
pollCalendar = do
  tokenResult <- getValidToken
  case tokenResult of
    Left NoToken -> pure $ Left "No Google token available"
    Left (RefreshFailed err) -> pure $ Left $ "Token refresh failed: " <> err
    Right accessToken -> do
      mPollState <- getPollState "calendar"
      let syncToken = mPollState >>= pollCursor
      pollWithToken accessToken syncToken

-- | Poll with a sync token, handling 410 errors by retrying without token
pollWithToken :: Text -> Maybe Text -> App (Either Text Int)
pollWithToken accessToken syncToken = do
  result <- liftIO $ listEvents accessToken syncToken Nothing
  case result of
    Left err ->
      -- Handle 410 Gone error (sync token invalidated)
      if "410" `T.isInfixOf` err || "Sync token" `T.isInfixOf` err
        then case syncToken of
          Just _ -> pollWithToken accessToken Nothing  -- Retry without sync token
          Nothing -> pure $ Left err  -- Already tried without token
        else pure $ Left err
    Right evtList -> do
      count <- processEventList accessToken evtList
      -- Update poll state with new sync token
      updatePollState "calendar" (nextSyncToken evtList)
      pure $ Right count

-- | Process an event list, handling pagination
processEventList :: Text -> CalendarEventList -> App Int
processEventList accessToken evtList = do
  let evts = fromMaybe [] (events evtList)
  count <- processEvents evts
  -- Handle pagination
  case nextPageToken evtList of
    Nothing -> pure count
    Just pt -> do
      nextResult <- liftIO $ listEvents accessToken Nothing (Just pt)
      case nextResult of
        Left _ -> pure count
        Right nextEvtList -> do
          nextCount <- processEventList accessToken nextEvtList
          pure $ count + nextCount

-- | Process a list of calendar events, inserting them as activities
processEvents :: [CalendarEvent] -> App Int
processEvents evts = do
  results <- forM evts $ \evt -> do
    -- Skip cancelled events
    case eventStatus evt of
      Just "cancelled" -> pure 0
      _ -> do
        let startTime = eventStart evt >>= parseEventTime
            endTime = eventEnd evt >>= parseEventTime
            organizerEmail = eventOrganizer evt >>= personEmail
            newActivity = NewActivity
              { newActivitySource = Calendar
              , newActivitySourceId = eventId evt
              , newActivityRaw = toJSON evt
              , newActivityTitle = eventSummary evt
              , newActivitySenderEmail = organizerEmail
              , newActivityStartsAt = startTime
              , newActivityEndsAt = endTime
              }
        result <- insertActivity newActivity
        pure $ case result of
          Just _ -> 1
          Nothing -> 0  -- Duplicate, already exists
  pure $ sum results

-- | Parse a CalendarDateTime to UTCTime.
-- Returns Nothing for all-day events (date-only).
parseEventTime :: CalendarDateTime -> Maybe UTCTime
parseEventTime dt =
  case dateTimeValue dt of
    Just dtStr -> parseISO8601 dtStr
    Nothing -> Nothing  -- All-day events have only dateValue

-- | Parse ISO8601 datetime string to UTCTime
-- Handles formats like "2026-02-01T10:00:00Z" and "2026-02-01T10:00:00+00:00"
parseISO8601 :: Text -> Maybe UTCTime
parseISO8601 txt =
  let str = T.unpack txt
  in  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" str
      <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" str
      <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" str
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) Nothing b = b
    (<|>) a _ = a
