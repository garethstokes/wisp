module Services.CalendarPoller
  ( pollCalendar
  , pollCalendarForAccount
  , pollAllCalendar
  , parseEventTime
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, ZonedTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601ParseM)

import App.Monad (App)
import Domain.Id (EntityId)
import Domain.Account (Account(..), accountIdentifier)
import Domain.Activity (NewActivity(..), ActivitySource(..))
import Infra.Db.Activity (insertActivity, activityExistsForAccount)
import Infra.Db.PollState (getPollStateForAccount, updatePollStateForAccount, ensurePollStateExists, PollState(..))
import Infra.Google.Calendar
  ( CalendarEvent(..)
  , CalendarEventList(..)
  , CalendarDateTime(..)
  , CalendarPerson(..)
  , listEvents
  )
import Infra.Google.TokenManager (getValidTokenForAccount, getAllValidTokens, TokenError(..))

-- | Poll Calendar for ALL accounts, returns (email, Either error [newActivityIds])
pollAllCalendar :: App [(Text, Either Text [EntityId])]
pollAllCalendar = do
  tokensWithAccounts <- getAllValidTokens
  forM tokensWithAccounts $ \(acc, tokenResult) -> do
    result <- case tokenResult of
      Left NoToken -> pure $ Left "No token"
      Left (RefreshFailed err) -> pure $ Left $ "Token refresh failed: " <> err
      Right token -> pollCalendarWithToken (accountId acc) token
    pure (fromMaybe "<unknown>" (accountIdentifier acc), result)

-- | Poll Calendar for a specific account
pollCalendarForAccount :: Account -> App (Either Text [EntityId])
pollCalendarForAccount acc = do
  tokenResult <- getValidTokenForAccount (accountId acc)
  case tokenResult of
    Left NoToken -> pure $ Left "No Google token available"
    Left (RefreshFailed err) -> pure $ Left $ "Token refresh failed: " <> err
    Right accessToken -> pollCalendarWithToken (accountId acc) accessToken

-- | Poll Calendar with a specific token for an account
pollCalendarWithToken :: EntityId -> Text -> App (Either Text [EntityId])
pollCalendarWithToken accId accessToken = do
  ensurePollStateExists accId "calendar"
  mPollState <- getPollStateForAccount accId "calendar"
  let mSyncToken = mPollState >>= pollCursor

  result <- liftIO $ listEvents accessToken mSyncToken Nothing
  case result of
    Left err ->
      if "410" `T.isInfixOf` err || "Sync token" `T.isInfixOf` err
        then do
          updatePollStateForAccount accId "calendar" Nothing
          pollCalendarWithToken accId accessToken
        else pure $ Left err
    Right eventList -> do
      let evts = fromMaybe [] (events eventList)
      newIds <- processEvents accId evts
      case nextSyncToken eventList of
        Just newToken -> updatePollStateForAccount accId "calendar" (Just newToken)
        Nothing -> pure ()
      pure $ Right newIds

-- | Process a list of calendar events
-- Returns list of newly created activity IDs
processEvents :: EntityId -> [CalendarEvent] -> App [EntityId]
processEvents accId evts = do
  results <- forM evts $ \evt -> do
    case eventStatus evt of
      Just "cancelled" -> pure Nothing
      _ -> do
        exists <- activityExistsForAccount accId Calendar (eventId evt)
        if exists
          then pure Nothing
          else do
            let startTime = eventStart evt >>= parseEventTime
            let endTime = eventEnd evt >>= parseEventTime
            let newActivity = NewActivity
                  { newActivityAccountId = accId
                  , newActivitySource = Calendar
                  , newActivitySourceId = eventId evt
                  , newActivityRaw = fromMaybe (toJSON evt) (eventRaw evt)
                  , newActivityTitle = eventSummary evt
                  , newActivitySenderEmail = eventOrganizer evt >>= personEmail
                  , newActivityStartsAt = startTime
                  , newActivityEndsAt = endTime
                  }
            insertActivity newActivity
  pure $ [aid | Just aid <- results]

-- | Parse event time from CalendarDateTime
-- Handles both Z suffix and numeric timezone offsets like +00:00
parseEventTime :: CalendarDateTime -> Maybe UTCTime
parseEventTime dt = case dateTimeValue dt of
  Just dtStr ->
    let s = T.unpack dtStr
    in -- Try parsing as UTCTime first (handles Z suffix)
       case iso8601ParseM s :: Maybe UTCTime of
         Just utc -> Just utc
         Nothing ->
           -- Try parsing as ZonedTime (handles +00:00 offsets) and convert
           case iso8601ParseM s :: Maybe ZonedTime of
             Just zt -> Just (zonedTimeToUTC zt)
             Nothing -> Nothing
  Nothing -> Nothing

-- Legacy: poll all accounts
pollCalendar :: App (Either Text [EntityId])
pollCalendar = do
  results <- pollAllCalendar
  case results of
    [] -> pure $ Left "No accounts configured"
    _ -> pure $ Right $ concat [ids | (_, Right ids) <- results]
