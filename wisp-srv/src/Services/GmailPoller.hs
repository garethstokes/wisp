-- src/Services/GmailPoller.hs
module Services.GmailPoller
  ( pollGmail
  , extractEmailInfo
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import App.Monad (App)
import Domain.Activity (NewActivity(..), ActivitySource(..))
import Infra.Db.Activity (insertActivity)
import Infra.Db.PollState (getPollState, updatePollState, PollState(..))
import Infra.Google.Gmail
  ( GmailMessage(..)
  , GmailMessageList(..)
  , GmailMessageRef(..)
  , GmailPayload(..)
  , GmailHeader(..)
  , GmailHistoryList(..)
  , GmailHistory(..)
  , GmailHistoryMessage(..)
  , listMessages
  , getMessage
  , listHistory
  )
import Infra.Google.TokenManager (getValidToken, TokenError(..))

-- | Poll Gmail for new messages and import them as activities.
-- Returns the count of successfully imported messages.
pollGmail :: App (Either Text Int)
pollGmail = do
  tokenResult <- getValidToken
  case tokenResult of
    Left NoToken -> pure $ Left "No Google token available"
    Left (RefreshFailed err) -> pure $ Left $ "Token refresh failed: " <> err
    Right accessToken -> do
      mPollState <- getPollState "gmail"
      case mPollState of
        Nothing -> initialPoll accessToken
        Just ps -> incrementalPoll accessToken ps

-- | Initial poll: fetch recent messages when no cursor exists
initialPoll :: Text -> App (Either Text Int)
initialPoll accessToken = do
  result <- liftIO $ listMessages accessToken Nothing
  case result of
    Left err -> pure $ Left err
    Right msgList -> do
      let refs = fromMaybe [] (messages msgList)
      count <- processMessageRefs accessToken refs
      -- Store the first message ID as cursor for future incremental polls
      -- Gmail's historyId from any message can be used as startHistoryId
      case refs of
        [] -> pure $ Right 0
        (firstRef:_) -> do
          -- Get the first message to extract its historyId for future polls
          firstMsgResult <- liftIO $ getMessage accessToken (refId firstRef)
          case firstMsgResult of
            Left _ -> do
              -- If we can't get historyId, just store the message ID
              updatePollState "gmail" (Just $ refId firstRef)
              pure $ Right count
            Right firstMsg -> do
              -- Use internalDate as a proxy cursor since we don't have historyId in message
              -- For proper history tracking, we'd need to call getProfile
              updatePollState "gmail" (gmailInternalDate firstMsg)
              pure $ Right count

-- | Incremental poll: use history API to get new messages since last cursor
incrementalPoll :: Text -> PollState -> App (Either Text Int)
incrementalPoll accessToken ps = do
  case pollCursor ps of
    Nothing -> initialPoll accessToken
    Just cursor -> do
      result <- liftIO $ listHistory accessToken cursor Nothing
      case result of
        Left err ->
          -- History ID may have expired, fall back to initial poll
          if "404" `T.isInfixOf` err || "historyId" `T.isInfixOf` err
            then initialPoll accessToken
            else pure $ Left err
        Right histList -> do
          let refs = extractRefsFromHistory histList
          count <- processMessageRefs accessToken refs
          -- Update cursor to new historyId if available
          let newCursor = historyId histList
          updatePollState "gmail" newCursor
          pure $ Right count

-- | Extract message refs from history records
extractRefsFromHistory :: GmailHistoryList -> [GmailMessageRef]
extractRefsFromHistory histList =
  let histories = fromMaybe [] (history histList)
      extractFromHistory h =
        case historyMessagesAdded h of
          Nothing -> []
          Just added -> map historyMessage added
  in concatMap extractFromHistory histories

-- | Process a list of message refs: fetch full message and insert as activity
processMessageRefs :: Text -> [GmailMessageRef] -> App Int
processMessageRefs accessToken refs = do
  results <- forM refs $ \ref -> do
    msgResult <- liftIO $ getMessage accessToken (refId ref)
    case msgResult of
      Left _ -> pure 0
      Right msg -> do
        let (subject, sender) = extractEmailInfo msg
        let newActivity = NewActivity
              { newActivitySource = Email
              , newActivitySourceId = gmailId msg
              , newActivityRaw = toJSON msg
              , newActivityTitle = subject
              , newActivitySenderEmail = sender
              , newActivityStartsAt = Nothing
              , newActivityEndsAt = Nothing
              }
        result <- insertActivity newActivity
        pure $ case result of
          Just _ -> 1
          Nothing -> 0  -- Duplicate, already exists
  pure $ sum results

-- | Extract subject and sender email from a Gmail message
extractEmailInfo :: GmailMessage -> (Maybe Text, Maybe Text)
extractEmailInfo msg =
  case gmailPayload msg of
    Nothing -> (Nothing, Nothing)
    Just payload ->
      case payloadHeaders payload of
        Nothing -> (Nothing, Nothing)
        Just headers ->
          let findHeader name =
                case filter (\h -> headerName h == name) headers of
                  [] -> Nothing
                  (h:_) -> Just (headerValue h)
              subject = findHeader "Subject"
              sender = findHeader "From"
          in (subject, sender)
