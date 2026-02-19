module Services.GmailPoller
  ( pollGmail
  , pollGmailForAccount
  , pollAllGmail
  , extractEmailInfo
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import App.Monad (App)
import Domain.Id (EntityId)
import Domain.Account (Account(..), accountIdentifier)
import Domain.Activity (NewActivity(..), ActivitySource(..))
import Infra.Db.Activity (insertActivity, activityExistsForAccount)
import Infra.Db.PollState (getPollStateForAccount, updatePollStateForAccount, ensurePollStateExists, PollState(..))
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
import Infra.Google.TokenManager (getValidTokenForAccount, getAllValidTokens, TokenError(..))

-- | Poll Gmail for ALL accounts, returns (email, Either error [newActivityIds])
pollAllGmail :: App [(Text, Either Text [EntityId])]
pollAllGmail = do
  tokensWithAccounts <- getAllValidTokens
  forM tokensWithAccounts $ \(acc, tokenResult) -> do
    result <- case tokenResult of
      Left NoToken -> pure $ Left "No token"
      Left (RefreshFailed err) -> pure $ Left $ "Token refresh failed: " <> err
      Right token -> pollGmailWithToken (accountId acc) token
    pure (fromMaybe "<unknown>" (accountIdentifier acc), result)

-- | Poll Gmail for a specific account
pollGmailForAccount :: Account -> App (Either Text [EntityId])
pollGmailForAccount acc = do
  tokenResult <- getValidTokenForAccount (accountId acc)
  case tokenResult of
    Left NoToken -> pure $ Left "No Google token available"
    Left (RefreshFailed err) -> pure $ Left $ "Token refresh failed: " <> err
    Right accessToken -> pollGmailWithToken (accountId acc) accessToken

-- | Poll Gmail with a specific token for an account
pollGmailWithToken :: EntityId -> Text -> App (Either Text [EntityId])
pollGmailWithToken accId accessToken = do
  -- Ensure poll state exists for this account
  ensurePollStateExists accId "gmail"
  mPollState <- getPollStateForAccount accId "gmail"
  case mPollState of
    Nothing -> initialPoll accId accessToken
    Just ps -> case pollCursor ps of
      Nothing -> initialPoll accId accessToken
      Just cursor -> incrementalPoll accId accessToken cursor

-- | Initial poll: fetch recent messages when no cursor exists
initialPoll :: EntityId -> Text -> App (Either Text [EntityId])
initialPoll accId accessToken = do
  result <- liftIO $ listMessages accessToken Nothing
  case result of
    Left err -> pure $ Left err
    Right msgList -> do
      let refs = fromMaybe [] (messages msgList)
      newIds <- processMessageRefs accId accessToken refs
      case refs of
        [] -> pure $ Right []
        (firstRef:_) -> do
          firstMsgResult <- liftIO $ getMessage accessToken (refId firstRef)
          case firstMsgResult of
            Left _ -> do
              updatePollStateForAccount accId "gmail" (Just $ refId firstRef)
              pure $ Right newIds
            Right firstMsg -> do
              updatePollStateForAccount accId "gmail" (gmailHistoryId firstMsg)
              pure $ Right newIds

-- | Incremental poll: use history API to get new messages since last cursor
incrementalPoll :: EntityId -> Text -> Text -> App (Either Text [EntityId])
incrementalPoll accId accessToken cursor = do
  result <- liftIO $ listHistory accessToken cursor Nothing
  case result of
    Left err ->
      if "404" `T.isInfixOf` err || "historyId" `T.isInfixOf` err
        then initialPoll accId accessToken
        else pure $ Left err
    Right histList -> do
      let refs = extractRefsFromHistory histList
      newIds <- processMessageRefs accId accessToken refs
      let newCursor = historyId histList
      updatePollStateForAccount accId "gmail" newCursor
      pure $ Right newIds

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
-- Returns list of newly created activity IDs
processMessageRefs :: EntityId -> Text -> [GmailMessageRef] -> App [EntityId]
processMessageRefs accId accessToken refs = do
  results <- forM refs $ \ref -> do
    -- Check if already exists for this account
    exists <- activityExistsForAccount accId Email (refId ref)
    if exists
      then pure Nothing
      else do
        msgResult <- liftIO $ getMessage accessToken (refId ref)
        case msgResult of
          Left _ -> pure Nothing
          Right msg -> do
            let (subject, sender) = extractEmailInfo msg
            let newActivity = NewActivity
                  { newActivityAccountId = accId
                  , newActivitySource = Email
                  , newActivitySourceId = gmailId msg
                  , newActivityRaw = toJSON msg
                  , newActivityTitle = subject
                  , newActivitySenderEmail = sender
                  , newActivityStartsAt = Nothing
                  , newActivityEndsAt = Nothing
                  }
            insertActivity newActivity
  pure $ [aid | Just aid <- results]

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

-- Legacy: poll using first available token
pollGmail :: App (Either Text [EntityId])
pollGmail = do
  results <- pollAllGmail
  case results of
    [] -> pure $ Left "No accounts configured"
    _ -> pure $ Right $ concat [ids | (_, Right ids) <- results]
