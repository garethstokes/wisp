module Infra.Db.PollState
  ( PollState(..)
  , getPollState
  , getPollStateForAccount
  , updatePollState
  , updatePollStateForAccount
  , ensurePollStateExists
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import App.Monad (App, getConn)
import Domain.Id (EntityId(..))

data PollState = PollState
  { pollAccountId :: EntityId    -- link to account
  , pollSource :: Text
  , pollLastAt :: UTCTime
  , pollCursor :: Maybe Text
  } deriving (Show)

instance FromRow PollState where
  fromRow = PollState
    <$> (EntityId <$> field)  -- account_id
    <*> field                  -- source
    <*> field                  -- last_poll_at
    <*> field                  -- cursor

-- Get poll state for an account and source
getPollStateForAccount :: EntityId -> Text -> App (Maybe PollState)
getPollStateForAccount accountId source = do
  conn <- getConn
  results <- liftIO $ query conn
    "select account_id, source, last_poll_at, cursor from poll_state \
    \where account_id = ? and source = ?"
    (unEntityId accountId, source)
  pure $ case results of
    [ps] -> Just ps
    _ -> Nothing

-- Legacy: get by source only (will return first match)
getPollState :: Text -> App (Maybe PollState)
getPollState source = do
  conn <- getConn
  results <- liftIO $ query conn
    "select account_id, source, last_poll_at, cursor from poll_state where source = ? limit 1"
    (Only source)
  pure $ case results of
    [ps] -> Just ps
    _ -> Nothing

-- Update poll state for an account
updatePollStateForAccount :: EntityId -> Text -> Maybe Text -> App ()
updatePollStateForAccount accountId source cursor = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "update poll_state set last_poll_at = now(), cursor = ? \
    \where account_id = ? and source = ?"
    (cursor, unEntityId accountId, source)
  pure ()

-- Legacy: update by source only
updatePollState :: Text -> Maybe Text -> App ()
updatePollState source cursor = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "update poll_state set last_poll_at = now(), cursor = ? where source = ?"
    (cursor, source)
  pure ()

-- Ensure poll state exists for an account (create if missing)
ensurePollStateExists :: EntityId -> Text -> App ()
ensurePollStateExists accountId source = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "insert into poll_state (account_id, source, last_poll_at, cursor) \
    \values (?, ?, now(), null) \
    \on conflict (account_id, source) do nothing"
    (unEntityId accountId, source)
  pure ()
