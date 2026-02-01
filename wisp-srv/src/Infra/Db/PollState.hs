module Infra.Db.PollState
  ( PollState(..)
  , getPollState
  , updatePollState
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import App.Monad (App, getConn)

data PollState = PollState
  { pollSource :: Text
  , pollLastAt :: UTCTime
  , pollCursor :: Maybe Text
  } deriving (Show)

instance FromRow PollState where
  fromRow = PollState <$> field <*> field <*> field

-- Get poll state for a source
getPollState :: Text -> App (Maybe PollState)
getPollState source = do
  conn <- getConn
  results <- liftIO $ query conn
    "select source, last_poll_at, cursor from poll_state where source = ?"
    (Only source)
  pure $ case results of
    [ps] -> Just ps
    _ -> Nothing

-- Update poll state with new cursor
updatePollState :: Text -> Maybe Text -> App ()
updatePollState source cursor = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "update poll_state set last_poll_at = now(), cursor = ? where source = ?"
    (cursor, source)
  pure ()
