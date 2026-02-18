module Infra.Db.Summary
  ( insertSummary
  , getSummary
  , getRecentSummaries
  , getSummariesForSessions
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Domain.Id (newEntityId, unEntityId)
import Domain.Summary
import App.Monad (App, getConn)

instance FromRow Summary where
  fromRow = Summary
    <$> (SummaryId <$> field)
    <*> field
    <*> (fromPGArray <$> field)
    <*> field
    <*> field

insertSummary :: Text -> [Text] -> Text -> App Summary
insertSummary agentId sessionIds content = do
  conn <- getConn
  sid <- liftIO $ unEntityId <$> newEntityId
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "insert into summaries (id, agent_id, session_ids, content, created_at) \
    \values (?, ?, ?, ?, ?)"
    (sid, agentId, PGArray sessionIds, content, now)
  pure Summary
    { summaryId = SummaryId sid
    , summaryAgentId = agentId
    , summarySessionIds = sessionIds
    , summaryContent = content
    , summaryCreatedAt = now
    }

getSummary :: SummaryId -> App (Maybe Summary)
getSummary sid = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, agent_id, session_ids, content, created_at \
    \from summaries where id = ?"
    (Only $ unSummaryId sid)
  pure $ case results of
    [s] -> Just s
    _ -> Nothing

getRecentSummaries :: Text -> Int -> App [Summary]
getRecentSummaries agentId limit = do
  conn <- getConn
  liftIO $ query conn
    "select id, agent_id, session_ids, content, created_at \
    \from summaries where agent_id = ? \
    \order by created_at desc limit ?"
    (agentId, limit)

getSummariesForSessions :: [Text] -> App [Summary]
getSummariesForSessions sessionIds = do
  conn <- getConn
  liftIO $ query conn
    "select id, agent_id, session_ids, content, created_at \
    \from summaries where session_ids && ? \
    \order by created_at desc"
    (Only $ PGArray sessionIds)
