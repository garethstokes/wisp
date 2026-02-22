module Infra.Db.Session
  ( createSession
  , getSession
  , getActiveSession
  , getRecentSessions
  , appendMessage
  , endSession
  , markSummarized
  , getUnsummarizedSessions
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, decode, Value)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Domain.Id (newEntityId, unEntityId)
import Domain.Session
import Domain.Chat (ChatMessage)
import App.Monad (App, getConn)

instance FromRow Session where
  fromRow = Session
    <$> (SessionId <$> field)
    <*> field
    <*> (parseMessages <$> field)
    <*> field
    <*> field
    <*> field
    <*> field  -- last_message_at
    where
      parseMessages :: Value -> [ChatMessage]
      parseMessages v = case decode (encode v) of
        Just msgs -> msgs
        Nothing -> []

createSession :: Text -> App Session
createSession agentId = do
  conn <- getConn
  sid <- liftIO $ unEntityId <$> newEntityId
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "insert into sessions (id, agent_id, messages, created_at, last_message_at) values (?, ?, '[]', ?, ?)"
    (sid, agentId, now, now)
  pure Session
    { sessionId = SessionId sid
    , sessionAgentId = agentId
    , sessionMessages = []
    , sessionCreatedAt = now
    , sessionEndedAt = Nothing
    , sessionSummarized = False
    , sessionLastMessageAt = now
    }

getSession :: SessionId -> App (Maybe Session)
getSession sid = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, agent_id, messages, created_at, ended_at, summarized, last_message_at \
    \from sessions where id = ?"
    (Only $ unSessionId sid)
  pure $ case results of
    [s] -> Just s
    _ -> Nothing

getActiveSession :: Text -> App (Maybe Session)
getActiveSession agentId = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, agent_id, messages, created_at, ended_at, summarized, last_message_at \
    \from sessions where agent_id = ? and ended_at is null \
    \order by created_at desc limit 1"
    (Only agentId)
  pure $ case results of
    [s] -> Just s
    _ -> Nothing

getRecentSessions :: Text -> Int -> App [Session]
getRecentSessions agentId limit = do
  conn <- getConn
  liftIO $ query conn
    "select id, agent_id, messages, created_at, ended_at, summarized, last_message_at \
    \from sessions where agent_id = ? \
    \order by created_at desc limit ?"
    (agentId, limit)

appendMessage :: SessionId -> ChatMessage -> App Session
appendMessage sid msg = do
  conn <- getConn
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "update sessions set messages = messages || ?::jsonb, last_message_at = ? where id = ?"
    (encode [msg], now, unSessionId sid)
  mSession <- getSession sid
  case mSession of
    Just s -> pure s
    Nothing -> error "Session not found after append"

endSession :: SessionId -> App ()
endSession sid = do
  conn <- getConn
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "update sessions set ended_at = ? where id = ?"
    (now, unSessionId sid)
  pure ()

markSummarized :: SessionId -> App ()
markSummarized sid = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "update sessions set summarized = true where id = ?"
    (Only $ unSessionId sid)
  pure ()

getUnsummarizedSessions :: Text -> App [Session]
getUnsummarizedSessions agentId = do
  conn <- getConn
  liftIO $ query conn
    "select id, agent_id, messages, created_at, ended_at, summarized, last_message_at \
    \from sessions \
    \where agent_id = ? and ended_at is not null and not summarized \
    \order by created_at"
    (Only agentId)
