module Infra.Db.Soul
  ( getSoul
  , getOrCreateSoul
  , updateSoulPersonality
  , addInsight
  , removeInsight
  , updateSoul
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, decode, Value)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Domain.Soul
import App.Monad (App, getConn)

instance FromRow Soul where
  fromRow = Soul
    <$> field
    <*> field
    <*> (parseInsights <$> field)
    <*> field
    where
      parseInsights :: Value -> [Text]
      parseInsights v = case decode (encode v) of
        Just insights -> insights
        Nothing -> []

getSoul :: Text -> App (Maybe Soul)
getSoul agentId = do
  conn <- getConn
  results <- liftIO $ query conn
    "select agent_id, personality, insights, updated_at from souls where agent_id = ?"
    (Only agentId)
  pure $ case results of
    [s] -> Just s
    _ -> Nothing

getOrCreateSoul :: Text -> App Soul
getOrCreateSoul agentId = do
  existing <- getSoul agentId
  case existing of
    Just soul -> pure soul
    Nothing -> do
      conn <- getConn
      now <- liftIO getCurrentTime
      _ <- liftIO $ execute conn
        "insert into souls (agent_id, personality, insights, updated_at) \
        \values (?, '', '[]', ?) on conflict do nothing"
        (agentId, now)
      mSoul <- getSoul agentId
      case mSoul of
        Just soul -> pure soul
        Nothing -> pure $ emptySoul agentId

updateSoulPersonality :: Text -> Text -> App ()
updateSoulPersonality agentId personality = do
  conn <- getConn
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "update souls set personality = ?, updated_at = ? where agent_id = ?"
    (personality, now, agentId)
  pure ()

addInsight :: Text -> Text -> App ()
addInsight agentId insight = do
  conn <- getConn
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "update souls set insights = insights || ?::jsonb, updated_at = ? where agent_id = ?"
    (encode [insight], now, agentId)
  pure ()

removeInsight :: Text -> Text -> App ()
removeInsight agentId insight = do
  conn <- getConn
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "update souls set \
    \  insights = (select coalesce(jsonb_agg(elem), '[]'::jsonb) from jsonb_array_elements(insights) elem where elem != ?::jsonb), \
    \  updated_at = ? \
    \where agent_id = ?"
    (encode insight, now, agentId)
  pure ()

updateSoul :: Soul -> App ()
updateSoul soul = do
  conn <- getConn
  now <- liftIO getCurrentTime
  _ <- liftIO $ execute conn
    "insert into souls (agent_id, personality, insights, updated_at) \
    \values (?, ?, ?, ?) \
    \on conflict (agent_id) do update set \
    \  personality = excluded.personality, \
    \  insights = excluded.insights, \
    \  updated_at = excluded.updated_at"
    (soulAgentId soul, soulPersonality soul, encode (soulInsights soul), now)
  pure ()
