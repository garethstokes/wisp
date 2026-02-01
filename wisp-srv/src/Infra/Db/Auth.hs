module Infra.Db.Auth
  ( AuthToken(..)
  , saveToken
  , getToken
  , updateToken
  , tokenNeedsRefresh
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Domain.Id (EntityId(..), newEntityId)
import App.Monad (App, getConn)

data AuthToken = AuthToken
  { tokenId :: EntityId
  , tokenProvider :: Text
  , tokenAccessToken :: Text
  , tokenRefreshToken :: Text
  , tokenExpiresAt :: UTCTime
  , tokenScopes :: [Text]
  } deriving (Show)

instance FromRow AuthToken where
  fromRow = AuthToken
    <$> (EntityId <$> field)
    <*> field
    <*> field
    <*> field
    <*> field
    <*> (fromPGArray <$> field)

-- Save a new token (upsert)
saveToken :: Text -> Text -> Text -> UTCTime -> [Text] -> App EntityId
saveToken prov accessTok refreshTok expires scps = do
  conn <- getConn
  tid <- liftIO newEntityId
  _ <- liftIO $ execute conn
    "insert into auth_tokens (id, provider, access_token, refresh_token, expires_at, scopes) \
    \values (?, ?, ?, ?, ?, ?) \
    \on conflict (provider) do update set \
    \  access_token = excluded.access_token, \
    \  refresh_token = excluded.refresh_token, \
    \  expires_at = excluded.expires_at, \
    \  scopes = excluded.scopes, \
    \  updated_at = now()"
    (unEntityId tid, prov, accessTok, refreshTok, expires, PGArray scps)
  pure tid

-- Get token for a provider
getToken :: Text -> App (Maybe AuthToken)
getToken prov = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, provider, access_token, refresh_token, expires_at, scopes \
    \from auth_tokens where provider = ?"
    (Only prov)
  pure $ case results of
    [tok] -> Just tok
    _ -> Nothing

-- Update access token after refresh
updateToken :: Text -> Text -> UTCTime -> App ()
updateToken prov accessTok expires = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "update auth_tokens set access_token = ?, expires_at = ?, updated_at = now() \
    \where provider = ?"
    (accessTok, expires, prov)
  pure ()

-- Check if token needs refresh (within 5 minutes of expiry)
tokenNeedsRefresh :: AuthToken -> IO Bool
tokenNeedsRefresh tok = do
  now <- getCurrentTime
  let fiveMinutes = 5 * 60
  pure $ tokenExpiresAt tok <= addUTCTime fiveMinutes now
