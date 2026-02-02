module Infra.Db.Auth
  ( AuthToken(..)
  , saveToken
  , getToken
  , getTokenForAccount
  , getAllTokens
  , updateToken
  , updateTokenForAccount
  , tokenNeedsRefresh
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (PGArray(..), Query(..))
import Domain.Id (EntityId(..), newEntityId)
import App.Monad (App, getConn)
import NeatInterpolation (trimming)

data AuthToken = AuthToken
  { tokenId :: EntityId
  , tokenAccountId :: EntityId
  , tokenProvider :: Text
  , tokenAccessToken :: Text
  , tokenRefreshToken :: Text
  , tokenExpiresAt :: UTCTime
  , tokenScopes :: [Text]
  } deriving (Show)

instance FromRow AuthToken where
  fromRow = AuthToken
    <$> (EntityId <$> field)        -- id
    <*> (EntityId <$> field)        -- account_id
    <*> field                        -- provider
    <*> field                        -- access_token
    <*> field                        -- refresh_token
    <*> field                        -- expires_at
    <*> (fromPGArray <$> field)     -- scopes

-- Save a new token (upsert)
saveToken :: EntityId -> Text -> Text -> Text -> UTCTime -> [Text] -> App EntityId
saveToken accountId prov accessTok refreshTok expires scps = do
  conn <- getConn
  tid <- liftIO newEntityId
  _ <- liftIO $ execute conn (Query $ encodeUtf8 [trimming|
    insert into auth_tokens (id, account_id, provider, access_token, refresh_token, expires_at, scopes)
    values (?, ?, ?, ?, ?, ?, ?)
    on conflict (account_id, provider) do update set
      access_token = excluded.access_token,
      refresh_token = excluded.refresh_token,
      expires_at = excluded.expires_at,
      scopes = excluded.scopes,
      updated_at = now()
  |]) (unEntityId tid, unEntityId accountId, prov, accessTok, refreshTok, expires, PGArray scps)
  pure tid

-- Get token for an account and provider
getTokenForAccount :: EntityId -> Text -> App (Maybe AuthToken)
getTokenForAccount accountId prov = do
  conn <- getConn
  results <- liftIO $ query conn (Query $ encodeUtf8 [trimming|
    select id, account_id, provider, access_token, refresh_token, expires_at, scopes
    from auth_tokens where account_id = ? and provider = ?
  |]) (unEntityId accountId, prov)
  pure $ case results of
    [tok] -> Just tok
    _ -> Nothing

-- Get all tokens for a provider (for polling all accounts)
getAllTokens :: Text -> App [AuthToken]
getAllTokens prov = do
  conn <- getConn
  liftIO $ query conn (Query $ encodeUtf8 [trimming|
    select id, account_id, provider, access_token, refresh_token, expires_at, scopes
    from auth_tokens where provider = ? and account_id is not null
  |]) (Only prov)

-- Update access token for an account after refresh
updateTokenForAccount :: EntityId -> Text -> Text -> UTCTime -> App ()
updateTokenForAccount accountId prov accessTok expires = do
  conn <- getConn
  _ <- liftIO $ execute conn (Query $ encodeUtf8 [trimming|
    update auth_tokens set access_token = ?, expires_at = ?, updated_at = now()
    where account_id = ? and provider = ?
  |]) (accessTok, expires, unEntityId accountId, prov)
  pure ()

-- Legacy: get token by provider only (for backwards compat during migration)
getToken :: Text -> App (Maybe AuthToken)
getToken prov = do
  conn <- getConn
  results <- liftIO $ query conn (Query $ encodeUtf8 [trimming|
    select id, account_id, provider, access_token, refresh_token, expires_at, scopes
    from auth_tokens where provider = ? limit 1
  |]) (Only prov)
  pure $ case results of
    [tok] -> Just tok
    _ -> Nothing

-- Legacy: update by provider only
updateToken :: Text -> Text -> UTCTime -> App ()
updateToken prov accessTok expires = do
  conn <- getConn
  _ <- liftIO $ execute conn (Query $ encodeUtf8 [trimming|
    update auth_tokens set access_token = ?, expires_at = ?, updated_at = now()
    where provider = ?
  |]) (accessTok, expires, prov)
  pure ()

-- Check if token needs refresh (within 5 minutes of expiry)
tokenNeedsRefresh :: AuthToken -> IO Bool
tokenNeedsRefresh tok = do
  now <- getCurrentTime
  let fiveMinutes = 5 * 60
  pure $ tokenExpiresAt tok <= addUTCTime fiveMinutes now
