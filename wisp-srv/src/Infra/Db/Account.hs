module Infra.Db.Account
  ( upsertAccount
  , getAllAccounts
  , getAccountByEmail
  , getAccountById
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow(..), fieldWith, field)
import Database.PostgreSQL.Simple.FromField (fromField, returnError, ResultError(..), Conversion)
import Domain.Id (EntityId(..), newEntityId)
import Domain.Account (Account(..), AccountProvider(..))
import App.Monad (App, getConn)

instance FromRow Account where
  fromRow = Account
    <$> (EntityId <$> field)  -- id
    <*> providerField         -- provider (text -> AccountProvider)
    <*> field                 -- display_name
    <*> field                 -- details (jsonb)
    <*> field                 -- created_at
    where
      providerField = fieldWith $ \f mbs -> do
        txt <- fromField f mbs :: Conversion Text
        case txt of
          "google" -> pure Google
          "github" -> pure GitHub
          other    -> returnError ConversionFailed f $ "Unknown provider: " <> T.unpack other

-- Upsert account by email, returns the account (existing or new)
-- NOTE: This function signature will be updated in Task 4 to support multi-provider
upsertAccount :: Text -> Maybe Text -> App Account
upsertAccount email displayName = do
  conn <- getConn
  aid <- liftIO newEntityId
  _ <- liftIO $ execute conn
    "insert into accounts (id, provider, display_name, details) \
    \values (?, 'google', ?, ?::jsonb) \
    \on conflict (provider, (details->>'email')) do update set \
    \  display_name = coalesce(excluded.display_name, accounts.display_name)"
    (unEntityId aid, displayName, "{\"email\":\"" <> email <> "\"}")
  -- Fetch the account (might be existing or newly created)
  results <- liftIO $ query conn
    "select id, provider, display_name, details, created_at from accounts where details->>'email' = ?"
    (Only email)
  case results of
    [acc] -> pure acc
    _ -> error $ "Failed to upsert account: " <> show email

-- Get all accounts
getAllAccounts :: App [Account]
getAllAccounts = do
  conn <- getConn
  liftIO $ query_ conn
    "select id, provider, display_name, details, created_at from accounts order by created_at"

-- Get account by email
getAccountByEmail :: Text -> App (Maybe Account)
getAccountByEmail email = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, provider, display_name, details, created_at from accounts where details->>'email' = ?"
    (Only email)
  pure $ case results of
    [acc] -> Just acc
    _ -> Nothing

-- Get account by ID
getAccountById :: EntityId -> App (Maybe Account)
getAccountById aid = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, provider, display_name, details, created_at from accounts where id = ?"
    (Only $ unEntityId aid)
  pure $ case results of
    [acc] -> Just acc
    _ -> Nothing
