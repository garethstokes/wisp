module Infra.Db.Account
  ( upsertAccount
  , getAllAccounts
  , getAccountByEmail
  , getAccountById
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Domain.Id (EntityId(..), newEntityId)
import Domain.Account (Account(..))
import App.Monad (App, getConn)

instance FromRow Account where
  fromRow = Account
    <$> (EntityId <$> field)  -- id
    <*> field                  -- email
    <*> field                  -- display_name
    <*> field                  -- created_at

-- Upsert account by email, returns the account (existing or new)
upsertAccount :: Text -> Maybe Text -> App Account
upsertAccount email displayName = do
  conn <- getConn
  aid <- liftIO newEntityId
  _ <- liftIO $ execute conn
    "insert into accounts (id, email, display_name) \
    \values (?, ?, ?) \
    \on conflict (email) do update set \
    \  display_name = coalesce(excluded.display_name, accounts.display_name)"
    (unEntityId aid, email, displayName)
  -- Fetch the account (might be existing or newly created)
  results <- liftIO $ query conn
    "select id, email, display_name, created_at from accounts where email = ?"
    (Only email)
  case results of
    [acc] -> pure acc
    _ -> error $ "Failed to upsert account: " <> show email

-- Get all accounts
getAllAccounts :: App [Account]
getAllAccounts = do
  conn <- getConn
  liftIO $ query_ conn
    "select id, email, display_name, created_at from accounts order by created_at"

-- Get account by email
getAccountByEmail :: Text -> App (Maybe Account)
getAccountByEmail email = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, email, display_name, created_at from accounts where email = ?"
    (Only email)
  pure $ case results of
    [acc] -> Just acc
    _ -> Nothing

-- Get account by ID
getAccountById :: EntityId -> App (Maybe Account)
getAccountById aid = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, email, display_name, created_at from accounts where id = ?"
    (Only $ unEntityId aid)
  pure $ case results of
    [acc] -> Just acc
    _ -> Nothing
