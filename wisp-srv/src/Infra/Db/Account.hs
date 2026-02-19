module Infra.Db.Account
  ( upsertAccount
  , upsertAccountByProvider
  , getAllAccounts
  , getAccountsByProvider
  , getAccountByEmail
  , getAccountByGitHubUsername
  , getAccountById
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow(..), fieldWith, field)
import Database.PostgreSQL.Simple.FromField (fromField, returnError, Conversion, FromField(..))
import Database.PostgreSQL.Simple.ToField (toField, ToField(..))
import Domain.Id (EntityId(..), newEntityId)
import Domain.Account (Account(..), AccountProvider(..))
import App.Monad (App, getConn)
import qualified Data.Aeson as Aeson

-- ToField for AccountProvider
instance ToField AccountProvider where
  toField Google = toField ("google" :: Text)
  toField GitHub = toField ("github" :: Text)

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

-- Upsert account by provider and identifier (generic)
upsertAccountByProvider :: AccountProvider -> Value -> Maybe Text -> App Account
upsertAccountByProvider provider details displayName = do
  conn <- getConn
  aid <- liftIO newEntityId
  let detailsJson = LBS.toStrict $ encode details
  _ <- liftIO $ execute conn
    "INSERT INTO accounts (id, provider, display_name, details) \
    \VALUES (?, ?, ?, ?::jsonb) \
    \ON CONFLICT (provider, (details->>'email')) DO UPDATE SET \
    \  display_name = COALESCE(EXCLUDED.display_name, accounts.display_name), \
    \  details = EXCLUDED.details"
    (unEntityId aid, provider, displayName, detailsJson)
  -- Fetch the account by provider and details
  results <- liftIO $ query conn
    "SELECT id, provider, display_name, details, created_at \
    \FROM accounts WHERE provider = ? AND details = ?::jsonb"
    (provider, detailsJson)
  case results of
    [acc] -> pure acc
    _ -> error "Failed to upsert account"

-- Legacy upsert by email (for Google accounts)
upsertAccount :: Text -> Maybe Text -> App Account
upsertAccount email displayName = do
  let details = Aeson.object ["email" Aeson..= email]
  upsertAccountByProvider Google details displayName

-- Get all accounts
getAllAccounts :: App [Account]
getAllAccounts = do
  conn <- getConn
  liftIO $ query_ conn
    "SELECT id, provider, display_name, details, created_at FROM accounts ORDER BY created_at"

-- Get accounts by provider
getAccountsByProvider :: AccountProvider -> App [Account]
getAccountsByProvider provider = do
  conn <- getConn
  liftIO $ query conn
    "SELECT id, provider, display_name, details, created_at \
    \FROM accounts WHERE provider = ? ORDER BY created_at"
    (Only provider)

-- Get account by email (Google accounts)
getAccountByEmail :: Text -> App (Maybe Account)
getAccountByEmail email = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT id, provider, display_name, details, created_at \
    \FROM accounts WHERE provider = 'google' AND details->>'email' = ?"
    (Only email)
  pure $ case results of
    [acc] -> Just acc
    _ -> Nothing

-- Get account by GitHub username
getAccountByGitHubUsername :: Text -> App (Maybe Account)
getAccountByGitHubUsername username = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT id, provider, display_name, details, created_at \
    \FROM accounts WHERE provider = 'github' AND details->>'username' = ?"
    (Only username)
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
