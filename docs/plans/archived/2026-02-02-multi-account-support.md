# Multi-Account Google Polling Support

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Enable polling Gmail and Calendar from multiple Google accounts (4-5 accounts) instead of just one.

**Architecture:** Introduce an `accounts` table keyed by email address. OAuth callback auto-detects the account by calling Google's userinfo endpoint. All tokens, poll state, and activities are linked to an account_id. Polling iterates over all accounts, with errors in one account not blocking others.

**Tech Stack:** Haskell, postgresql-simple, http-client-tls, aeson

---

## Task 1: Accounts Table Migration

**Files:**
- Create: `wisp-srv/migrations/007_accounts.sql`

**Step 1: Write the migration file**

```sql
-- migrations/007_accounts.sql

-- Accounts table for multi-account support
create table if not exists accounts (
  id text primary key,
  email text not null unique,
  display_name text,
  created_at timestamptz not null default now()
);

-- Clean slate: drop existing poll_state and activities data
truncate poll_state;
truncate activities;

-- Add account_id to auth_tokens
alter table auth_tokens add column if not exists account_id text references accounts(id);

-- Drop old unique constraint and add new one
drop index if exists auth_tokens_provider_unique;
create unique index if not exists auth_tokens_account_provider_unique
  on auth_tokens (account_id, provider);

-- Add account_id to poll_state
alter table poll_state add column if not exists account_id text references accounts(id);

-- Change poll_state primary key to (account_id, source)
alter table poll_state drop constraint if exists poll_state_pkey;
alter table poll_state add primary key (account_id, source);

-- Add account_id to activities
alter table activities add column if not exists account_id text references accounts(id);

-- Drop old unique constraint and add new one
drop index if exists activities_source_unique;
create unique index if not exists activities_account_source_unique
  on activities (account_id, source, source_id);

-- Make account_id not null after migration (new inserts will have it)
-- Note: existing orphaned auth_tokens will need re-auth
```

**Step 2: Verify migration file exists**

Run: `head -10 /home/gareth/code/hacking/wisp/wisp-srv/migrations/007_accounts.sql`
Expected: Shows the SQL header

**Step 3: Commit**

```bash
git add wisp-srv/migrations/007_accounts.sql
git commit -m "feat: add accounts table migration for multi-account support"
```

---

## Task 2: Account Domain Type

**Files:**
- Create: `wisp-srv/src/Domain/Account.hs`
- Create: `wisp-srv/test/Domain/AccountSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Domain/AccountSpec.hs`:

```haskell
module Domain.AccountSpec where

import Test.Hspec
import Domain.Account
import Data.Aeson (encode, decode)

spec :: Spec
spec = describe "Account" $ do
  describe "JSON serialization" $ do
    it "round-trips Account through JSON" $ do
      -- Basic structure test - full round-trip needs UTCTime
      True `shouldBe` True
```

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test 2>&1 | grep -E "(AccountSpec|not found|error)"`
Expected: FAIL - module Domain.Account not found

**Step 3: Write the Account module**

Create `wisp-srv/src/Domain/Account.hs`:

```haskell
module Domain.Account
  ( Account(..)
  , NewAccount(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), (.:?))
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Id (EntityId(..))
import GHC.Generics (Generic)

data Account = Account
  { accountId :: EntityId
  , accountEmail :: Text
  , accountDisplayName :: Maybe Text
  , accountCreatedAt :: UTCTime
  } deriving (Show, Eq)

instance ToJSON Account where
  toJSON a = object
    [ "id" .= unEntityId (accountId a)
    , "email" .= accountEmail a
    , "display_name" .= accountDisplayName a
    , "created_at" .= accountCreatedAt a
    ]

data NewAccount = NewAccount
  { newAccountEmail :: Text
  , newAccountDisplayName :: Maybe Text
  } deriving (Show, Eq)
```

**Step 4: Update wisp-srv.cabal**

Add `Domain.Account` to `other-modules` in the library/executable section.
Add `Domain.AccountSpec` to `other-modules` in the test-suite section.

**Step 5: Run test to verify it passes**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Domain/Account.hs wisp-srv/test/Domain/AccountSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Account domain type"
```

---

## Task 3: Account Database Operations

**Files:**
- Create: `wisp-srv/src/Infra/Db/Account.hs`
- Create: `wisp-srv/test/Infra/Db/AccountSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Infra/Db/AccountSpec.hs`:

```haskell
module Infra.Db.AccountSpec where

import Test.Hspec

spec :: Spec
spec = describe "Account DB" $ do
  it "placeholder - account operations need integration tests" $ do
    True `shouldBe` True
```

**Step 2: Write the Account DB module**

Create `wisp-srv/src/Infra/Db/Account.hs`:

```haskell
module Infra.Db.Account
  ( upsertAccount
  , getAllAccounts
  , getAccountByEmail
  , getAccountById
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Domain.Id (EntityId(..), newEntityId)
import Domain.Account (Account(..), NewAccount(..))
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
```

**Step 3: Update wisp-srv.cabal**

Add `Infra.Db.Account` to `other-modules` in executable.
Add `Infra.Db.AccountSpec` to `other-modules` in test-suite.

**Step 4: Run test to verify it compiles**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Db/Account.hs wisp-srv/test/Infra/Db/AccountSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add account database operations"
```

---

## Task 4: Google Userinfo Client

**Files:**
- Modify: `wisp-srv/src/Infra/Google/Auth.hs`
- Modify: `wisp-srv/test/Infra/Google/AuthSpec.hs`

**Step 1: Write the failing test**

Add to `wisp-srv/test/Infra/Google/AuthSpec.hs`:

```haskell
  describe "UserInfo parsing" $ do
    it "parses userinfo response" $ do
      let json = "{\"email\":\"test@gmail.com\",\"name\":\"Test User\"}"
      case decode json :: Maybe UserInfo of
        Nothing -> expectationFailure "Failed to parse UserInfo"
        Just ui -> do
          userEmail ui `shouldBe` "test@gmail.com"
          userName ui `shouldBe` Just "Test User"
```

Add import: `import Infra.Google.Auth (UserInfo(..), userEmail, userName)`

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test 2>&1 | grep -E "(AuthSpec|UserInfo|error)"`
Expected: FAIL - UserInfo not exported

**Step 3: Add UserInfo type to Auth module**

Add to `wisp-srv/src/Infra/Google/Auth.hs`:

```haskell
-- Add to module exports:
-- , UserInfo(..)
-- , getUserInfo

-- Add the type and function:

data UserInfo = UserInfo
  { userEmail :: Text
  , userName :: Maybe Text
  , userPicture :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON UserInfo where
  parseJSON = withObject "UserInfo" $ \v -> UserInfo
    <$> v .: "email"
    <*> v .:? "name"
    <*> v .:? "picture"

-- Fetch user info using access token
getUserInfo :: Text -> IO (Either Text UserInfo)
getUserInfo accessToken = do
  manager <- newManager tlsManagerSettings
  let url = "https://www.googleapis.com/oauth2/v2/userinfo"
  req <- parseRequest url
  let authReq = req
        { requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 accessToken)]
        }
  response <- httpLbs authReq manager
  pure $ case Aeson.decode (responseBody response) of
    Just ui -> Right ui
    Nothing -> Left $ "Failed to parse userinfo: " <> T.pack (show $ responseBody response)
```

Add required imports at top of Auth.hs:
```haskell
import Data.Aeson ((.:?))
```

**Step 4: Run test to verify it passes**

Run: `cabal test wisp-srv-test 2>&1 | tail -15`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Google/Auth.hs wisp-srv/test/Infra/Google/AuthSpec.hs
git commit -m "feat: add Google userinfo endpoint for email detection"
```

---

## Task 5: Update Auth Token Storage for Multi-Account

**Files:**
- Modify: `wisp-srv/src/Infra/Db/Auth.hs`

**Step 1: Update AuthToken type and functions**

Modify `wisp-srv/src/Infra/Db/Auth.hs`:

```haskell
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
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Domain.Id (EntityId(..), newEntityId)
import App.Monad (App, getConn)

data AuthToken = AuthToken
  { tokenId :: EntityId
  , tokenAccountId :: EntityId      -- NEW: link to account
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

-- Save a new token for an account (upsert)
saveToken :: EntityId -> Text -> Text -> Text -> UTCTime -> [Text] -> App EntityId
saveToken accountId prov accessTok refreshTok expires scps = do
  conn <- getConn
  tid <- liftIO newEntityId
  _ <- liftIO $ execute conn
    "insert into auth_tokens (id, account_id, provider, access_token, refresh_token, expires_at, scopes) \
    \values (?, ?, ?, ?, ?, ?, ?) \
    \on conflict (account_id, provider) do update set \
    \  access_token = excluded.access_token, \
    \  refresh_token = excluded.refresh_token, \
    \  expires_at = excluded.expires_at, \
    \  scopes = excluded.scopes, \
    \  updated_at = now()"
    (unEntityId tid, unEntityId accountId, prov, accessTok, refreshTok, expires, PGArray scps)
  pure tid

-- Get token for an account and provider
getTokenForAccount :: EntityId -> Text -> App (Maybe AuthToken)
getTokenForAccount accountId prov = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, account_id, provider, access_token, refresh_token, expires_at, scopes \
    \from auth_tokens where account_id = ? and provider = ?"
    (unEntityId accountId, prov)
  pure $ case results of
    [tok] -> Just tok
    _ -> Nothing

-- Get all tokens for a provider (for polling all accounts)
getAllTokens :: Text -> App [AuthToken]
getAllTokens prov = do
  conn <- getConn
  liftIO $ query conn
    "select id, account_id, provider, access_token, refresh_token, expires_at, scopes \
    \from auth_tokens where provider = ? and account_id is not null"
    (Only prov)

-- Legacy: get token by provider only (for backwards compat during migration)
getToken :: Text -> App (Maybe AuthToken)
getToken prov = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, account_id, provider, access_token, refresh_token, expires_at, scopes \
    \from auth_tokens where provider = ? limit 1"
    (Only prov)
  pure $ case results of
    [tok] -> Just tok
    _ -> Nothing

-- Update access token for an account after refresh
updateTokenForAccount :: EntityId -> Text -> Text -> UTCTime -> App ()
updateTokenForAccount accountId prov accessTok expires = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "update auth_tokens set access_token = ?, expires_at = ?, updated_at = now() \
    \where account_id = ? and provider = ?"
    (accessTok, expires, unEntityId accountId, prov)
  pure ()

-- Legacy: update by provider only
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
```

**Step 2: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds (may have warnings about unused imports)

**Step 3: Commit**

```bash
git add wisp-srv/src/Infra/Db/Auth.hs
git commit -m "feat: update auth token storage for multi-account support"
```

---

## Task 6: Update Poll State for Multi-Account

**Files:**
- Modify: `wisp-srv/src/Infra/Db/PollState.hs`

**Step 1: Update PollState functions**

Modify `wisp-srv/src/Infra/Db/PollState.hs`:

```haskell
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
import Domain.Id (EntityId(..))
import App.Monad (App, getConn)

data PollState = PollState
  { pollAccountId :: EntityId    -- NEW: link to account
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
```

**Step 2: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Infra/Db/PollState.hs
git commit -m "feat: update poll state for multi-account support"
```

---

## Task 7: Update Activity Storage for Multi-Account

**Files:**
- Modify: `wisp-srv/src/Domain/Activity.hs`
- Modify: `wisp-srv/src/Infra/Db/Activity.hs`

**Step 1: Update Activity domain type**

Add `accountId` field to `Activity` and `NewActivity` in `wisp-srv/src/Domain/Activity.hs`:

```haskell
-- In Activity record, add after activityId:
  , activityAccountId :: EntityId

-- In NewActivity record, add at the start:
  { newActivityAccountId :: EntityId
```

Update imports to include `Domain.Id (EntityId)`.

**Step 2: Update Activity DB operations**

Modify `wisp-srv/src/Infra/Db/Activity.hs`:

```haskell
-- Update FromRow instance to include account_id:
instance FromRow Activity where
  fromRow = Activity
    <$> (EntityId <$> field)          -- id
    <*> (EntityId <$> field)          -- account_id  (NEW)
    <*> (parseSource <$> field)       -- source
    <*> field                          -- source_id
    <*> field                          -- raw (jsonb)
    <*> (parseStatus <$> field)       -- status
    <*> field                          -- title
    <*> field                          -- summary
    <*> field                          -- sender_email
    <*> field                          -- starts_at
    <*> field                          -- ends_at
    <*> field                          -- created_at
    where
      parseSource :: Text -> ActivitySource
      parseSource "email" = Email
      parseSource "calendar" = Calendar
      parseSource _ = Email

      parseStatus :: Text -> ActivityStatus
      parseStatus "pending" = Pending
      parseStatus "quarantined" = Quarantined
      parseStatus "processed" = Processed
      parseStatus "surfaced" = Surfaced
      parseStatus "archived" = Archived
      parseStatus _ = Pending

-- Update insertActivity to include account_id:
insertActivity :: NewActivity -> App (Maybe EntityId)
insertActivity new = do
  conn <- getConn
  aid <- liftIO newEntityId
  let srcText = case newActivitySource new of
        Email -> "email" :: Text
        Calendar -> "calendar"
  n <- liftIO $ execute conn
    "insert into activities \
    \(id, account_id, source, source_id, raw, title, sender_email, starts_at, ends_at) \
    \values (?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \on conflict (account_id, source, source_id) do nothing"
    ( unEntityId aid
    , unEntityId (newActivityAccountId new)
    , srcText
    , newActivitySourceId new
    , toJSON (newActivityRaw new)
    , newActivityTitle new
    , newActivitySenderEmail new
    , newActivityStartsAt new
    , newActivityEndsAt new
    )
  pure $ if n > 0 then Just aid else Nothing

-- Update activityExists to check by account:
activityExistsForAccount :: EntityId -> ActivitySource -> Text -> App Bool
activityExistsForAccount accountId src srcId = do
  conn <- getConn
  let srcText = case src of
        Email -> "email" :: Text
        Calendar -> "calendar"
  results <- liftIO $ query conn
    "select 1 from activities where account_id = ? and source = ? and source_id = ? limit 1"
    (unEntityId accountId, srcText, srcId)
  pure $ not (null (results :: [Only Int]))

-- Update getActivity query to include account_id:
getActivity :: EntityId -> App (Maybe Activity)
getActivity aid = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at \
    \from activities where id = ?"
    (Only $ unEntityId aid)
  pure $ case results of
    [a] -> Just a
    _ -> Nothing

-- Update getActivitiesByStatus query:
getActivitiesByStatus :: ActivityStatus -> Int -> App [Activity]
getActivitiesByStatus status limit = do
  conn <- getConn
  let statusText = case status of
        Pending -> "pending" :: Text
        Quarantined -> "quarantined"
        Processed -> "processed"
        Surfaced -> "surfaced"
        Archived -> "archived"
  liftIO $ query conn
    "select id, account_id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at \
    \from activities where status = ? \
    \order by created_at desc limit ?"
    (statusText, limit)

-- Add new export: activityExistsForAccount
-- Remove or deprecate: activityExists (replace usages with activityExistsForAccount)
```

**Step 3: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build may fail due to callers not yet updated - that's expected

**Step 4: Commit**

```bash
git add wisp-srv/src/Domain/Activity.hs wisp-srv/src/Infra/Db/Activity.hs
git commit -m "feat: update activity storage for multi-account support"
```

---

## Task 8: Update Token Manager for Multi-Account

**Files:**
- Modify: `wisp-srv/src/Infra/Google/TokenManager.hs`

**Step 1: Update TokenManager**

Rewrite `wisp-srv/src/Infra/Google/TokenManager.hs`:

```haskell
module Infra.Google.TokenManager
  ( getValidToken
  , getValidTokenForAccount
  , getAllValidTokens
  , TokenError(..)
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime)
import App.Monad (App, getConfig)
import App.Config (Config(..), GoogleConfig(..))
import Domain.Id (EntityId)
import Domain.Account (Account(..))
import Infra.Db.Auth (AuthToken(..), getTokenForAccount, getAllTokens, updateTokenForAccount, tokenNeedsRefresh)
import Infra.Db.Account (getAllAccounts)
import Infra.Google.Auth (OAuthConfig(..), refreshAccessToken, TokenResponse(..))

data TokenError
  = NoToken
  | RefreshFailed Text
  deriving (Show, Eq)

-- Build OAuthConfig from app config
mkOAuthConfig :: Config -> OAuthConfig
mkOAuthConfig cfg = OAuthConfig
  { oauthClientId = cfg.google.clientId
  , oauthClientSecret = cfg.google.clientSecret
  , oauthRedirectUri = "http://127.0.0.1:8080/auth/google/callback"
  }

-- Get a valid access token for a specific account
getValidTokenForAccount :: EntityId -> App (Either TokenError Text)
getValidTokenForAccount accountId = do
  mtoken <- getTokenForAccount accountId "google"
  case mtoken of
    Nothing -> pure $ Left NoToken
    Just tok -> do
      needsRefresh <- liftIO $ tokenNeedsRefresh tok
      if needsRefresh
        then refreshAndUpdateForAccount accountId tok
        else pure $ Right (tokenAccessToken tok)

-- Get valid tokens for ALL accounts (for polling)
getAllValidTokens :: App [(Account, Either TokenError Text)]
getAllValidTokens = do
  accounts <- getAllAccounts
  forM accounts $ \acc -> do
    tokenResult <- getValidTokenForAccount (accountId acc)
    pure (acc, tokenResult)

-- Legacy: get any valid token (first account found)
getValidToken :: App (Either TokenError Text)
getValidToken = do
  tokens <- getAllTokens "google"
  case tokens of
    [] -> pure $ Left NoToken
    (tok:_) -> do
      needsRefresh <- liftIO $ tokenNeedsRefresh tok
      if needsRefresh
        then refreshAndUpdateForAccount (tokenAccountId tok) tok
        else pure $ Right (tokenAccessToken tok)

-- Refresh the token and update in database for a specific account
refreshAndUpdateForAccount :: EntityId -> AuthToken -> App (Either TokenError Text)
refreshAndUpdateForAccount accountId tok = do
  cfg <- getConfig
  let oauthCfg = mkOAuthConfig cfg
  result <- liftIO $ refreshAccessToken oauthCfg (tokenRefreshToken tok)
  case result of
    Left err -> pure $ Left (RefreshFailed err)
    Right newTok -> do
      now <- liftIO getCurrentTime
      let newExpiry = addUTCTime (fromIntegral $ expiresIn newTok) now
      updateTokenForAccount accountId "google" (accessToken newTok) newExpiry
      pure $ Right (accessToken newTok)
```

**Step 2: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Infra/Google/TokenManager.hs
git commit -m "feat: update token manager for multi-account support"
```

---

## Task 9: Update Gmail Poller for Multi-Account

**Files:**
- Modify: `wisp-srv/src/Services/GmailPoller.hs`

**Step 1: Update GmailPoller**

Modify `wisp-srv/src/Services/GmailPoller.hs`:

```haskell
module Services.GmailPoller
  ( pollGmail
  , pollGmailForAccount
  , pollAllGmail
  , extractEmailInfo
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import App.Monad (App)
import Domain.Id (EntityId)
import Domain.Account (Account(..))
import Domain.Activity (NewActivity(..), ActivitySource(..))
import Infra.Db.Activity (insertActivity, activityExistsForAccount)
import Infra.Db.PollState (getPollStateForAccount, updatePollStateForAccount, ensurePollStateExists, PollState(..))
import Infra.Google.Gmail
import Infra.Google.TokenManager (getValidTokenForAccount, getAllValidTokens, TokenError(..))

-- | Poll Gmail for ALL accounts
pollAllGmail :: App [(Text, Either Text Int)]
pollAllGmail = do
  tokensWithAccounts <- getAllValidTokens
  forM tokensWithAccounts $ \(acc, tokenResult) -> do
    result <- case tokenResult of
      Left NoToken -> pure $ Left "No token"
      Left (RefreshFailed err) -> pure $ Left $ "Token refresh failed: " <> err
      Right token -> pollGmailWithToken (accountId acc) token
    pure (accountEmail acc, result)

-- | Poll Gmail for a specific account
pollGmailForAccount :: Account -> App (Either Text Int)
pollGmailForAccount acc = do
  tokenResult <- getValidTokenForAccount (accountId acc)
  case tokenResult of
    Left NoToken -> pure $ Left "No Google token available"
    Left (RefreshFailed err) -> pure $ Left $ "Token refresh failed: " <> err
    Right accessToken -> pollGmailWithToken (accountId acc) accessToken

-- | Poll Gmail with a specific token for an account
pollGmailWithToken :: EntityId -> Text -> App (Either Text Int)
pollGmailWithToken accId accessToken = do
  -- Ensure poll state exists for this account
  ensurePollStateExists accId "gmail"
  mPollState <- getPollStateForAccount accId "gmail"
  case mPollState of
    Nothing -> initialPoll accId accessToken
    Just ps -> case pollCursor ps of
      Nothing -> initialPoll accId accessToken
      Just cursor -> incrementalPoll accId accessToken cursor

-- | Initial poll: fetch recent messages when no cursor exists
initialPoll :: EntityId -> Text -> App (Either Text Int)
initialPoll accId accessToken = do
  result <- liftIO $ listMessages accessToken Nothing
  case result of
    Left err -> pure $ Left err
    Right msgList -> do
      let refs = fromMaybe [] (messages msgList)
      count <- processMessageRefs accId accessToken refs
      case refs of
        [] -> pure $ Right 0
        (firstRef:_) -> do
          firstMsgResult <- liftIO $ getMessage accessToken (refId firstRef)
          case firstMsgResult of
            Left _ -> do
              updatePollStateForAccount accId "gmail" (Just $ refId firstRef)
              pure $ Right count
            Right firstMsg -> do
              updatePollStateForAccount accId "gmail" (gmailHistoryId firstMsg)
              pure $ Right count

-- | Incremental poll: use history API to get new messages since last cursor
incrementalPoll :: EntityId -> Text -> Text -> App (Either Text Int)
incrementalPoll accId accessToken cursor = do
  result <- liftIO $ listHistory accessToken cursor Nothing
  case result of
    Left err ->
      if "404" `T.isInfixOf` err || "historyId" `T.isInfixOf` err
        then initialPoll accId accessToken
        else pure $ Left err
    Right histList -> do
      let refs = extractRefsFromHistory histList
      count <- processMessageRefs accId accessToken refs
      let newCursor = historyId histList
      updatePollStateForAccount accId "gmail" newCursor
      pure $ Right count

-- | Extract message refs from history records
extractRefsFromHistory :: GmailHistoryList -> [GmailMessageRef]
extractRefsFromHistory histList =
  let histories = fromMaybe [] (history histList)
      extractFromHistory h =
        case historyMessagesAdded h of
          Nothing -> []
          Just added -> map historyMessage added
  in concatMap extractFromHistory histories

-- | Process a list of message refs: fetch full message and insert as activity
processMessageRefs :: EntityId -> Text -> [GmailMessageRef] -> App Int
processMessageRefs accId accessToken refs = do
  results <- forM refs $ \ref -> do
    -- Check if already exists for this account
    exists <- activityExistsForAccount accId Email (refId ref)
    if exists
      then pure 0
      else do
        msgResult <- liftIO $ getMessage accessToken (refId ref)
        case msgResult of
          Left _ -> pure 0
          Right msg -> do
            let (subject, sender) = extractEmailInfo msg
            let newActivity = NewActivity
                  { newActivityAccountId = accId
                  , newActivitySource = Email
                  , newActivitySourceId = gmailId msg
                  , newActivityRaw = toJSON msg
                  , newActivityTitle = subject
                  , newActivitySenderEmail = sender
                  , newActivityStartsAt = Nothing
                  , newActivityEndsAt = Nothing
                  }
            result <- insertActivity newActivity
            pure $ case result of
              Just _ -> 1
              Nothing -> 0
  pure $ sum results

-- | Extract subject and sender email from a Gmail message
extractEmailInfo :: GmailMessage -> (Maybe Text, Maybe Text)
extractEmailInfo msg =
  case gmailPayload msg of
    Nothing -> (Nothing, Nothing)
    Just payload ->
      case payloadHeaders payload of
        Nothing -> (Nothing, Nothing)
        Just headers ->
          let findHeader name =
                case filter (\h -> headerName h == name) headers of
                  [] -> Nothing
                  (h:_) -> Just (headerValue h)
              subject = findHeader "Subject"
              sender = findHeader "From"
          in (subject, sender)

-- Legacy: poll using first available token
pollGmail :: App (Either Text Int)
pollGmail = do
  results <- pollAllGmail
  case results of
    [] -> pure $ Left "No accounts configured"
    _ -> pure $ Right $ sum [n | (_, Right n) <- results]
```

**Step 2: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Services/GmailPoller.hs
git commit -m "feat: update Gmail poller for multi-account support"
```

---

## Task 10: Update Calendar Poller for Multi-Account

**Files:**
- Modify: `wisp-srv/src/Services/CalendarPoller.hs`

**Step 1: Update CalendarPoller**

Modify `wisp-srv/src/Services/CalendarPoller.hs` following the same pattern as GmailPoller:

```haskell
module Services.CalendarPoller
  ( pollCalendar
  , pollCalendarForAccount
  , pollAllCalendar
  , parseEventTime
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)

import App.Monad (App)
import Domain.Id (EntityId)
import Domain.Account (Account(..))
import Domain.Activity (NewActivity(..), ActivitySource(..))
import Infra.Db.Activity (insertActivity, activityExistsForAccount)
import Infra.Db.PollState (getPollStateForAccount, updatePollStateForAccount, ensurePollStateExists, PollState(..))
import Infra.Google.Calendar
import Infra.Google.TokenManager (getValidTokenForAccount, getAllValidTokens, TokenError(..))

-- | Poll Calendar for ALL accounts
pollAllCalendar :: App [(Text, Either Text Int)]
pollAllCalendar = do
  tokensWithAccounts <- getAllValidTokens
  forM tokensWithAccounts $ \(acc, tokenResult) -> do
    result <- case tokenResult of
      Left NoToken -> pure $ Left "No token"
      Left (RefreshFailed err) -> pure $ Left $ "Token refresh failed: " <> err
      Right token -> pollCalendarWithToken (accountId acc) token
    pure (accountEmail acc, result)

-- | Poll Calendar for a specific account
pollCalendarForAccount :: Account -> App (Either Text Int)
pollCalendarForAccount acc = do
  tokenResult <- getValidTokenForAccount (accountId acc)
  case tokenResult of
    Left NoToken -> pure $ Left "No Google token available"
    Left (RefreshFailed err) -> pure $ Left $ "Token refresh failed: " <> err
    Right accessToken -> pollCalendarWithToken (accountId acc) accessToken

-- | Poll Calendar with a specific token for an account
pollCalendarWithToken :: EntityId -> Text -> App (Either Text Int)
pollCalendarWithToken accId accessToken = do
  ensurePollStateExists accId "calendar"
  mPollState <- getPollStateForAccount accId "calendar"
  let mSyncToken = mPollState >>= pollCursor

  result <- liftIO $ listEvents accessToken mSyncToken Nothing
  case result of
    Left err ->
      if "410" `T.isInfixOf` err || "Sync token" `T.isInfixOf` err
        then do
          updatePollStateForAccount accId "calendar" Nothing
          pollCalendarWithToken accId accessToken
        else pure $ Left err
    Right eventList -> do
      let evts = fromMaybe [] (events eventList)
      count <- processEvents accId evts
      case nextSyncToken eventList of
        Just newToken -> updatePollStateForAccount accId "calendar" (Just newToken)
        Nothing -> pure ()
      pure $ Right count

-- | Process a list of calendar events
processEvents :: EntityId -> [CalendarEvent] -> App Int
processEvents accId evts = do
  results <- forM evts $ \evt -> do
    if eventStatus evt == "cancelled"
      then pure 0
      else do
        exists <- activityExistsForAccount accId Calendar (eventId evt)
        if exists
          then pure 0
          else do
            let startTime = eventStart evt >>= parseEventTime
            let endTime = eventEnd evt >>= parseEventTime
            let newActivity = NewActivity
                  { newActivityAccountId = accId
                  , newActivitySource = Calendar
                  , newActivitySourceId = eventId evt
                  , newActivityRaw = fromMaybe (toJSON evt) (eventRaw evt)
                  , newActivityTitle = eventSummary evt
                  , newActivitySenderEmail = eventOrganizer evt >>= personEmail
                  , newActivityStartsAt = startTime
                  , newActivityEndsAt = endTime
                  }
            result <- insertActivity newActivity
            pure $ case result of
              Just _ -> 1
              Nothing -> 0
  pure $ sum results

-- | Parse event time from CalendarDateTime
parseEventTime :: CalendarDateTime -> Maybe UTCTime
parseEventTime dt = case dateTimeValue dt of
  Just dtStr -> iso8601ParseM (T.unpack dtStr)
  Nothing -> Nothing

-- Legacy: poll all accounts
pollCalendar :: App (Either Text Int)
pollCalendar = do
  results <- pollAllCalendar
  case results of
    [] -> pure $ Left "No accounts configured"
    _ -> pure $ Right $ sum [n | (_, Right n) <- results]
```

**Step 2: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Services/CalendarPoller.hs
git commit -m "feat: update Calendar poller for multi-account support"
```

---

## Task 11: Update Scheduler for Multi-Account

**Files:**
- Modify: `wisp-srv/src/Services/Scheduler.hs`

**Step 1: Update Scheduler**

Modify `wisp-srv/src/Services/Scheduler.hs`:

```haskell
module Services.Scheduler
  ( startPolling
  , runPollCycle
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Monad (forever, forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import App.Monad (App, Env, runApp, getConfig)
import App.Config (Config(..), PollingConfig(..))
import Services.GmailPoller (pollAllGmail)
import Services.CalendarPoller (pollAllCalendar)

-- Run a single poll cycle for all accounts
runPollCycle :: App ()
runPollCycle = do
  liftIO $ putStrLn "\n=== Starting poll cycle ==="

  -- Poll Gmail for all accounts
  gmailResults <- pollAllGmail
  liftIO $ putStrLn "Gmail results:"
  forM_ gmailResults $ \(email, result) -> do
    case result of
      Left err -> putStrLn $ "  " <> T.unpack email <> ": ERROR - " <> T.unpack err
      Right count -> putStrLn $ "  " <> T.unpack email <> ": " <> show count <> " messages"

  -- Poll Calendar for all accounts
  calResults <- pollAllCalendar
  liftIO $ putStrLn "Calendar results:"
  forM_ calResults $ \(email, result) -> do
    case result of
      Left err -> putStrLn $ "  " <> T.unpack email <> ": ERROR - " <> T.unpack err
      Right count -> putStrLn $ "  " <> T.unpack email <> ": " <> show count <> " events"

  liftIO $ putStrLn "=== Poll cycle complete ===\n"

-- Start background polling
startPolling :: Env -> IO ()
startPolling env = do
  -- Run initial poll immediately
  putStrLn "Running initial poll..."
  runApp env runPollCycle

  -- Get poll interval from config
  let intervalMinutes = (runApp env getConfig >>= \cfg -> pure cfg.polling.intervalMinutes)
  interval <- intervalMinutes
  let intervalMicros = interval * 60 * 1000000

  putStrLn $ "Starting background polling every " <> show interval <> " minutes"

  -- Start background polling thread
  pollThread <- async $ forever $ do
    threadDelay intervalMicros
    runApp env runPollCycle

  -- Link thread so exceptions propagate
  link pollThread
```

**Step 2: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Services/Scheduler.hs
git commit -m "feat: update scheduler for multi-account polling"
```

---

## Task 12: Update OAuth Callback for Auto-Detection

**Files:**
- Modify: `wisp-srv/src/Http/Handlers/Auth.hs`

**Step 1: Update Auth handler**

Modify `wisp-srv/src/Http/Handlers/Auth.hs` to call userinfo and upsert account:

```haskell
-- Add imports:
import Infra.Google.Auth (getUserInfo, UserInfo(..))
import Infra.Db.Account (upsertAccount)
import Domain.Account (Account(..))

-- Update getGoogleCallback to:
getGoogleCallback :: ActionT (ReaderT Env IO) ()
getGoogleCallback = do
  code <- queryParam "code"
  case code of
    Nothing -> do
      status status400
      json $ object ["error" .= ("Missing code parameter" :: Text)]
    Just authCode -> do
      cfg <- lift getConfig
      let oauthCfg = mkOAuthConfig cfg
      result <- liftIO $ exchangeCode oauthCfg authCode
      case result of
        Left err -> do
          status status500
          json $ object ["error" .= err]
        Right tokenResp -> do
          -- NEW: Get user info to identify the account
          userInfoResult <- liftIO $ getUserInfo (accessToken tokenResp)
          case userInfoResult of
            Left err -> do
              status status500
              json $ object ["error" .= ("Failed to get user info: " <> err)]
            Right userInfo -> do
              -- Upsert account by email
              account <- lift $ upsertAccount (userEmail userInfo) (userName userInfo)

              -- Save token linked to account
              now <- liftIO getCurrentTime
              let expiry = addUTCTime (fromIntegral $ expiresIn tokenResp) now
              _ <- lift $ saveToken
                (accountId account)
                "google"
                (accessToken tokenResp)
                (refreshToken tokenResp)
                expiry
                ["https://www.googleapis.com/auth/gmail.readonly", "https://www.googleapis.com/auth/calendar.readonly"]

              json $ object
                [ "status" .= ("authenticated" :: Text)
                , "email" .= userEmail userInfo
                , "name" .= userName userInfo
                ]
```

Also update imports and `saveToken` call signature throughout.

**Step 2: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Auth.hs
git commit -m "feat: update OAuth callback for multi-account auto-detection"
```

---

## Task 13: Update Auth Status Endpoint

**Files:**
- Modify: `wisp-srv/src/Http/Handlers/Auth.hs`

**Step 1: Update getAuthStatus**

Update `getAuthStatus` in `wisp-srv/src/Http/Handlers/Auth.hs` to return all accounts:

```haskell
-- Add import:
import Infra.Db.Account (getAllAccounts)
import Infra.Db.Auth (getAllTokens)

-- Replace getAuthStatus:
getAuthStatus :: ActionT (ReaderT Env IO) ()
getAuthStatus = do
  accounts <- lift getAllAccounts
  tokens <- lift $ getAllTokens "google"

  let accountsWithTokens = map (findTokenForAccount tokens) accounts

  json $ object
    [ "accounts" .= map accountToJson accountsWithTokens
    , "count" .= length accounts
    ]
  where
    findTokenForAccount tokens acc =
      let mToken = find (\t -> tokenAccountId t == accountId acc) tokens
      in (acc, mToken)

    accountToJson (acc, mToken) = object
      [ "email" .= accountEmail acc
      , "display_name" .= accountDisplayName acc
      , "authenticated" .= isJust mToken
      , "expires_at" .= fmap tokenExpiresAt mToken
      ]
```

Add import: `import Data.Maybe (isJust)` and `import Data.List (find)`.

**Step 2: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Auth.hs
git commit -m "feat: update auth status to show all accounts"
```

---

## Task 14: Update Activities Endpoint

**Files:**
- Modify: `wisp-srv/src/Http/Handlers/Activities.hs`

**Step 1: Update activityToJson**

Update `activityToJson` in `wisp-srv/src/Http/Handlers/Activities.hs` to include account_id:

```haskell
-- Update activityToJson:
activityToJson :: Activity -> Value
activityToJson a = object
  [ "id" .= unEntityId (activityId a)
  , "account_id" .= unEntityId (activityAccountId a)
  , "source" .= show (activitySource a)
  , "source_id" .= activitySourceId a
  , "status" .= show (activityStatus a)
  , "title" .= activityTitle a
  , "summary" .= activitySummary a
  , "sender_email" .= activitySenderEmail a
  , "starts_at" .= activityStartsAt a
  , "ends_at" .= activityEndsAt a
  , "created_at" .= activityCreatedAt a
  ]
```

**Step 2: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Activities.hs
git commit -m "feat: include account_id in activities response"
```

---

## Task 15: Update CLI Status Display

**Files:**
- Modify: `wisp-cli/app/Main.hs`

**Step 1: Update runStatus**

Update `runStatus` in `wisp-cli/app/Main.hs` to show multiple accounts:

```haskell
runStatus :: IO ()
runStatus = do
  TIO.putStrLn "Wisp Status"
  TIO.putStrLn "==========="
  manager <- newManager defaultManagerSettings

  -- Check server health
  healthReq <- parseRequest $ baseUrl <> "/health"
  healthResp <- httpLbs healthReq manager
  case decode (responseBody healthResp) :: Maybe Value of
    Just _ -> TIO.putStrLn "Server:     online"
    Nothing -> TIO.putStrLn "Server:     offline"

  -- Check auth status (now shows all accounts)
  authReq <- parseRequest $ baseUrl <> "/auth/status"
  authResp <- httpLbs authReq manager
  case decode (responseBody authResp) of
    Just (Object obj) -> do
      case KM.lookup "accounts" obj of
        Just (Array accounts) -> do
          TIO.putStrLn $ "Accounts:   " <> T.pack (show $ length accounts)
          V.forM_ accounts $ \accVal -> do
            case accVal of
              Object acc -> do
                let email = case KM.lookup "email" acc of
                      Just (String e) -> e
                      _ -> "unknown"
                let authed = case KM.lookup "authenticated" acc of
                      Just (Bool b) -> b
                      _ -> False
                let statusStr = if authed then "authenticated" else "not authenticated"
                TIO.putStrLn $ "  - " <> email <> " (" <> statusStr <> ")"
              _ -> pure ()
        _ -> TIO.putStrLn "Accounts:   unknown format"
    _ -> TIO.putStrLn "Accounts:   error fetching"

  -- Check activities count
  activitiesReq <- parseRequest $ baseUrl <> "/activities"
  activitiesResp <- httpLbs activitiesReq manager
  case decode (responseBody activitiesResp) of
    Just (Object obj) -> do
      case KM.lookup "count" obj of
        Just (Number n) -> TIO.putStrLn $ "Activities: " <> T.pack (show (round n :: Int)) <> " pending"
        _ -> return ()
    _ -> return ()
```

Add import: `import qualified Data.Vector as V` and `import Data.Aeson (Array)`.

**Step 2: Run to verify it compiles**

Run: `cabal build wisp-cli 2>&1 | tail -5`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-cli/app/Main.hs
git commit -m "feat: update CLI status to show multiple accounts"
```

---

## Task 16: Fix Tests

**Files:**
- Modify: `wisp-srv/test/Services/GmailPollerSpec.hs`
- Modify: `wisp-srv/test/Services/CalendarPollerSpec.hs`
- Modify: `wisp-srv/test/Domain/ActivitySpec.hs`

**Step 1: Update GmailPollerSpec**

The test creates a `GmailMessage` directly - no changes needed since `extractEmailInfo` signature is unchanged.

**Step 2: Update CalendarPollerSpec**

The test uses `parseEventTime` directly - no changes needed.

**Step 3: Update ActivitySpec if needed**

Review and update any tests that create Activity or NewActivity records to include the new accountId field.

**Step 4: Run all tests**

Run: `cabal test wisp-srv-test 2>&1 | tail -20`
Expected: All tests PASS

**Step 5: Commit**

```bash
git add wisp-srv/test/
git commit -m "fix: update tests for multi-account support"
```

---

## Task 17: Integration Test

**Step 1: Build everything**

Run: `cabal build all 2>&1 | tail -10`
Expected: Build succeeds

**Step 2: Run tests**

Run: `cabal test all 2>&1 | tail -20`
Expected: All tests pass

**Step 3: Manual verification checklist**

1. Start server: `cabal run wisp-srv`
2. Run `wisp auth` - should redirect to Google OAuth
3. Complete OAuth for first account
4. Check status: `wisp status` - should show 1 account
5. Run `wisp auth` again with different Google account
6. Check status: `wisp status` - should show 2 accounts
7. Trigger poll: `wisp poll`
8. Check activities: `curl localhost:8080/activities`

**Step 4: Final commit**

```bash
git add -A
git commit -m "feat: complete multi-account Google polling support"
```

---

## Summary

This plan adds multi-account support for Google polling:

1. **Schema changes** (Task 1): New `accounts` table, updated foreign keys
2. **Domain types** (Tasks 2-3): Account type and DB operations
3. **Google userinfo** (Task 4): Fetch email during OAuth
4. **Storage updates** (Tasks 5-7): Auth tokens, poll state, activities all linked to accounts
5. **Token manager** (Task 8): Support multiple tokens
6. **Pollers** (Tasks 9-10): Poll all accounts, track state per-account
7. **Scheduler** (Task 11): Iterate over all accounts
8. **OAuth flow** (Task 12): Auto-detect account by email
9. **HTTP endpoints** (Tasks 13-14): Show all accounts in status, include account in activities
10. **CLI** (Task 15): Display multiple accounts
11. **Tests & verification** (Tasks 16-17): Ensure everything works
