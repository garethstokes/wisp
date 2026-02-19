# GitHub Integration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add GitHub OAuth integration with hourly events polling, storing all GitHub activity as activities.

**Architecture:** Extend the existing Account model with provider support, add GitHub OAuth flow mirroring the Google pattern, create Events API client with ETag caching, and wire into the existing scheduler.

**Tech Stack:** Haskell, PostgreSQL (JSONB), http-client-tls, aeson

---

## Task 1: Database Migration - Account Providers

**Files:**
- Create: `wisp-srv/migrations/016_account_providers.sql`

**Step 1: Write migration SQL**

```sql
-- 016_account_providers.sql
-- Add provider support to accounts table

-- Add provider column (default 'google' for existing accounts)
ALTER TABLE accounts ADD COLUMN provider TEXT NOT NULL DEFAULT 'google';

-- Add details JSONB column for provider-specific data
ALTER TABLE accounts ADD COLUMN details JSONB NOT NULL DEFAULT '{}';

-- Migrate existing email data to details JSON
UPDATE accounts SET details = jsonb_build_object('email', email);

-- Drop the email column
ALTER TABLE accounts DROP COLUMN email;

-- Create index on provider for filtering
CREATE INDEX idx_accounts_provider ON accounts(provider);
```

**Step 2: Verify migration syntax**

Run: `psql $DATABASE_URL -c "\d accounts"` to see current schema before applying.

**Step 3: Commit**

```bash
git add wisp-srv/migrations/016_account_providers.sql
git commit -m "feat(db): add provider and details columns to accounts"
```

---

## Task 2: Domain - AccountProvider Type

**Files:**
- Modify: `wisp-srv/src/Domain/Account.hs`
- Modify: `wisp-srv/test/Domain/AccountSpec.hs`

**Step 1: Write failing test for AccountProvider**

Add to `wisp-srv/test/Domain/AccountSpec.hs`:

```haskell
module Domain.AccountSpec where

import Test.Hspec
import Domain.Account
import Domain.Id (EntityId(..))
import Data.Aeson (encode, decode, object, (.=))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.ByteString.Lazy.Char8 as LBS

spec :: Spec
spec = describe "Account" $ do
  describe "AccountProvider" $ do
    it "serializes Google provider to JSON" $ do
      let json = LBS.unpack (encode Google)
      json `shouldBe` "\"google\""

    it "serializes GitHub provider to JSON" $ do
      let json = LBS.unpack (encode GitHub)
      json `shouldBe` "\"github\""

    it "parses provider from JSON" $ do
      decode "\"google\"" `shouldBe` Just Google
      decode "\"github\"" `shouldBe` Just GitHub

  describe "JSON serialization" $ do
    it "encodes Account with provider and details" $ do
      let testTime = posixSecondsToUTCTime 1704067200
          details = object ["email" .= ("test@example.com" :: String)]
          account = Account
            { accountId = EntityId "abc123"
            , accountProvider = Google
            , accountDisplayName = Just "Test User"
            , accountDetails = details
            , accountCreatedAt = testTime
            }
          json = LBS.unpack (encode account)
      json `shouldContain` "\"provider\":\"google\""
      json `shouldContain` "\"details\""
      json `shouldContain` "\"test@example.com\""

    it "encodes GitHub account with username in details" $ do
      let testTime = posixSecondsToUTCTime 1704067200
          details = object ["username" .= ("garethstokes" :: String)]
          account = Account
            { accountId = EntityId "xyz789"
            , accountProvider = GitHub
            , accountDisplayName = Just "Gareth"
            , accountDetails = details
            , accountCreatedAt = testTime
            }
          json = LBS.unpack (encode account)
      json `shouldContain` "\"provider\":\"github\""
      json `shouldContain` "\"garethstokes\""
```

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test --test-option=--match="/Account/"`
Expected: FAIL - AccountProvider not defined

**Step 3: Update Domain/Account.hs**

Replace `wisp-srv/src/Domain/Account.hs`:

```haskell
module Domain.Account
  ( Account(..)
  , AccountProvider(..)
  , NewAccount(..)
  , accountIdentifier
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value, object, withText, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Id (EntityId(..))
import GHC.Generics (Generic)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson as Aeson

data AccountProvider = Google | GitHub
  deriving (Eq, Show, Generic)

instance ToJSON AccountProvider where
  toJSON Google = "google"
  toJSON GitHub = "github"

instance FromJSON AccountProvider where
  parseJSON = withText "AccountProvider" $ \case
    "google" -> pure Google
    "github" -> pure GitHub
    other -> fail $ "Unknown provider: " <> show other

data Account = Account
  { accountId :: EntityId
  , accountProvider :: AccountProvider
  , accountDisplayName :: Maybe Text
  , accountDetails :: Value  -- JSON: provider-specific data
  , accountCreatedAt :: UTCTime
  } deriving (Show, Eq)

instance ToJSON Account where
  toJSON a = object
    [ "id" .= unEntityId (accountId a)
    , "provider" .= accountProvider a
    , "display_name" .= accountDisplayName a
    , "details" .= accountDetails a
    , "created_at" .= accountCreatedAt a
    ]

-- Helper to get primary identifier (email for Google, username for GitHub)
accountIdentifier :: Account -> Maybe Text
accountIdentifier acc = case accountDetails acc of
  Aeson.Object obj -> case accountProvider acc of
    Google -> case KM.lookup "email" obj of
      Just (Aeson.String e) -> Just e
      _ -> Nothing
    GitHub -> case KM.lookup "username" obj of
      Just (Aeson.String u) -> Just u
      _ -> Nothing
  _ -> Nothing

data NewAccount = NewAccount
  { newAccountProvider :: AccountProvider
  , newAccountDisplayName :: Maybe Text
  , newAccountDetails :: Value
  } deriving (Show, Eq)
```

**Step 4: Run test to verify it passes**

Run: `cabal test wisp-srv-test --test-option=--match="/Account/"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Domain/Account.hs wisp-srv/test/Domain/AccountSpec.hs
git commit -m "feat(domain): add AccountProvider and details to Account"
```

---

## Task 3: Domain - Add GitHubEvent to ActivitySource

**Files:**
- Modify: `wisp-srv/src/Domain/Activity.hs`
- Modify: `wisp-srv/test/Domain/ActivitySpec.hs`

**Step 1: Write failing test**

Add test case to `wisp-srv/test/Domain/ActivitySpec.hs`:

```haskell
  describe "ActivitySource" $ do
    it "serializes GitHubEvent to JSON" $ do
      let json = encode GitHubEvent
      json `shouldBe` "\"github_event\""

    it "parses GitHubEvent from JSON" $ do
      decode "\"github_event\"" `shouldBe` Just GitHubEvent
```

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test --test-option=--match="/ActivitySource/"`
Expected: FAIL - GitHubEvent not defined

**Step 3: Update Domain/Activity.hs**

Add `GitHubEvent` to the `ActivitySource` type:

```haskell
data ActivitySource = Email | Calendar | Conversation | Note | GitHubEvent
  deriving (Eq, Show, Generic)

instance ToJSON ActivitySource where
  toJSON Email = "email"
  toJSON Calendar = "calendar"
  toJSON Conversation = "conversation"
  toJSON Note = "note"
  toJSON GitHubEvent = "github_event"

instance FromJSON ActivitySource where
  parseJSON = withText "ActivitySource" $ \case
    "email" -> pure Email
    "calendar" -> pure Calendar
    "conversation" -> pure Conversation
    "note" -> pure Note
    "github_event" -> pure GitHubEvent
    _ -> fail "Invalid activity source"
```

**Step 4: Run test to verify it passes**

Run: `cabal test wisp-srv-test --test-option=--match="/ActivitySource/"`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Domain/Activity.hs wisp-srv/test/Domain/ActivitySpec.hs
git commit -m "feat(domain): add GitHubEvent to ActivitySource"
```

---

## Task 4: Infra/Db - Update Account Database Layer

**Files:**
- Modify: `wisp-srv/src/Infra/Db/Account.hs`

**Step 1: Update FromRow instance and queries**

Replace `wisp-srv/src/Infra/Db/Account.hs`:

```haskell
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
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField (fromField, FromField(..))
import Database.PostgreSQL.Simple.ToField (toField, ToField(..), Action(..))
import Domain.Id (EntityId(..), newEntityId)
import Domain.Account (Account(..), AccountProvider(..), NewAccount(..))
import App.Monad (App, getConn)
import qualified Data.Aeson as Aeson

-- FromField for AccountProvider
instance FromField AccountProvider where
  fromField f mdata = do
    txt <- fromField f mdata :: Conversion Text
    case txt of
      "google" -> pure Google
      "github" -> pure GitHub
      other -> returnError ConversionFailed f $ "Unknown provider: " <> show other

-- ToField for AccountProvider
instance ToField AccountProvider where
  toField Google = toField ("google" :: Text)
  toField GitHub = toField ("github" :: Text)

-- FromField for JSON Value (details column)
instance FromField Value where
  fromField f mdata = do
    bs <- fromField f mdata :: Conversion LBS.ByteString
    case Aeson.decode bs of
      Just v -> pure v
      Nothing -> returnError ConversionFailed f "Invalid JSON in details"

-- ToField for JSON Value
instance ToField Value where
  toField v = toField (LBS.toStrict $ encode v)

instance FromRow Account where
  fromRow = Account
    <$> (EntityId <$> field)  -- id
    <*> field                  -- provider
    <*> field                  -- display_name
    <*> field                  -- details
    <*> field                  -- created_at

-- Upsert account by provider and identifier (generic)
upsertAccountByProvider :: AccountProvider -> Value -> Maybe Text -> App Account
upsertAccountByProvider provider details displayName = do
  conn <- getConn
  aid <- liftIO newEntityId
  _ <- liftIO $ execute conn
    "INSERT INTO accounts (id, provider, display_name, details) \
    \VALUES (?, ?, ?, ?) \
    \ON CONFLICT (provider, (details->>'email')) DO UPDATE SET \
    \  display_name = COALESCE(EXCLUDED.display_name, accounts.display_name), \
    \  details = EXCLUDED.details"
    (unEntityId aid, provider, displayName, details)
  -- Fetch the account
  results <- liftIO $ query conn
    "SELECT id, provider, display_name, details, created_at \
    \FROM accounts WHERE provider = ? AND details = ?"
    (provider, details)
  case results of
    [acc] -> pure acc
    _ -> error $ "Failed to upsert account"

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
    "SELECT id, provider, display_name, details, created_at \
    \FROM accounts ORDER BY created_at"

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
    "SELECT id, provider, display_name, details, created_at \
    \FROM accounts WHERE id = ?"
    (Only $ unEntityId aid)
  pure $ case results of
    [acc] -> Just acc
    _ -> Nothing
```

**Step 2: Build to verify compilation**

Run: `cabal build wisp-srv`
Expected: Compiles successfully (may have warnings about changed imports elsewhere)

**Step 3: Commit**

```bash
git add wisp-srv/src/Infra/Db/Account.hs
git commit -m "feat(db): update Account queries for provider model"
```

---

## Task 5: Config - Add GitHub OAuth Config

**Files:**
- Modify: `wisp-srv/src/App/Config.hs`

**Step 1: Add GitHubConfig type and update Config**

Add to `wisp-srv/src/App/Config.hs` after GoogleConfig:

```haskell
data GitHubConfig = GitHubConfig
  { clientId :: Text
  , clientSecret :: Text
  } deriving (Generic)

instance FromJSON GitHubConfig

instance Show GitHubConfig where
  show c = "GitHubConfig {clientId = " <> show (clientId c) <> ", clientSecret = \"<redacted>\"}"
```

Update the `Config` record:

```haskell
data Config = Config
  { server :: ServerConfig
  , database :: DatabaseConfig
  , google :: GoogleConfig
  , github :: Maybe GitHubConfig  -- Optional, may not be configured
  , polling :: PollingConfig
  , classification :: ClassificationConfig
  , claude :: ClaudeConfig
  , notifications :: Maybe NotificationConfig
  } deriving (Generic, Show)
```

Update `loadConfig` to read GitHub env vars:

```haskell
loadConfig :: FilePath -> IO Config
loadConfig path = do
  result <- decodeFileEither path
  case result of
    Left err -> error $ "Config error: " <> show err
    Right cfg -> do
      mDbUrl <- lookupEnv "DATABASE_URL"
      mPort <- lookupEnv "PORT"
      mGoogleClientId <- lookupEnv "GOOGLE_CLIENT_ID"
      mGoogleClientSecret <- lookupEnv "GOOGLE_CLIENT_SECRET"
      mGitHubClientId <- lookupEnv "GITHUB_CLIENT_ID"
      mGitHubClientSecret <- lookupEnv "GITHUB_CLIENT_SECRET"
      mAnthropicApiKey <- lookupEnv "ANTHROPIC_API_KEY"

      let githubConfig = case (mGitHubClientId, mGitHubClientSecret) of
            (Just cid, Just cs) -> Just $ GitHubConfig (T.pack cid) (T.pack cs)
            _ -> cfg.github

      pure cfg
        { database = cfg.database
            { url = maybe cfg.database.url T.pack mDbUrl
            }
        , server = cfg.server
            { port = maybe cfg.server.port read mPort
            }
        , google = cfg.google
            { clientId = maybe cfg.google.clientId T.pack mGoogleClientId
            , clientSecret = maybe cfg.google.clientSecret T.pack mGoogleClientSecret
            }
        , github = githubConfig
        , claude = cfg.claude
            { apiKey = maybe cfg.claude.apiKey T.pack mAnthropicApiKey
            }
        }
```

**Step 2: Build to verify compilation**

Run: `cabal build wisp-srv`

**Step 3: Commit**

```bash
git add wisp-srv/src/App/Config.hs
git commit -m "feat(config): add GitHub OAuth config"
```

---

## Task 6: Infra/GitHub - Auth Module

**Files:**
- Create: `wisp-srv/src/Infra/GitHub/Auth.hs`
- Create: `wisp-srv/test/Infra/GitHub/AuthSpec.hs`

**Step 1: Create directory**

Run: `mkdir -p wisp-srv/src/Infra/GitHub wisp-srv/test/Infra/GitHub`

**Step 2: Write failing test**

Create `wisp-srv/test/Infra/GitHub/AuthSpec.hs`:

```haskell
module Infra.GitHub.AuthSpec where

import Test.Hspec
import Infra.GitHub.Auth
import Data.Aeson (decode)
import Data.Text (isInfixOf)

spec :: Spec
spec = describe "GitHub Auth" $ do
  describe "buildAuthUrl" $ do
    it "includes client_id in URL" $ do
      let cfg = GitHubOAuthConfig
            { ghClientId = "test-client-id"
            , ghClientSecret = "test-secret"
            , ghRedirectUri = "http://localhost:8080/auth/github/callback"
            }
      let url = buildGitHubAuthUrl cfg
      "test-client-id" `isInfixOf` url `shouldBe` True

    it "includes read:user scope" $ do
      let cfg = GitHubOAuthConfig
            { ghClientId = "test-id"
            , ghClientSecret = "test-secret"
            , ghRedirectUri = "http://localhost:8080/auth/github/callback"
            }
      let url = buildGitHubAuthUrl cfg
      "read%3Auser" `isInfixOf` url `shouldBe` True

  describe "GitHubUser parsing" $ do
    it "parses user response" $ do
      let json = "{\"login\":\"garethstokes\",\"name\":\"Gareth Stokes\",\"avatar_url\":\"https://example.com/avatar.png\"}"
      case decode json :: Maybe GitHubUser of
        Nothing -> expectationFailure "Failed to parse GitHubUser"
        Just u -> do
          ghUserLogin u `shouldBe` "garethstokes"
          ghUserName u `shouldBe` Just "Gareth Stokes"
```

**Step 3: Run test to verify it fails**

Run: `cabal test wisp-srv-test --test-option=--match="/GitHub Auth/"`
Expected: FAIL - module not found

**Step 4: Create Infra/GitHub/Auth.hs**

Create `wisp-srv/src/Infra/GitHub/Auth.hs`:

```haskell
module Infra.GitHub.Auth
  ( GitHubOAuthConfig(..)
  , GitHubTokenResponse(..)
  , GitHubUser(..)
  , buildGitHubAuthUrl
  , exchangeGitHubCode
  , getGitHubUser
  ) where

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

data GitHubOAuthConfig = GitHubOAuthConfig
  { ghClientId :: Text
  , ghClientSecret :: Text
  , ghRedirectUri :: Text
  } deriving (Show)

data GitHubTokenResponse = GitHubTokenResponse
  { ghAccessToken :: Text
  , ghTokenType :: Text
  , ghScope :: Text
  } deriving (Show)

instance FromJSON GitHubTokenResponse where
  parseJSON = withObject "GitHubTokenResponse" $ \v -> GitHubTokenResponse
    <$> v .: "access_token"
    <*> v .: "token_type"
    <*> v .: "scope"

data GitHubUser = GitHubUser
  { ghUserLogin :: Text
  , ghUserName :: Maybe Text
  , ghUserAvatarUrl :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON GitHubUser where
  parseJSON = withObject "GitHubUser" $ \v -> GitHubUser
    <$> v .: "login"
    <*> v .:? "name"
    <*> v .:? "avatar_url"

-- URL-encode a text value
urlEncode :: Text -> Text
urlEncode = T.pack . concatMap encodeChar . T.unpack
  where
    encodeChar c
      | c >= 'a' && c <= 'z' = [c]
      | c >= 'A' && c <= 'Z' = [c]
      | c >= '0' && c <= '9' = [c]
      | c `elem` ['-', '_', '.', '~'] = [c]
      | otherwise = '%' : showHex2 (fromEnum c)
    showHex2 n = let (q, r) = n `divMod` 16
                 in [hexDigit q, hexDigit r]
    hexDigit n
      | n < 10 = toEnum (fromEnum '0' + n)
      | otherwise = toEnum (fromEnum 'A' + n - 10)

-- Build the GitHub OAuth authorization URL
buildGitHubAuthUrl :: GitHubOAuthConfig -> Text
buildGitHubAuthUrl cfg = T.concat
  [ "https://github.com/login/oauth/authorize"
  , "?client_id=", urlEncode (ghClientId cfg)
  , "&redirect_uri=", urlEncode (ghRedirectUri cfg)
  , "&scope=", urlEncode "read:user"
  ]

-- Exchange authorization code for access token
exchangeGitHubCode :: GitHubOAuthConfig -> Text -> IO (Either Text GitHubTokenResponse)
exchangeGitHubCode cfg code = do
  manager <- newManager tlsManagerSettings
  let body = BS.intercalate "&"
        [ "client_id=" <> encodeUtf8 (ghClientId cfg)
        , "client_secret=" <> encodeUtf8 (ghClientSecret cfg)
        , "code=" <> encodeUtf8 code
        , "redirect_uri=" <> encodeUtf8 (ghRedirectUri cfg)
        ]
  initialReq <- parseRequest "https://github.com/login/oauth/access_token"
  let req = initialReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/x-www-form-urlencoded")
            , ("Accept", "application/json")
            ]
        , requestBody = RequestBodyBS body
        }
  response <- httpLbs req manager
  pure $ case Aeson.decode (responseBody response) of
    Just tok -> Right tok
    Nothing -> Left $ "Failed to parse token response: " <> T.pack (show $ responseBody response)

-- Fetch GitHub user info using access token
getGitHubUser :: Text -> IO (Either Text GitHubUser)
getGitHubUser accessToken = do
  manager <- newManager tlsManagerSettings
  let url = "https://api.github.com/user"
  req <- parseRequest url
  let authReq = req
        { requestHeaders =
            [ ("Authorization", "Bearer " <> encodeUtf8 accessToken)
            , ("User-Agent", "wisp-srv")
            ]
        }
  response <- httpLbs authReq manager
  pure $ case Aeson.decode (responseBody response) of
    Just ui -> Right ui
    Nothing -> Left $ "Failed to parse user info: " <> T.pack (show $ responseBody response)
```

**Step 5: Add to cabal file**

Add to `wisp-srv.cabal` in `other-modules`:
- `Infra.GitHub.Auth`

Add to test suite `other-modules`:
- `Infra.GitHub.AuthSpec`

**Step 6: Run test to verify it passes**

Run: `cabal test wisp-srv-test --test-option=--match="/GitHub Auth/"`
Expected: PASS

**Step 7: Commit**

```bash
git add wisp-srv/src/Infra/GitHub/Auth.hs wisp-srv/test/Infra/GitHub/AuthSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(github): add OAuth auth module"
```

---

## Task 7: Infra/GitHub - Events Module

**Files:**
- Create: `wisp-srv/src/Infra/GitHub/Events.hs`
- Create: `wisp-srv/test/Infra/GitHub/EventsSpec.hs`

**Step 1: Write failing test**

Create `wisp-srv/test/Infra/GitHub/EventsSpec.hs`:

```haskell
module Infra.GitHub.EventsSpec where

import Test.Hspec
import Infra.GitHub.Events
import Data.Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS

spec :: Spec
spec = describe "GitHub Events" $ do
  describe "GitHubEvent parsing" $ do
    it "parses PushEvent" $ do
      let json = LBS.pack $ unlines
            [ "{"
            , "  \"id\": \"12345678901\","
            , "  \"type\": \"PushEvent\","
            , "  \"actor\": { \"login\": \"garethstokes\" },"
            , "  \"repo\": { \"name\": \"org/repo\" },"
            , "  \"payload\": { \"commits\": [] },"
            , "  \"created_at\": \"2025-02-19T10:00:00Z\""
            , "}"
            ]
      case eitherDecode json :: Either String GitHubEvent of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right e -> do
          ghEventId e `shouldBe` "12345678901"
          ghEventType e `shouldBe` "PushEvent"
          ghEventRepo e `shouldBe` "org/repo"

    it "parses event list" $ do
      let json = LBS.pack "[{\"id\":\"1\",\"type\":\"PushEvent\",\"actor\":{\"login\":\"u\"},\"repo\":{\"name\":\"o/r\"},\"payload\":{},\"created_at\":\"2025-01-01T00:00:00Z\"}]"
      case eitherDecode json :: Either String [GitHubEvent] of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right events -> length events `shouldBe` 1

  describe "extractTitle" $ do
    it "generates title for PushEvent" $ do
      let json = LBS.pack "{\"id\":\"1\",\"type\":\"PushEvent\",\"actor\":{\"login\":\"u\"},\"repo\":{\"name\":\"org/repo\"},\"payload\":{},\"created_at\":\"2025-01-01T00:00:00Z\"}"
      case decode json :: Maybe GitHubEvent of
        Nothing -> expectationFailure "Failed to parse"
        Just e -> extractEventTitle e `shouldBe` "PushEvent to org/repo"
```

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test --test-option=--match="/GitHub Events/"`
Expected: FAIL - module not found

**Step 3: Create Infra/GitHub/Events.hs**

Create `wisp-srv/src/Infra/GitHub/Events.hs`:

```haskell
module Infra.GitHub.Events
  ( GitHubEvent(..)
  , EventsResponse(..)
  , listEvents
  , extractEventTitle
  ) where

import Data.Aeson (FromJSON(..), Value, (.:), withObject)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI

data GitHubEvent = GitHubEvent
  { ghEventId :: Text
  , ghEventType :: Text
  , ghEventActor :: Text
  , ghEventRepo :: Text
  , ghEventPayload :: Value
  , ghEventCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance FromJSON GitHubEvent where
  parseJSON = withObject "GitHubEvent" $ \v -> do
    eid <- v .: "id"
    etype <- v .: "type"
    actor <- v .: "actor"
    actorLogin <- actor .: "login"
    repo <- v .: "repo"
    repoName <- repo .: "name"
    payload <- v .: "payload"
    createdAt <- v .: "created_at"
    pure $ GitHubEvent eid etype actorLogin repoName payload createdAt

-- Response includes events and optional new ETag
data EventsResponse = EventsResponse
  { eventsData :: [GitHubEvent]
  , eventsETag :: Maybe Text
  , eventsNotModified :: Bool
  } deriving (Show)

-- Fetch events for a user, with optional ETag for caching
-- Returns (events, Maybe newETag, notModified)
listEvents :: Text -> Text -> Maybe Text -> IO (Either Text EventsResponse)
listEvents username accessToken mETag = do
  manager <- newManager tlsManagerSettings
  let url = "https://api.github.com/users/" <> T.unpack username <> "/events"
  req <- parseRequest url
  let headers =
        [ ("Authorization", "Bearer " <> encodeUtf8 accessToken)
        , ("User-Agent", "wisp-srv")
        , ("Accept", "application/vnd.github+json")
        ] <> maybe [] (\etag -> [("If-None-Match", encodeUtf8 etag)]) mETag
  let authReq = req { requestHeaders = headers }
  response <- httpLbs authReq manager

  let status = statusCode (responseStatus response)
  let respHeaders = responseHeaders response
  let newETag = lookup (CI.mk "ETag") respHeaders >>= Just . T.pack . BS.unpack

  case status of
    304 -> pure $ Right $ EventsResponse [] newETag True
    200 -> case Aeson.decode (responseBody response) of
      Just events -> pure $ Right $ EventsResponse events newETag False
      Nothing -> pure $ Left "Failed to parse events response"
    _ -> pure $ Left $ "GitHub API error: " <> T.pack (show status)

-- Extract a human-readable title for an event
extractEventTitle :: GitHubEvent -> Text
extractEventTitle e = ghEventType e <> " to " <> ghEventRepo e
```

**Step 4: Add to cabal file**

Add to `wisp-srv.cabal` in `other-modules`:
- `Infra.GitHub.Events`

Add to test suite:
- `Infra.GitHub.EventsSpec`

**Step 5: Run test to verify it passes**

Run: `cabal test wisp-srv-test --test-option=--match="/GitHub Events/"`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Infra/GitHub/Events.hs wisp-srv/test/Infra/GitHub/EventsSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(github): add Events API client with ETag support"
```

---

## Task 8: Http/Handlers - GitHub OAuth Endpoints

**Files:**
- Modify: `wisp-srv/src/Http/Handlers/Auth.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`

**Step 1: Update Auth.hs with GitHub handlers**

Add to `wisp-srv/src/Http/Handlers/Auth.hs`:

```haskell
module Http.Handlers.Auth
  ( getGoogleAuth
  , getGoogleCallback
  , getGitHubAuth
  , getGitHubCallback
  , getAuthStatus
  ) where

-- Add imports:
import qualified Data.Aeson as Aeson
import App.Config (Config(..), ServerConfig(..), GoogleConfig(..), GitHubConfig(..))
import Infra.GitHub.Auth (GitHubOAuthConfig(..), buildGitHubAuthUrl, exchangeGitHubCode, getGitHubUser, GitHubUser(..), GitHubTokenResponse(..))
import Infra.Db.Account (upsertAccount, upsertAccountByProvider, getAllAccounts, getAccountsByProvider)
import Domain.Account (Account(..), AccountProvider(..), accountIdentifier)

-- Add GitHub OAuth config builder:
mkGitHubOAuthConfig :: Config -> Maybe GitHubOAuthConfig
mkGitHubOAuthConfig cfg = case cfg.github of
  Nothing -> Nothing
  Just gh -> Just $ GitHubOAuthConfig
    { ghClientId = gh.clientId
    , ghClientSecret = gh.clientSecret
    , ghRedirectUri = "http://127.0.0.1:" <> T.pack (show cfg.server.port) <> "/auth/github/callback"
    }

-- GitHub OAuth redirect
getGitHubAuth :: ActionT (ReaderT Env IO) ()
getGitHubAuth = do
  cfg <- lift getConfig
  case mkGitHubOAuthConfig cfg of
    Nothing -> do
      status status500
      json $ object ["error" .= ("GitHub OAuth not configured" :: Text)]
    Just oauthCfg -> do
      let url = buildGitHubAuthUrl oauthCfg
      setHeader "Location" (TL.fromStrict url)
      status status302
      json $ object ["redirect" .= url]

-- GitHub OAuth callback
getGitHubCallback :: ActionT (ReaderT Env IO) ()
getGitHubCallback = do
  mcode <- queryParamMaybe "code"
  merror <- queryParamMaybe "error"

  case merror of
    Just err -> do
      status status400
      json $ object ["error" .= (err :: Text)]
    Nothing -> case mcode of
      Nothing -> do
        status status400
        json $ object ["error" .= ("Missing authorization code" :: Text)]
      Just code -> do
        cfg <- lift getConfig
        case mkGitHubOAuthConfig cfg of
          Nothing -> do
            status status500
            json $ object ["error" .= ("GitHub OAuth not configured" :: Text)]
          Just oauthCfg -> do
            result <- liftIO $ exchangeGitHubCode oauthCfg code
            case result of
              Left err -> do
                status status500
                json $ object ["error" .= err]
              Right tok -> do
                -- Get user info
                userResult <- liftIO $ getGitHubUser (ghAccessToken tok)
                case userResult of
                  Left err -> do
                    status status500
                    json $ object ["error" .= ("Failed to get user info: " <> err)]
                  Right user -> do
                    -- Create account with GitHub details
                    let details = Aeson.object
                          [ "username" Aeson..= ghUserLogin user
                          , "access_token" Aeson..= ghAccessToken tok
                          ]
                    account <- lift $ upsertAccountByProvider GitHub details (ghUserName user)
                    json $ object
                      [ "status" .= ("authenticated" :: Text)
                      , "username" .= ghUserLogin user
                      , "provider" .= ("github" :: Text)
                      ]

-- Update accountToJson to use new model:
accountToJson :: UTCTime -> Account -> Data.Aeson.Value
accountToJson _ acc = object
  [ "provider" .= accountProvider acc
  , "identifier" .= accountIdentifier acc
  , "display_name" .= accountDisplayName acc
  , "created_at" .= accountCreatedAt acc
  ]
```

**Step 2: Update Routes.hs**

Add to `wisp-srv/src/Http/Routes.hs`:

```haskell
import Http.Handlers.Auth (getGoogleAuth, getGoogleCallback, getGitHubAuth, getGitHubCallback, getAuthStatus)

-- In routes, add:
  get "/auth/github" getGitHubAuth
  get "/auth/github/callback" getGitHubCallback
```

**Step 3: Build to verify compilation**

Run: `cabal build wisp-srv`

**Step 4: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Auth.hs wisp-srv/src/Http/Routes.hs
git commit -m "feat(http): add GitHub OAuth endpoints"
```

---

## Task 9: Services - GitHub Poller

**Files:**
- Create: `wisp-srv/src/Services/GitHubPoller.hs`
- Create: `wisp-srv/test/Services/GitHubPollerSpec.hs`

**Step 1: Write failing test**

Create `wisp-srv/test/Services/GitHubPollerSpec.hs`:

```haskell
module Services.GitHubPollerSpec where

import Test.Hspec
import Services.GitHubPoller (buildActivityFromEvent)
import Infra.GitHub.Events (GitHubEvent(..))
import Domain.Activity (ActivitySource(..), NewActivity(..))
import Domain.Id (EntityId(..))
import Data.Aeson (object)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

spec :: Spec
spec = describe "GitHubPoller" $ do
  describe "buildActivityFromEvent" $ do
    it "creates NewActivity from GitHubEvent" $ do
      let testTime = posixSecondsToUTCTime 1704067200
          event = GitHubEvent
            { ghEventId = "12345"
            , ghEventType = "PushEvent"
            , ghEventActor = "garethstokes"
            , ghEventRepo = "org/repo"
            , ghEventPayload = object []
            , ghEventCreatedAt = testTime
            }
          accountId = EntityId "acc123"
          activity = buildActivityFromEvent accountId event

      newActivitySource activity `shouldBe` GitHubEvent
      newActivitySourceId activity `shouldBe` "12345"
      newActivityTitle activity `shouldBe` Just "PushEvent to org/repo"
      newActivityStartsAt activity `shouldBe` Just testTime
```

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test --test-option=--match="/GitHubPoller/"`
Expected: FAIL - module not found

**Step 3: Create Services/GitHubPoller.hs**

Create `wisp-srv/src/Services/GitHubPoller.hs`:

```haskell
module Services.GitHubPoller
  ( pollGitHubForAccount
  , pollAllGitHub
  , buildActivityFromEvent
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import App.Monad (App)
import Domain.Id (EntityId)
import Domain.Account (Account(..), AccountProvider(..), accountIdentifier)
import Domain.Activity (NewActivity(..), ActivitySource(..))
import Infra.Db.Account (getAccountsByProvider)
import Infra.Db.Activity (insertActivity, activityExistsForAccount)
import Infra.Db.PollState (getPollStateForAccount, updatePollStateForAccount, ensurePollStateExists)
import Infra.GitHub.Events (GitHubEvent(..), EventsResponse(..), listEvents, extractEventTitle)

-- | Build a NewActivity from a GitHubEvent
buildActivityFromEvent :: EntityId -> GitHubEvent -> NewActivity
buildActivityFromEvent accId event = NewActivity
  { newActivityAccountId = accId
  , newActivitySource = GitHubEvent
  , newActivitySourceId = ghEventId event
  , newActivityRaw = toJSON event
  , newActivityTitle = Just $ extractEventTitle event
  , newActivitySenderEmail = Nothing  -- GitHub uses usernames, not emails
  , newActivityStartsAt = Just $ ghEventCreatedAt event
  , newActivityEndsAt = Nothing
  }

-- | Get access token from account details
getAccessToken :: Account -> Maybe Text
getAccessToken acc = case accountDetails acc of
  Aeson.Object obj -> case KM.lookup "access_token" obj of
    Just (Aeson.String t) -> Just t
    _ -> Nothing
  _ -> Nothing

-- | Poll GitHub for ALL GitHub accounts
pollAllGitHub :: App [(Text, Either Text [EntityId])]
pollAllGitHub = do
  accounts <- getAccountsByProvider GitHub
  forM accounts $ \acc -> do
    result <- pollGitHubForAccount acc
    let identifier = maybe "unknown" id (accountIdentifier acc)
    pure (identifier, result)

-- | Poll GitHub for a specific account
pollGitHubForAccount :: Account -> App (Either Text [EntityId])
pollGitHubForAccount acc = do
  case (accountIdentifier acc, getAccessToken acc) of
    (Nothing, _) -> pure $ Left "No username in account details"
    (_, Nothing) -> pure $ Left "No access token in account details"
    (Just username, Just token) -> do
      -- Ensure poll state exists
      ensurePollStateExists (accountId acc) "github"
      mPollState <- getPollStateForAccount (accountId acc) "github"
      let mETag = mPollState >>= \ps -> pollCursor ps

      -- Fetch events with ETag
      result <- liftIO $ listEvents username token mETag
      case result of
        Left err -> pure $ Left err
        Right response
          | eventsNotModified response -> pure $ Right []  -- Nothing new
          | otherwise -> do
              -- Process events
              newIds <- processEvents (accountId acc) (eventsData response)
              -- Update ETag in poll state
              updatePollStateForAccount (accountId acc) "github" (eventsETag response)
              pure $ Right newIds

-- | Process a list of events, inserting as activities
processEvents :: EntityId -> [GitHubEvent] -> App [EntityId]
processEvents accId events = do
  results <- forM events $ \event -> do
    exists <- activityExistsForAccount accId GitHubEvent (ghEventId event)
    if exists
      then pure Nothing
      else do
        let activity = buildActivityFromEvent accId event
        actId <- insertActivity activity
        pure $ Just actId
  pure [aid | Just aid <- results]
```

**Step 4: Add to cabal file**

Add to `wisp-srv.cabal`:
- `Services.GitHubPoller`
- `Services.GitHubPollerSpec` in test

**Step 5: Run test to verify it passes**

Run: `cabal test wisp-srv-test --test-option=--match="/GitHubPoller/"`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Services/GitHubPoller.hs wisp-srv/test/Services/GitHubPollerSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(services): add GitHub events poller"
```

---

## Task 10: Services - Integrate GitHub into Scheduler

**Files:**
- Modify: `wisp-srv/src/Services/Scheduler.hs`

**Step 1: Add GitHub polling to scheduler**

Update `wisp-srv/src/Services/Scheduler.hs`:

```haskell
-- Add import:
import Services.GitHubPoller (pollAllGitHub)

-- Update runPollCycle to include GitHub:
runPollCycle :: App ()
runPollCycle = do
  logInfo "Starting poll cycle"

  -- Poll Gmail for all accounts
  gmailResults <- pollAllGmail
  gmailIds <- case gmailResults of
    [] -> do
      logInfo "Gmail: no accounts configured"
      pure []
    _ -> do
      let allIds = concat [ids | (_, Right ids) <- gmailResults]
      logInfo $ "Gmail: imported " <> T.pack (show (length allIds)) <> " messages total"
      mapM_ logAccountResult $ map (\(e, r) -> ("Gmail", e, fmap length r)) gmailResults
      pure allIds

  -- Poll Calendar for all accounts
  calResults <- pollAllCalendar
  calIds <- case calResults of
    [] -> do
      logInfo "Calendar: no accounts configured"
      pure []
    _ -> do
      let allIds = concat [ids | (_, Right ids) <- calResults]
      logInfo $ "Calendar: imported " <> T.pack (show (length allIds)) <> " events total"
      mapM_ logAccountResult $ map (\(e, r) -> ("Calendar", e, fmap length r)) calResults
      pure allIds

  -- Poll GitHub for all accounts
  githubResults <- pollAllGitHub
  githubIds <- case githubResults of
    [] -> do
      logInfo "GitHub: no accounts configured"
      pure []
    _ -> do
      let allIds = concat [ids | (_, Right ids) <- githubResults]
      logInfo $ "GitHub: imported " <> T.pack (show (length allIds)) <> " events total"
      mapM_ logAccountResult $ map (\(u, r) -> ("GitHub", u, fmap length r)) githubResults
      pure allIds

  -- Enqueue newly imported activities for classification
  let newIds = gmailIds ++ calIds ++ githubIds
  enqueueForClassification newIds

  logInfo "Poll cycle complete"
```

**Step 2: Build to verify compilation**

Run: `cabal build wisp-srv`

**Step 3: Commit**

```bash
git add wisp-srv/src/Services/Scheduler.hs
git commit -m "feat(scheduler): add GitHub polling to poll cycle"
```

---

## Task 11: Fix Compilation Issues

**Files:**
- Various files as needed

**Step 1: Build full project**

Run: `cabal build wisp-srv`

**Step 2: Fix any import/type errors**

Review errors and fix one by one. Common issues:
- Missing imports
- Type mismatches in Account usage (old email field references)
- Aeson.KeyMap import needed

**Step 3: Run full test suite**

Run: `cabal test wisp-srv-test`

**Step 4: Commit fixes**

```bash
git add -A
git commit -m "fix: resolve compilation issues from account refactor"
```

---

## Task 12: Apply Migration and Test End-to-End

**Step 1: Apply migration**

Run: `psql $DATABASE_URL -f wisp-srv/migrations/016_account_providers.sql`

**Step 2: Start server**

Run: `cabal run wisp-srv`

**Step 3: Test GitHub OAuth flow**

1. Visit `http://localhost:8080/auth/github`
2. Authorize with GitHub
3. Verify callback creates account

**Step 4: Check auth status**

Run: `curl http://localhost:8080/auth/status | jq`

Verify both Google and GitHub accounts appear.

**Step 5: Trigger poll and verify**

Run: `curl -X POST http://localhost:8080/poll`

Check logs for GitHub polling output.

**Step 6: Final commit**

```bash
git add -A
git commit -m "feat: complete GitHub integration"
```

---

## Summary

This plan has 12 tasks covering:

1. Database migration for provider model
2. Domain types (AccountProvider, GitHubEvent)
3. Database layer updates
4. Config changes
5. GitHub Auth infrastructure
6. GitHub Events infrastructure
7. HTTP handlers for OAuth
8. GitHub Poller service
9. Scheduler integration
10. Compilation fixes
11. End-to-end testing

Each task follows TDD where applicable, with exact file paths and code provided.
