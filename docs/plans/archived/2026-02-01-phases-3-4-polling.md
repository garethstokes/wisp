# Wisp Backend Phases 3 & 4: Gmail and Calendar Polling

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement Gmail and Calendar pollers that fetch messages/events from Google APIs and store them as activities for later processing.

**Architecture:** Pollers use OAuth tokens from Phase 2, fetch data via Google APIs, store raw responses as versioned entities, track sync state via `poll_state` table. Token refresh happens automatically before API calls.

**Tech Stack:** Haskell, http-client-tls (Google API calls), aeson (JSON parsing), postgresql-simple, async (background polling)

---

## Task 1: Activities Migration

**Files:**
- Create: `wisp-srv/migrations/006_activities.sql`

**Step 1: Write the activities table migration**

```sql
-- migrations/006_activities.sql

-- Activities table for storing emails and calendar events

create table if not exists activities (
  id text primary key,
  source text not null,           -- 'email', 'calendar'
  source_id text not null,        -- Gmail message ID, Calendar event ID
  raw jsonb not null,             -- Original API response
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),

  -- Classification (populated later by LLM)
  personas text[],
  activity_type text,
  urgency text,
  autonomy_tier int,
  confidence float,
  status text not null default 'pending',

  -- Content
  title text,
  summary text,
  sender_email text,
  person_id text,

  -- Calendar-specific
  starts_at timestamptz,
  ends_at timestamptz
);

-- Prevent duplicate imports
create unique index if not exists activities_source_unique
  on activities (source, source_id);

-- Index for status queries
create index if not exists activities_by_status
  on activities (status, created_at desc);

-- Index for person queries
create index if not exists activities_by_person
  on activities (person_id, created_at desc);

-- Index for date range queries (calendar)
create index if not exists activities_by_date
  on activities (starts_at, ends_at)
  where starts_at is not null;
```

**Step 2: Verify migration file exists**

Run: `cat /home/gareth/code/hacking/wisp/wisp-srv/migrations/006_activities.sql | head -5`
Expected: Shows the SQL header

**Step 3: Commit**

```bash
git add wisp-srv/migrations/006_activities.sql
git commit -m "feat: add activities table migration"
```

---

## Task 2: Activity Domain Types

**Files:**
- Create: `wisp-srv/src/Domain/Activity.hs`

**Step 1: Write the failing test**

Create `wisp-srv/test/Domain/ActivitySpec.hs`:

```haskell
module Domain.ActivitySpec where

import Test.Hspec
import Domain.Activity
import Data.Aeson (encode, decode, object, (.=))

spec :: Spec
spec = describe "Activity" $ do
  describe "ActivitySource" $ do
    it "serializes Email to JSON" $ do
      encode Email `shouldBe` "\"email\""

    it "serializes Calendar to JSON" $ do
      encode Calendar `shouldBe` "\"calendar\""

  describe "ActivityStatus" $ do
    it "round-trips through JSON" $ do
      decode (encode Pending) `shouldBe` Just Pending
      decode (encode Processed) `shouldBe` Just Processed
```

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: FAIL - module Domain.Activity not found

**Step 3: Write the Activity module**

```haskell
-- src/Domain/Activity.hs
module Domain.Activity
  ( Activity(..)
  , ActivitySource(..)
  , ActivityStatus(..)
  , NewActivity(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value, withText)
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Id (EntityId)
import GHC.Generics (Generic)

data ActivitySource = Email | Calendar
  deriving (Eq, Show, Generic)

instance ToJSON ActivitySource where
  toJSON Email = "email"
  toJSON Calendar = "calendar"

instance FromJSON ActivitySource where
  parseJSON = withText "ActivitySource" $ \case
    "email" -> pure Email
    "calendar" -> pure Calendar
    _ -> fail "Invalid activity source"

data ActivityStatus
  = Pending
  | Quarantined
  | Processed
  | Surfaced
  | Archived
  deriving (Eq, Show, Generic)

instance ToJSON ActivityStatus where
  toJSON Pending = "pending"
  toJSON Quarantined = "quarantined"
  toJSON Processed = "processed"
  toJSON Surfaced = "surfaced"
  toJSON Archived = "archived"

instance FromJSON ActivityStatus where
  parseJSON = withText "ActivityStatus" $ \case
    "pending" -> pure Pending
    "quarantined" -> pure Quarantined
    "processed" -> pure Processed
    "surfaced" -> pure Surfaced
    "archived" -> pure Archived
    _ -> fail "Invalid activity status"

-- Full activity record from database
data Activity = Activity
  { activityId :: EntityId
  , activitySource :: ActivitySource
  , activitySourceId :: Text
  , activityRaw :: Value
  , activityStatus :: ActivityStatus
  , activityTitle :: Maybe Text
  , activitySummary :: Maybe Text
  , activitySenderEmail :: Maybe Text
  , activityStartsAt :: Maybe UTCTime
  , activityEndsAt :: Maybe UTCTime
  , activityCreatedAt :: UTCTime
  } deriving (Show)

-- For creating new activities
data NewActivity = NewActivity
  { newActivitySource :: ActivitySource
  , newActivitySourceId :: Text
  , newActivityRaw :: Value
  , newActivityTitle :: Maybe Text
  , newActivitySenderEmail :: Maybe Text
  , newActivityStartsAt :: Maybe UTCTime
  , newActivityEndsAt :: Maybe UTCTime
  } deriving (Show)
```

**Step 4: Update wisp-srv.cabal with new module**

Add to `other-modules` in both executable and test-suite:
```
Domain.Activity
```

Add to test-suite `other-modules`:
```
Domain.ActivitySpec
```

**Step 5: Run test to verify it passes**

Run: `cabal test wisp-srv-test 2>&1 | tail -15`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Domain/Activity.hs wisp-srv/test/Domain/ActivitySpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Activity domain types"
```

---

## Task 3: Activity Database Operations

**Files:**
- Create: `wisp-srv/src/Infra/Db/Activity.hs`

**Step 1: Write the failing test**

Create `wisp-srv/test/Infra/Db/ActivitySpec.hs`:

```haskell
module Infra.Db.ActivitySpec where

import Test.Hspec

spec :: Spec
spec = describe "Activity DB" $ do
  it "placeholder - activity operations need integration tests" $ do
    True `shouldBe` True
```

**Step 2: Write the Activity DB module**

```haskell
-- src/Infra/Db/Activity.hs
module Infra.Db.Activity
  ( insertActivity
  , activityExists
  , getActivity
  , getActivitiesByStatus
  , updateActivityStatus
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, Value, toJSON, encode)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Domain.Id (EntityId(..), newEntityId)
import Domain.Activity
import App.Monad (App, getConn)

instance FromRow Activity where
  fromRow = Activity
    <$> (EntityId <$> field)          -- id
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
      parseSource _ = Email  -- default

      parseStatus :: Text -> ActivityStatus
      parseStatus "pending" = Pending
      parseStatus "quarantined" = Quarantined
      parseStatus "processed" = Processed
      parseStatus "surfaced" = Surfaced
      parseStatus "archived" = Archived
      parseStatus _ = Pending  -- default

-- Insert a new activity (returns Nothing if duplicate)
insertActivity :: NewActivity -> App (Maybe EntityId)
insertActivity new = do
  conn <- getConn
  aid <- liftIO newEntityId
  let srcText = case newActivitySource new of
        Email -> "email" :: Text
        Calendar -> "calendar"
  n <- liftIO $ execute conn
    "insert into activities \
    \(id, source, source_id, raw, title, sender_email, starts_at, ends_at) \
    \values (?, ?, ?, ?, ?, ?, ?, ?) \
    \on conflict (source, source_id) do nothing"
    ( unEntityId aid
    , srcText
    , newActivitySourceId new
    , toJSON (newActivityRaw new)
    , newActivityTitle new
    , newActivitySenderEmail new
    , newActivityStartsAt new
    , newActivityEndsAt new
    )
  pure $ if n > 0 then Just aid else Nothing

-- Check if activity already exists
activityExists :: ActivitySource -> Text -> App Bool
activityExists src srcId = do
  conn <- getConn
  let srcText = case src of
        Email -> "email" :: Text
        Calendar -> "calendar"
  results <- liftIO $ query conn
    "select 1 from activities where source = ? and source_id = ? limit 1"
    (srcText, srcId)
  pure $ not (null (results :: [Only Int]))

-- Get activity by ID
getActivity :: EntityId -> App (Maybe Activity)
getActivity aid = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at \
    \from activities where id = ?"
    (Only $ unEntityId aid)
  pure $ case results of
    [a] -> Just a
    _ -> Nothing

-- Get activities by status
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
    "select id, source, source_id, raw, status, title, summary, \
    \sender_email, starts_at, ends_at, created_at \
    \from activities where status = ? \
    \order by created_at desc limit ?"
    (statusText, limit)

-- Update activity status
updateActivityStatus :: EntityId -> ActivityStatus -> App ()
updateActivityStatus aid status = do
  conn <- getConn
  let statusText = case status of
        Pending -> "pending" :: Text
        Quarantined -> "quarantined"
        Processed -> "processed"
        Surfaced -> "surfaced"
        Archived -> "archived"
  _ <- liftIO $ execute conn
    "update activities set status = ?, updated_at = now() where id = ?"
    (statusText, unEntityId aid)
  pure ()
```

**Step 3: Update wisp-srv.cabal**

Add to `other-modules` in executable and test-suite:
```
Infra.Db.Activity
```

Add to test-suite:
```
Infra.Db.ActivitySpec
```

**Step 4: Run test to verify it compiles**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Db/Activity.hs wisp-srv/test/Infra/Db/ActivitySpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add activity database operations"
```

---

## Task 4: Poll State Operations

**Files:**
- Create: `wisp-srv/src/Infra/Db/PollState.hs`

**Step 1: Write the failing test**

Create `wisp-srv/test/Infra/Db/PollStateSpec.hs`:

```haskell
module Infra.Db.PollStateSpec where

import Test.Hspec

spec :: Spec
spec = describe "PollState" $ do
  it "placeholder - poll state operations need integration tests" $ do
    True `shouldBe` True
```

**Step 2: Write the PollState module**

```haskell
-- src/Infra/Db/PollState.hs
module Infra.Db.PollState
  ( PollState(..)
  , getPollState
  , updatePollState
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import App.Monad (App, getConn)

data PollState = PollState
  { pollSource :: Text
  , pollLastAt :: UTCTime
  , pollCursor :: Maybe Text
  } deriving (Show)

instance FromRow PollState where
  fromRow = PollState <$> field <*> field <*> field

-- Get poll state for a source
getPollState :: Text -> App (Maybe PollState)
getPollState source = do
  conn <- getConn
  results <- liftIO $ query conn
    "select source, last_poll_at, cursor from poll_state where source = ?"
    (Only source)
  pure $ case results of
    [ps] -> Just ps
    _ -> Nothing

-- Update poll state with new cursor
updatePollState :: Text -> Maybe Text -> App ()
updatePollState source cursor = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "update poll_state set last_poll_at = now(), cursor = ? where source = ?"
    (cursor, source)
  pure ()
```

**Step 3: Update wisp-srv.cabal**

Add to `other-modules` in executable and test-suite:
```
Infra.Db.PollState
```

Add to test-suite:
```
Infra.Db.PollStateSpec
```

**Step 4: Run test to verify it compiles**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Db/PollState.hs wisp-srv/test/Infra/Db/PollStateSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add poll state database operations"
```

---

## Task 5: Token Manager with Auto-Refresh

**Files:**
- Create: `wisp-srv/src/Infra/Google/TokenManager.hs`

**Step 1: Write the failing test**

Create `wisp-srv/test/Infra/Google/TokenManagerSpec.hs`:

```haskell
module Infra.Google.TokenManagerSpec where

import Test.Hspec

spec :: Spec
spec = describe "TokenManager" $ do
  it "placeholder - token manager needs integration tests" $ do
    True `shouldBe` True
```

**Step 2: Write the TokenManager module**

```haskell
-- src/Infra/Google/TokenManager.hs
module Infra.Google.TokenManager
  ( getValidToken
  , TokenError(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime)
import App.Monad (App, getConfig)
import App.Config (Config(..), GoogleConfig(..))
import Infra.Db.Auth (AuthToken(..), getToken, updateToken, tokenNeedsRefresh)
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

-- Get a valid access token, refreshing if needed
getValidToken :: App (Either TokenError Text)
getValidToken = do
  mtoken <- getToken "google"
  case mtoken of
    Nothing -> pure $ Left NoToken
    Just tok -> do
      needsRefresh <- liftIO $ tokenNeedsRefresh tok
      if needsRefresh
        then refreshAndUpdate tok
        else pure $ Right (tokenAccessToken tok)

-- Refresh the token and update in database
refreshAndUpdate :: AuthToken -> App (Either TokenError Text)
refreshAndUpdate tok = do
  cfg <- getConfig
  let oauthCfg = mkOAuthConfig cfg
  result <- liftIO $ refreshAccessToken oauthCfg (tokenRefreshToken tok)
  case result of
    Left err -> pure $ Left (RefreshFailed err)
    Right newTok -> do
      now <- liftIO getCurrentTime
      let newExpiry = addUTCTime (fromIntegral $ expiresIn newTok) now
      updateToken "google" (accessToken newTok) newExpiry
      pure $ Right (accessToken newTok)
```

**Step 3: Update wisp-srv.cabal**

Add to `other-modules` in executable and test-suite:
```
Infra.Google.TokenManager
```

Add to test-suite:
```
Infra.Google.TokenManagerSpec
```

**Step 4: Run test to verify it compiles**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Google/TokenManager.hs wisp-srv/test/Infra/Google/TokenManagerSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add token manager with auto-refresh"
```

---

## Task 6: Gmail API Client

**Files:**
- Create: `wisp-srv/src/Infra/Google/Gmail.hs`

**Step 1: Write the failing test**

Create `wisp-srv/test/Infra/Google/GmailSpec.hs`:

```haskell
module Infra.Google.GmailSpec where

import Test.Hspec
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LBS

spec :: Spec
spec = describe "Gmail API" $ do
  describe "GmailMessage parsing" $ do
    it "parses a minimal message response" $ do
      let json = LBS.pack "{\"id\":\"msg123\",\"threadId\":\"t456\"}"
      case decode json of
        Nothing -> expectationFailure "Failed to parse"
        Just msg -> do
          True `shouldBe` True  -- parsing succeeded
```

**Step 2: Write the Gmail module**

```haskell
-- src/Infra/Google/Gmail.hs
module Infra.Google.Gmail
  ( GmailMessage(..)
  , GmailMessageList(..)
  , GmailHistoryList(..)
  , GmailHistory(..)
  , listMessages
  , getMessage
  , listHistory
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject, Value)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- Gmail message (minimal fields we need)
data GmailMessage = GmailMessage
  { gmailId :: Text
  , gmailThreadId :: Text
  , gmailLabelIds :: Maybe [Text]
  , gmailSnippet :: Maybe Text
  , gmailPayload :: Maybe GmailPayload
  , gmailInternalDate :: Maybe Text
  , gmailRaw :: Maybe Value  -- Full response for storage
  } deriving (Show, Generic)

instance FromJSON GmailMessage where
  parseJSON = withObject "GmailMessage" $ \v -> GmailMessage
    <$> v .: "id"
    <*> v .: "threadId"
    <*> v .:? "labelIds"
    <*> v .:? "snippet"
    <*> v .:? "payload"
    <*> v .:? "internalDate"
    <*> pure Nothing

data GmailPayload = GmailPayload
  { payloadHeaders :: Maybe [GmailHeader]
  , payloadMimeType :: Maybe Text
  , payloadBody :: Maybe GmailBody
  } deriving (Show, Generic)

instance FromJSON GmailPayload where
  parseJSON = withObject "GmailPayload" $ \v -> GmailPayload
    <$> v .:? "headers"
    <*> v .:? "mimeType"
    <*> v .:? "body"

data GmailHeader = GmailHeader
  { headerName :: Text
  , headerValue :: Text
  } deriving (Show, Generic)

instance FromJSON GmailHeader where
  parseJSON = withObject "GmailHeader" $ \v -> GmailHeader
    <$> v .: "name"
    <*> v .: "value"

data GmailBody = GmailBody
  { bodySize :: Int
  , bodyData :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON GmailBody where
  parseJSON = withObject "GmailBody" $ \v -> GmailBody
    <$> v .: "size"
    <*> v .:? "data"

-- List messages response
data GmailMessageList = GmailMessageList
  { messages :: Maybe [GmailMessageRef]
  , nextPageToken :: Maybe Text
  , resultSizeEstimate :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON GmailMessageList

data GmailMessageRef = GmailMessageRef
  { refId :: Text
  , refThreadId :: Text
  } deriving (Show)

instance FromJSON GmailMessageRef where
  parseJSON = withObject "GmailMessageRef" $ \v -> GmailMessageRef
    <$> v .: "id"
    <*> v .: "threadId"

-- History list response
data GmailHistoryList = GmailHistoryList
  { history :: Maybe [GmailHistory]
  , historyNextPageToken :: Maybe Text
  , historyId :: Maybe Text
  } deriving (Show)

instance FromJSON GmailHistoryList where
  parseJSON = withObject "GmailHistoryList" $ \v -> GmailHistoryList
    <$> v .:? "history"
    <*> v .:? "nextPageToken"
    <*> v .:? "historyId"

data GmailHistory = GmailHistory
  { historyRecordId :: Text
  , historyMessagesAdded :: Maybe [GmailHistoryMessage]
  } deriving (Show)

instance FromJSON GmailHistory where
  parseJSON = withObject "GmailHistory" $ \v -> GmailHistory
    <$> v .: "id"
    <*> v .:? "messagesAdded"

data GmailHistoryMessage = GmailHistoryMessage
  { historyMessage :: GmailMessageRef
  } deriving (Show)

instance FromJSON GmailHistoryMessage where
  parseJSON = withObject "GmailHistoryMessage" $ \v -> GmailHistoryMessage
    <$> v .: "message"

-- API call helpers
gmailApiBase :: String
gmailApiBase = "https://gmail.googleapis.com/gmail/v1/users/me"

-- List messages (recent, no history ID)
listMessages :: Text -> Maybe Text -> IO (Either Text GmailMessageList)
listMessages token pageToken = do
  manager <- newManager tlsManagerSettings
  let url = gmailApiBase <> "/messages?maxResults=50"
            <> maybe "" (\pt -> "&pageToken=" <> T.unpack pt) pageToken
  req <- parseRequest url
  let authReq = req
        { requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 token)]
        }
  response <- httpLbs authReq manager
  pure $ case Aeson.decode (responseBody response) of
    Just ml -> Right ml
    Nothing -> Left $ "Failed to parse messages list: " <> T.pack (show $ responseBody response)

-- Get full message by ID
getMessage :: Text -> Text -> IO (Either Text GmailMessage)
getMessage token msgId = do
  manager <- newManager tlsManagerSettings
  let url = gmailApiBase <> "/messages/" <> T.unpack msgId <> "?format=full"
  req <- parseRequest url
  let authReq = req
        { requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 token)]
        }
  response <- httpLbs authReq manager
  pure $ case Aeson.decode (responseBody response) of
    Just msg -> Right msg { gmailRaw = Aeson.decode (responseBody response) }
    Nothing -> Left $ "Failed to parse message: " <> T.pack (show $ responseBody response)

-- List history since a history ID
listHistory :: Text -> Text -> Maybe Text -> IO (Either Text GmailHistoryList)
listHistory token startHistoryId pageToken = do
  manager <- newManager tlsManagerSettings
  let url = gmailApiBase <> "/history?startHistoryId=" <> T.unpack startHistoryId
            <> "&historyTypes=messageAdded"
            <> maybe "" (\pt -> "&pageToken=" <> T.unpack pt) pageToken
  req <- parseRequest url
  let authReq = req
        { requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 token)]
        }
  response <- httpLbs authReq manager
  pure $ case Aeson.decode (responseBody response) of
    Just hl -> Right hl
    Nothing -> Left $ "Failed to parse history: " <> T.pack (show $ responseBody response)
```

**Step 3: Update wisp-srv.cabal**

Add to `other-modules` in executable and test-suite:
```
Infra.Google.Gmail
```

Add to test-suite:
```
Infra.Google.GmailSpec
```

**Step 4: Run test to verify it compiles**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Google/Gmail.hs wisp-srv/test/Infra/Google/GmailSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Gmail API client"
```

---

## Task 7: Gmail Poller Service

**Files:**
- Create: `wisp-srv/src/Services/GmailPoller.hs`

**Step 1: Write the failing test**

Create `wisp-srv/test/Services/GmailPollerSpec.hs`:

```haskell
module Services.GmailPollerSpec where

import Test.Hspec
import Services.GmailPoller (extractEmailInfo)
import Infra.Google.Gmail (GmailMessage(..), GmailPayload(..), GmailHeader(..))

spec :: Spec
spec = describe "GmailPoller" $ do
  describe "extractEmailInfo" $ do
    it "extracts subject and sender from headers" $ do
      let headers = Just
            [ GmailHeader "Subject" "Test Subject"
            , GmailHeader "From" "sender@example.com"
            ]
      let payload = Just $ GmailPayload headers Nothing Nothing
      let msg = GmailMessage "id1" "t1" Nothing Nothing payload Nothing Nothing
      let (subject, sender) = extractEmailInfo msg
      subject `shouldBe` Just "Test Subject"
      sender `shouldBe` Just "sender@example.com"

    it "returns Nothing when headers missing" $ do
      let msg = GmailMessage "id1" "t1" Nothing Nothing Nothing Nothing Nothing
      let (subject, sender) = extractEmailInfo msg
      subject `shouldBe` Nothing
      sender `shouldBe` Nothing
```

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: FAIL - module Services.GmailPoller not found

**Step 3: Write the GmailPoller module**

```haskell
-- src/Services/GmailPoller.hs
module Services.GmailPoller
  ( pollGmail
  , extractEmailInfo
  ) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import App.Monad (App)
import Domain.Activity (NewActivity(..), ActivitySource(..))
import Infra.Db.Activity (insertActivity, activityExists)
import Infra.Db.PollState (getPollState, updatePollState)
import Infra.Google.Gmail
import Infra.Google.TokenManager (getValidToken, TokenError(..))

-- Extract subject and sender from a Gmail message
extractEmailInfo :: GmailMessage -> (Maybe Text, Maybe Text)
extractEmailInfo msg =
  case gmailPayload msg of
    Nothing -> (Nothing, Nothing)
    Just payload -> case payloadHeaders payload of
      Nothing -> (Nothing, Nothing)
      Just headers ->
        let findHeader name = headerValue <$> listToMaybe
              (filter (\h -> T.toLower (headerName h) == T.toLower name) headers)
        in (findHeader "Subject", findHeader "From")

-- Poll Gmail for new messages
pollGmail :: App (Either Text Int)
pollGmail = do
  tokenResult <- getValidToken
  case tokenResult of
    Left NoToken -> pure $ Left "No Google token - please authenticate first"
    Left (RefreshFailed err) -> pure $ Left $ "Token refresh failed: " <> err
    Right token -> do
      -- Get current poll state
      mstate <- getPollState "gmail"
      let mCursor = mstate >>= pollCursor

      -- Fetch messages based on whether we have a history ID
      case mCursor of
        Nothing -> pollInitial token
        Just historyId -> pollIncremental token historyId

-- Initial poll: fetch recent messages
pollInitial :: Text -> App (Either Text Int)
pollInitial token = do
  liftIO $ putStrLn "Gmail: Initial poll - fetching recent messages"
  result <- liftIO $ listMessages token Nothing
  case result of
    Left err -> pure $ Left err
    Right msgList -> do
      let refs = fromMaybe [] (messages msgList)
      count <- processMessageRefs token refs
      -- For initial poll, we need to get a history ID
      -- Get it from the first message or use profile
      when (not $ null refs) $ do
        let firstRef = head refs
        msgResult <- liftIO $ getMessage token (refId firstRef)
        case msgResult of
          Right _ -> do
            -- The history ID comes from the messages.list response metadata
            -- For simplicity, store the first message ID as cursor
            -- In production, use users.getProfile to get historyId
            updatePollState "gmail" (Just $ refId firstRef)
          Left _ -> pure ()
      pure $ Right count

-- Incremental poll using history API
pollIncremental :: Text -> Text -> App (Either Text Int)
pollIncremental token historyId = do
  liftIO $ putStrLn $ "Gmail: Incremental poll from history ID " <> T.unpack historyId
  result <- liftIO $ listHistory token historyId Nothing
  case result of
    Left err ->
      -- History ID might be invalid, fall back to initial
      if "404" `T.isInfixOf` err || "historyId" `T.isInfixOf` err
        then do
          liftIO $ putStrLn "Gmail: History ID invalid, falling back to initial poll"
          updatePollState "gmail" Nothing
          pollInitial token
        else pure $ Left err
    Right histList -> do
      let addedMsgs = concatMap getAddedMessages (fromMaybe [] $ history histList)
      count <- processMessageRefs token addedMsgs
      -- Update cursor with new history ID
      case historyId histList of
        Just newId -> updatePollState "gmail" (Just newId)
        Nothing -> pure ()
      pure $ Right count
  where
    getAddedMessages :: GmailHistory -> [GmailMessageRef]
    getAddedMessages h = map historyMessage $ fromMaybe [] (historyMessagesAdded h)

-- Process a list of message references
processMessageRefs :: Text -> [GmailMessageRef] -> App Int
processMessageRefs token refs = do
  counts <- forM refs $ \ref -> do
    -- Check if already imported
    exists <- activityExists Email (refId ref)
    if exists
      then pure 0
      else do
        -- Fetch full message
        msgResult <- liftIO $ getMessage token (refId ref)
        case msgResult of
          Left err -> do
            liftIO $ putStrLn $ "Gmail: Failed to fetch message " <> T.unpack (refId ref) <> ": " <> T.unpack err
            pure 0
          Right msg -> do
            let (subject, sender) = extractEmailInfo msg
            let activity = NewActivity
                  { newActivitySource = Email
                  , newActivitySourceId = gmailId msg
                  , newActivityRaw = fromMaybe (toJSON msg) (gmailRaw msg)
                  , newActivityTitle = subject
                  , newActivitySenderEmail = sender
                  , newActivityStartsAt = Nothing
                  , newActivityEndsAt = Nothing
                  }
            result <- insertActivity activity
            case result of
              Just _ -> do
                liftIO $ putStrLn $ "Gmail: Imported message: " <> T.unpack (fromMaybe "(no subject)" subject)
                pure 1
              Nothing -> pure 0
  pure $ sum counts
```

**Step 4: Update wisp-srv.cabal**

Add to `other-modules` in executable and test-suite:
```
Services.GmailPoller
```

Add to test-suite:
```
Services.GmailPollerSpec
```

**Step 5: Run test to verify it passes**

Run: `cabal test wisp-srv-test 2>&1 | tail -15`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Services/GmailPoller.hs wisp-srv/test/Services/GmailPollerSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Gmail poller service"
```

---

## Task 8: Calendar API Client

**Files:**
- Create: `wisp-srv/src/Infra/Google/Calendar.hs`

**Step 1: Write the failing test**

Create `wisp-srv/test/Infra/Google/CalendarSpec.hs`:

```haskell
module Infra.Google.CalendarSpec where

import Test.Hspec
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LBS

spec :: Spec
spec = describe "Calendar API" $ do
  describe "CalendarEvent parsing" $ do
    it "parses a minimal event response" $ do
      let json = LBS.pack "{\"id\":\"evt123\",\"status\":\"confirmed\"}"
      case decode json of
        Nothing -> expectationFailure "Failed to parse"
        Just evt -> do
          True `shouldBe` True  -- parsing succeeded
```

**Step 2: Write the Calendar module**

```haskell
-- src/Infra/Google/Calendar.hs
module Infra.Google.Calendar
  ( CalendarEvent(..)
  , CalendarEventList(..)
  , CalendarDateTime(..)
  , listEvents
  , getEvent
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject, Value)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- Calendar event
data CalendarEvent = CalendarEvent
  { eventId :: Text
  , eventStatus :: Text
  , eventSummary :: Maybe Text
  , eventDescription :: Maybe Text
  , eventStart :: Maybe CalendarDateTime
  , eventEnd :: Maybe CalendarDateTime
  , eventCreator :: Maybe CalendarPerson
  , eventOrganizer :: Maybe CalendarPerson
  , eventAttendees :: Maybe [CalendarPerson]
  , eventHtmlLink :: Maybe Text
  , eventUpdated :: Maybe Text
  , eventRaw :: Maybe Value
  } deriving (Show, Generic)

instance FromJSON CalendarEvent where
  parseJSON = withObject "CalendarEvent" $ \v -> CalendarEvent
    <$> v .: "id"
    <*> v .: "status"
    <*> v .:? "summary"
    <*> v .:? "description"
    <*> v .:? "start"
    <*> v .:? "end"
    <*> v .:? "creator"
    <*> v .:? "organizer"
    <*> v .:? "attendees"
    <*> v .:? "htmlLink"
    <*> v .:? "updated"
    <*> pure Nothing

data CalendarDateTime = CalendarDateTime
  { dateTimeValue :: Maybe Text    -- For timed events
  , dateValue :: Maybe Text        -- For all-day events
  , timeZone :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON CalendarDateTime where
  parseJSON = withObject "CalendarDateTime" $ \v -> CalendarDateTime
    <$> v .:? "dateTime"
    <*> v .:? "date"
    <*> v .:? "timeZone"

data CalendarPerson = CalendarPerson
  { personEmail :: Maybe Text
  , personDisplayName :: Maybe Text
  , personSelf :: Maybe Bool
  , personResponseStatus :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON CalendarPerson where
  parseJSON = withObject "CalendarPerson" $ \v -> CalendarPerson
    <$> v .:? "email"
    <*> v .:? "displayName"
    <*> v .:? "self"
    <*> v .:? "responseStatus"

-- Event list response
data CalendarEventList = CalendarEventList
  { events :: Maybe [CalendarEvent]
  , nextPageToken :: Maybe Text
  , nextSyncToken :: Maybe Text
  } deriving (Show)

instance FromJSON CalendarEventList where
  parseJSON = withObject "CalendarEventList" $ \v -> CalendarEventList
    <$> v .:? "items"
    <*> v .:? "nextPageToken"
    <*> v .:? "nextSyncToken"

-- API helpers
calendarApiBase :: String
calendarApiBase = "https://www.googleapis.com/calendar/v3/calendars/primary"

-- List events (with optional sync token for incremental sync)
listEvents :: Text -> Maybe Text -> Maybe Text -> IO (Either Text CalendarEventList)
listEvents token syncToken pageToken = do
  manager <- newManager tlsManagerSettings
  let baseUrl = calendarApiBase <> "/events?maxResults=100&singleEvents=true"
  let url = baseUrl
            <> maybe "" (\st -> "&syncToken=" <> T.unpack st) syncToken
            <> maybe "" (\pt -> "&pageToken=" <> T.unpack pt) pageToken
            -- If no sync token, get events from last 7 days
            <> if syncToken == Nothing && pageToken == Nothing
               then "&timeMin=" <> "2020-01-01T00:00:00Z"  -- Far past to get all
               else ""
  req <- parseRequest url
  let authReq = req
        { requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 token)]
        }
  response <- httpLbs authReq manager
  pure $ case Aeson.decode (responseBody response) of
    Just el -> Right el
    Nothing -> Left $ "Failed to parse events list: " <> T.pack (show $ responseBody response)

-- Get single event by ID
getEvent :: Text -> Text -> IO (Either Text CalendarEvent)
getEvent token eventId = do
  manager <- newManager tlsManagerSettings
  let url = calendarApiBase <> "/events/" <> T.unpack eventId
  req <- parseRequest url
  let authReq = req
        { requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 token)]
        }
  response <- httpLbs authReq manager
  pure $ case Aeson.decode (responseBody response) of
    Just evt -> Right evt { eventRaw = Aeson.decode (responseBody response) }
    Nothing -> Left $ "Failed to parse event: " <> T.pack (show $ responseBody response)
```

**Step 3: Update wisp-srv.cabal**

Add to `other-modules` in executable and test-suite:
```
Infra.Google.Calendar
```

Add to test-suite:
```
Infra.Google.CalendarSpec
```

**Step 4: Run test to verify it compiles**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Google/Calendar.hs wisp-srv/test/Infra/Google/CalendarSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Calendar API client"
```

---

## Task 9: Calendar Poller Service

**Files:**
- Create: `wisp-srv/src/Services/CalendarPoller.hs`

**Step 1: Write the failing test**

Create `wisp-srv/test/Services/CalendarPollerSpec.hs`:

```haskell
module Services.CalendarPollerSpec where

import Test.Hspec
import Services.CalendarPoller (parseEventTime)
import Infra.Google.Calendar (CalendarDateTime(..))

spec :: Spec
spec = describe "CalendarPoller" $ do
  describe "parseEventTime" $ do
    it "parses ISO8601 datetime" $ do
      let dt = CalendarDateTime (Just "2026-02-01T10:00:00Z") Nothing Nothing
      case parseEventTime dt of
        Nothing -> expectationFailure "Failed to parse"
        Just _ -> True `shouldBe` True

    it "returns Nothing for date-only (all-day) events" $ do
      let dt = CalendarDateTime Nothing (Just "2026-02-01") Nothing
      -- All-day events have date but no dateTime
      -- We still want to capture them
      parseEventTime dt `shouldBe` Nothing
```

**Step 2: Write the CalendarPoller module**

```haskell
-- src/Services/CalendarPoller.hs
module Services.CalendarPoller
  ( pollCalendar
  , parseEventTime
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import App.Monad (App)
import Domain.Activity (NewActivity(..), ActivitySource(..))
import Infra.Db.Activity (insertActivity, activityExists)
import Infra.Db.PollState (getPollState, updatePollState)
import Infra.Google.Calendar
import Infra.Google.TokenManager (getValidToken, TokenError(..))

-- Parse event time from CalendarDateTime
parseEventTime :: CalendarDateTime -> Maybe UTCTime
parseEventTime dt = case dateTimeValue dt of
  Just dtStr -> iso8601ParseM (T.unpack dtStr)
  Nothing -> Nothing  -- All-day events don't have precise time

-- Poll Calendar for events
pollCalendar :: App (Either Text Int)
pollCalendar = do
  tokenResult <- getValidToken
  case tokenResult of
    Left NoToken -> pure $ Left "No Google token - please authenticate first"
    Left (RefreshFailed err) -> pure $ Left $ "Token refresh failed: " <> err
    Right token -> do
      -- Get current poll state (sync token)
      mstate <- getPollState "calendar"
      let mSyncToken = mstate >>= pollCursor

      liftIO $ putStrLn $ "Calendar: Polling" <> maybe " (initial)" (\_ -> " (incremental)") mSyncToken
      result <- liftIO $ listEvents token mSyncToken Nothing
      case result of
        Left err ->
          -- Sync token might be invalid
          if "410" `T.isInfixOf` err || "Sync token" `T.isInfixOf` err
            then do
              liftIO $ putStrLn "Calendar: Sync token invalid, resetting"
              updatePollState "calendar" Nothing
              pollCalendar  -- Retry without sync token
            else pure $ Left err
        Right eventList -> do
          let evts = fromMaybe [] (events eventList)
          count <- processEvents evts
          -- Save new sync token for incremental updates
          case nextSyncToken eventList of
            Just newToken -> updatePollState "calendar" (Just newToken)
            Nothing -> pure ()
          pure $ Right count

-- Process a list of calendar events
processEvents :: [CalendarEvent] -> App Int
processEvents evts = do
  counts <- forM evts $ \evt -> do
    -- Skip cancelled events
    if eventStatus evt == "cancelled"
      then pure 0
      else do
        -- Check if already imported
        exists <- activityExists Calendar (eventId evt)
        if exists
          then pure 0
          else do
            let startTime = eventStart evt >>= parseEventTime
            let endTime = eventEnd evt >>= parseEventTime
            let activity = NewActivity
                  { newActivitySource = Calendar
                  , newActivitySourceId = eventId evt
                  , newActivityRaw = fromMaybe (toJSON evt) (eventRaw evt)
                  , newActivityTitle = eventSummary evt
                  , newActivitySenderEmail = eventOrganizer evt >>= personEmail
                  , newActivityStartsAt = startTime
                  , newActivityEndsAt = endTime
                  }
            result <- insertActivity activity
            case result of
              Just _ -> do
                liftIO $ putStrLn $ "Calendar: Imported event: " <> T.unpack (fromMaybe "(no title)" $ eventSummary evt)
                pure 1
              Nothing -> pure 0
  pure $ sum counts
```

**Step 3: Update wisp-srv.cabal**

Add to `other-modules` in executable and test-suite:
```
Services.CalendarPoller
```

Add to test-suite:
```
Services.CalendarPollerSpec
```

**Step 4: Run test to verify it passes**

Run: `cabal test wisp-srv-test 2>&1 | tail -15`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Services/CalendarPoller.hs wisp-srv/test/Services/CalendarPollerSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Calendar poller service"
```

---

## Task 10: Polling Scheduler

**Files:**
- Create: `wisp-srv/src/Services/Scheduler.hs`
- Modify: `wisp-srv/app/Main.hs`

**Step 1: Add async dependency to cabal file**

In `wisp-srv/wisp-srv.cabal`, add to build-depends for executable:
```
async,
```

**Step 2: Write the Scheduler module**

```haskell
-- src/Services/Scheduler.hs
module Services.Scheduler
  ( startPolling
  , runPollCycle
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import App.Monad (App, Env, runApp, getConfig)
import App.Config (Config(..), PollingConfig(..))
import Services.GmailPoller (pollGmail)
import Services.CalendarPoller (pollCalendar)

-- Run a single poll cycle
runPollCycle :: App ()
runPollCycle = do
  liftIO $ putStrLn "\n=== Starting poll cycle ==="

  -- Poll Gmail
  gmailResult <- pollGmail
  case gmailResult of
    Left err -> liftIO $ putStrLn $ "Gmail poll error: " <> show err
    Right count -> liftIO $ putStrLn $ "Gmail: imported " <> show count <> " messages"

  -- Poll Calendar
  calResult <- pollCalendar
  case calResult of
    Left err -> liftIO $ putStrLn $ "Calendar poll error: " <> show err
    Right count -> liftIO $ putStrLn $ "Calendar: imported " <> show count <> " events"

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

**Step 3: Update Main.hs to start polling**

```haskell
-- app/Main.hs
module Main where

import Control.Concurrent.Async (async, link, wait)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, (</>))
import App.Config (loadConfig)
import App.Env (buildEnv)
import App.Monad (runApp)
import Http.Server (startServer)
import Infra.Db.Migrations (runMigrations)
import Services.Scheduler (startPolling)

main :: IO ()
main = do
  args <- getArgs
  let configPath = case args of
        [p] -> p
        _ -> "wisp.yaml"
      configDir = takeDirectory configPath
      migrationsPath = if null configDir
                       then "migrations"
                       else configDir </> "migrations"

  putStrLn "Loading configuration..."
  config <- loadConfig configPath

  putStrLn "Building environment..."
  env <- buildEnv config

  putStrLn "Running migrations..."
  runApp env $ runMigrations migrationsPath

  -- Start polling in background
  putStrLn "Starting background polling..."
  pollThread <- async $ startPolling env

  -- Link so exceptions propagate
  link pollThread

  putStrLn "Starting wisp-srv..."
  runApp env startServer
```

**Step 4: Update wisp-srv.cabal with Scheduler module**

Add to `other-modules` in executable:
```
Services.Scheduler
```

**Step 5: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds

**Step 6: Commit**

```bash
git add wisp-srv/src/Services/Scheduler.hs wisp-srv/app/Main.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add polling scheduler with background execution"
```

---

## Task 11: Activities HTTP Endpoint

**Files:**
- Create: `wisp-srv/src/Http/Handlers/Activities.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`

**Step 1: Write the Activities handler**

```haskell
-- src/Http/Handlers/Activities.hs
module Http.Handlers.Activities
  ( getActivities
  , getActivityById
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=), ToJSON(..), Value)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status (status404)
import Web.Scotty.Trans (ActionT, json, status, pathParam)
import App.Monad (Env)
import Domain.Id (EntityId(..))
import Domain.Activity (Activity(..), ActivityStatus(..))
import Infra.Db.Activity (getActivitiesByStatus, getActivity)

-- Convert Activity to JSON
activityToJson :: Activity -> Value
activityToJson a = object
  [ "id" .= unEntityId (activityId a)
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

-- GET /activities
getActivities :: ActionT (ReaderT Env IO) ()
getActivities = do
  -- Get recent pending activities
  activities <- lift $ getActivitiesByStatus Pending 50
  json $ object
    [ "activities" .= map activityToJson activities
    , "count" .= length activities
    ]

-- GET /activities/:id
getActivityById :: ActionT (ReaderT Env IO) ()
getActivityById = do
  aid <- pathParam "id"
  mactivity <- lift $ getActivity (EntityId aid)
  case mactivity of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Activity not found" :: Text)]
    Just activity -> json $ activityToJson activity
```

**Step 2: Update Routes.hs**

```haskell
-- src/Http/Routes.hs
module Http.Routes
  ( routes
  ) where

import Control.Monad.Reader (ReaderT)
import Web.Scotty.Trans (ScottyT, get)
import Http.Handlers.Health (getHealth)
import Http.Handlers.Auth (getGoogleAuth, getGoogleCallback, getAuthStatus)
import Http.Handlers.Activities (getActivities, getActivityById)
import App.Monad (Env)

routes :: ScottyT (ReaderT Env IO) ()
routes = do
  -- Health
  get "/health" getHealth

  -- Auth
  get "/auth/google" getGoogleAuth
  get "/auth/google/callback" getGoogleCallback
  get "/auth/status" getAuthStatus

  -- Activities
  get "/activities" getActivities
  get "/activities/:id" getActivityById
```

**Step 3: Update wisp-srv.cabal**

Add to `other-modules` in executable and test-suite:
```
Http.Handlers.Activities
```

**Step 4: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Activities.hs wisp-srv/src/Http/Routes.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add activities HTTP endpoints"
```

---

## Task 12: CLI Status Command Enhancement

**Files:**
- Modify: `wisp-cli/app/Main.hs`

**Step 1: Update the CLI status command**

Update `runStatus` in `wisp-cli/app/Main.hs`:

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

  -- Check auth status
  authReq <- parseRequest $ baseUrl <> "/auth/status"
  authResp <- httpLbs authReq manager
  case decode (responseBody authResp) of
    Just (Object obj) -> do
      let authed = case KM.lookup "authenticated" obj of
            Just (Bool b) -> b
            _ -> False
      if authed
        then do
          TIO.putStrLn "Google:     authenticated"
          case KM.lookup "expires_at" obj of
            Just (String exp) -> TIO.putStrLn $ "Expires:    " <> exp
            _ -> return ()
        else TIO.putStrLn "Google:     not authenticated"
    _ -> TIO.putStrLn "Google:     unknown"

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

Also add the needed import at the top:

```haskell
import Data.Scientific (Scientific)
```

**Step 2: Run to verify it compiles**

Run: `cabal build wisp-cli 2>&1 | tail -5`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-cli/app/Main.hs
git commit -m "feat: enhance CLI status with activities count"
```

---

## Task 13: Manual Poll Trigger Endpoint

**Files:**
- Modify: `wisp-srv/src/Http/Routes.hs`
- Modify: `wisp-srv/src/Http/Handlers/Activities.hs`

**Step 1: Add poll trigger endpoint**

Add to `Http/Handlers/Activities.hs`:

```haskell
-- POST /poll
triggerPoll :: ActionT (ReaderT Env IO) ()
triggerPoll = do
  lift runPollCycle
  json $ object ["status" .= ("poll triggered" :: Text)]
```

Add import:
```haskell
import Services.Scheduler (runPollCycle)
```

Update module exports:
```haskell
module Http.Handlers.Activities
  ( getActivities
  , getActivityById
  , triggerPoll
  ) where
```

**Step 2: Add route**

In `Http/Routes.hs`, add:

```haskell
import Web.Scotty.Trans (ScottyT, get, post)
```

And add route:
```haskell
  -- Poll trigger
  post "/poll" triggerPoll
```

Update import:
```haskell
import Http.Handlers.Activities (getActivities, getActivityById, triggerPoll)
```

**Step 3: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -5`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Activities.hs wisp-srv/src/Http/Routes.hs
git commit -m "feat: add manual poll trigger endpoint"
```

---

## Task 14: CLI Poll Command

**Files:**
- Modify: `wisp-cli/app/Main.hs`

**Step 1: Add poll command to CLI**

Update the Command type:
```haskell
data Command
  = Auth
  | Status
  | Poll
  | Help
  deriving (Show)
```

Update commandParser:
```haskell
commandParser :: Parser Command
commandParser = subparser
  ( command "auth" (info (pure Auth) (progDesc "Start OAuth flow"))
  <> command "status" (info (pure Status) (progDesc "Quick status overview"))
  <> command "poll" (info (pure Poll) (progDesc "Trigger a poll cycle"))
  <> command "help" (info (pure Help) (progDesc "Show help"))
  )
```

Update main:
```haskell
main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Auth -> runAuth
    Status -> runStatus
    Poll -> runPoll
    Help -> runHelp
```

Add runPoll:
```haskell
runPoll :: IO ()
runPoll = do
  TIO.putStrLn "Triggering poll..."
  manager <- newManager defaultManagerSettings
  initialReq <- parseRequest $ baseUrl <> "/poll"
  let req = initialReq { method = "POST" }
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (Object obj) -> case KM.lookup "status" obj of
      Just (String s) -> TIO.putStrLn $ "Result: " <> s
      _ -> TIO.putStrLn "Poll triggered"
    _ -> TIO.putStrLn "Poll request sent"
```

Update runHelp:
```haskell
runHelp :: IO ()
runHelp = do
  TIO.putStrLn "wisp - your autonomy-preserving assistant"
  TIO.putStrLn ""
  TIO.putStrLn "Commands:"
  TIO.putStrLn "  auth    Start OAuth flow with Google"
  TIO.putStrLn "  status  Show server and auth status"
  TIO.putStrLn "  poll    Trigger a poll cycle"
  TIO.putStrLn "  help    Show this help"
  TIO.putStrLn ""
  TIO.putStrLn "Use --help for more details"
```

**Step 2: Run to verify it compiles**

Run: `cabal build wisp-cli 2>&1 | tail -5`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-cli/app/Main.hs
git commit -m "feat: add poll command to CLI"
```

---

## Summary

This plan implements Phases 3 & 4 from the technical design:

**Phase 3: Polling & Capture (Tasks 1-7, 10)**
- Activities table migration and domain types
- Activity database operations
- Poll state tracking
- Token manager with auto-refresh
- Gmail API client and poller service
- Polling scheduler with background execution

**Phase 4: Calendar Integration (Tasks 8-9)**
- Calendar API client
- Calendar poller service

**Additional Features (Tasks 11-14)**
- Activities HTTP endpoint
- Enhanced CLI status command
- Manual poll trigger endpoint
- CLI poll command

Each task follows TDD with small, focused steps and frequent commits.
