# Classification Pipeline Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add AI-powered classification to transform raw emails/events from "pending" into actionable, categorized activities with urgency, autonomy tier, and routing.

**Architecture:** A three-stage pipeline: (1) Classify - LLM analyzes raw activity and outputs structured classification, (2) Resolve People - match sender to people table (create if new), (3) Route - deterministically set status based on confidence + autonomy tier. Each stage is a separate service that can be called from the scheduler.

**Tech Stack:** Haskell, http-client-tls (for Claude API), aeson, postgresql-simple

---

## Task 1: People Table Migration

**Files:**
- Create: `wisp-srv/migrations/008_people.sql`

**Step 1: Write the migration file**

```sql
-- migrations/008_people.sql

-- People table for contact management
create table if not exists people (
  id text primary key,
  email text unique not null,
  display_name text,
  personas text[],                  -- ['work'], ['home'], etc.
  relationship text,                -- 'colleague', 'investor', 'spouse', etc.
  organisation text,
  notes text,
  first_contact timestamptz,
  last_contact timestamptz,
  contact_count int not null default 0,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

-- Index for email lookups
create index if not exists people_by_email on people (email);

-- Add person_id FK to activities (column already exists from 006_activities.sql)
-- Just add the foreign key constraint
alter table activities
  add constraint activities_person_fk
  foreign key (person_id) references people(id);
```

**Step 2: Verify migration file exists**

Run: `head -10 wisp-srv/migrations/008_people.sql`
Expected: Shows the SQL header

**Step 3: Commit**

```bash
git add wisp-srv/migrations/008_people.sql
git commit -m "feat: add people table migration"
```

---

## Task 2: Person Domain Type

**Files:**
- Create: `wisp-srv/src/Domain/Person.hs`
- Create: `wisp-srv/test/Domain/PersonSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Domain/PersonSpec.hs`:

```haskell
module Domain.PersonSpec where

import Test.Hspec
import Domain.Person (Person(..), personEmail, personDisplayName)
import Data.Aeson (encode, decode, toJSON)
import Data.Text (Text)

spec :: Spec
spec = describe "Person" $ do
  describe "JSON serialization" $ do
    it "serializes email field correctly" $ do
      let json = toJSON (testPerson "test@example.com" (Just "Test User"))
      -- Just verify it encodes without error
      encode json `shouldNotBe` ""

testPerson :: Text -> Maybe Text -> Person
testPerson email name = Person
  { personId = undefined  -- We'll test this separately
  , personEmail = email
  , personDisplayName = name
  , personPersonas = Nothing
  , personRelationship = Nothing
  , personOrganisation = Nothing
  , personNotes = Nothing
  , personFirstContact = Nothing
  , personLastContact = Nothing
  , personContactCount = 0
  , personCreatedAt = undefined
  }
```

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test 2>&1 | grep -E "(PersonSpec|not found|error)" | head -5`
Expected: FAIL - module Domain.Person not found

**Step 3: Write the Person module**

Create `wisp-srv/src/Domain/Person.hs`:

```haskell
module Domain.Person
  ( Person(..)
  , NewPerson(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Id (EntityId(..))

data Person = Person
  { personId :: EntityId
  , personEmail :: Text
  , personDisplayName :: Maybe Text
  , personPersonas :: Maybe [Text]
  , personRelationship :: Maybe Text
  , personOrganisation :: Maybe Text
  , personNotes :: Maybe Text
  , personFirstContact :: Maybe UTCTime
  , personLastContact :: Maybe UTCTime
  , personContactCount :: Int
  , personCreatedAt :: UTCTime
  } deriving (Show, Eq)

instance ToJSON Person where
  toJSON p = object
    [ "id" .= unEntityId (personId p)
    , "email" .= personEmail p
    , "display_name" .= personDisplayName p
    , "personas" .= personPersonas p
    , "relationship" .= personRelationship p
    , "organisation" .= personOrganisation p
    , "notes" .= personNotes p
    , "first_contact" .= personFirstContact p
    , "last_contact" .= personLastContact p
    , "contact_count" .= personContactCount p
    , "created_at" .= personCreatedAt p
    ]

data NewPerson = NewPerson
  { newPersonEmail :: Text
  , newPersonDisplayName :: Maybe Text
  } deriving (Show, Eq)
```

**Step 4: Update wisp-srv.cabal**

Add `Domain.Person` to `other-modules` in executable section.
Add `Domain.PersonSpec` to `other-modules` in test-suite section.

**Step 5: Run test to verify it passes**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Domain/Person.hs wisp-srv/test/Domain/PersonSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Person domain type"
```

---

## Task 3: Person Database Operations

**Files:**
- Create: `wisp-srv/src/Infra/Db/Person.hs`
- Create: `wisp-srv/test/Infra/Db/PersonSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Infra/Db/PersonSpec.hs`:

```haskell
module Infra.Db.PersonSpec where

import Test.Hspec

spec :: Spec
spec = describe "Person DB" $ do
  it "placeholder - person operations need integration tests" $ do
    True `shouldBe` True
```

**Step 2: Write the Person DB module**

Create `wisp-srv/src/Infra/Db/Person.hs`:

```haskell
module Infra.Db.Person
  ( upsertPerson
  , getPersonByEmail
  , getPersonById
  , getAllPeople
  , updatePersonContact
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Domain.Id (EntityId(..), newEntityId)
import Domain.Person (Person(..), NewPerson(..))
import App.Monad (App, getConn)

instance FromRow Person where
  fromRow = Person
    <$> (EntityId <$> field)          -- id
    <*> field                          -- email
    <*> field                          -- display_name
    <*> (fmap fromPGArray <$> field)  -- personas (nullable array)
    <*> field                          -- relationship
    <*> field                          -- organisation
    <*> field                          -- notes
    <*> field                          -- first_contact
    <*> field                          -- last_contact
    <*> field                          -- contact_count
    <*> field                          -- created_at

-- Upsert person by email (returns existing or creates new)
upsertPerson :: Text -> Maybe Text -> App Person
upsertPerson email displayName = do
  conn <- getConn
  pid <- liftIO newEntityId
  _ <- liftIO $ execute conn
    "insert into people (id, email, display_name, first_contact) \
    \values (?, ?, ?, now()) \
    \on conflict (email) do update set \
    \  display_name = coalesce(excluded.display_name, people.display_name)"
    (unEntityId pid, email, displayName)
  -- Fetch the person (might be existing or newly created)
  results <- liftIO $ query conn
    "select id, email, display_name, personas, relationship, organisation, \
    \notes, first_contact, last_contact, contact_count, created_at \
    \from people where email = ?"
    (Only email)
  case results of
    [p] -> pure p
    _ -> error $ "Failed to upsert person: " <> show email

-- Get person by email
getPersonByEmail :: Text -> App (Maybe Person)
getPersonByEmail email = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, email, display_name, personas, relationship, organisation, \
    \notes, first_contact, last_contact, contact_count, created_at \
    \from people where email = ?"
    (Only email)
  pure $ case results of
    [p] -> Just p
    _ -> Nothing

-- Get person by ID
getPersonById :: EntityId -> App (Maybe Person)
getPersonById pid = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, email, display_name, personas, relationship, organisation, \
    \notes, first_contact, last_contact, contact_count, created_at \
    \from people where id = ?"
    (Only $ unEntityId pid)
  pure $ case results of
    [p] -> Just p
    _ -> Nothing

-- Get all people
getAllPeople :: App [Person]
getAllPeople = do
  conn <- getConn
  liftIO $ query_ conn
    "select id, email, display_name, personas, relationship, organisation, \
    \notes, first_contact, last_contact, contact_count, created_at \
    \from people order by last_contact desc nulls last"

-- Update last_contact and increment contact_count
updatePersonContact :: EntityId -> App ()
updatePersonContact pid = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "update people set last_contact = now(), contact_count = contact_count + 1, \
    \updated_at = now() where id = ?"
    (Only $ unEntityId pid)
  pure ()
```

**Step 3: Update wisp-srv.cabal**

Add `Infra.Db.Person` to `other-modules` in executable.
Add `Infra.Db.PersonSpec` to `other-modules` in test-suite.

**Step 4: Run test to verify it compiles**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Infra/Db/Person.hs wisp-srv/test/Infra/Db/PersonSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add person database operations"
```

---

## Task 4: Classification Domain Type

**Files:**
- Create: `wisp-srv/src/Domain/Classification.hs`
- Create: `wisp-srv/test/Domain/ClassificationSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Domain/ClassificationSpec.hs`:

```haskell
module Domain.ClassificationSpec where

import Test.Hspec
import Domain.Classification
import Data.Aeson (decode, encode)

spec :: Spec
spec = describe "Classification" $ do
  describe "JSON parsing" $ do
    it "parses a valid classification response" $ do
      let json = "{\"personas\":[\"work\"],\"activity_type\":\"request\",\
                 \\"urgency\":\"normal\",\"autonomy_tier\":2,\
                 \\"confidence\":0.85,\"summary\":\"Meeting request\"}"
      case decode json :: Maybe Classification of
        Nothing -> expectationFailure "Failed to parse Classification"
        Just c -> do
          classificationPersonas c `shouldBe` ["work"]
          classificationActivityType c `shouldBe` Request
          classificationUrgency c `shouldBe` Normal
          classificationAutonomyTier c `shouldBe` 2
          classificationConfidence c `shouldBe` 0.85
          classificationSummary c `shouldBe` "Meeting request"

  describe "ActivityType" $ do
    it "parses all activity types" $ do
      (decode "\"request\"" :: Maybe ActivityType) `shouldBe` Just Request
      (decode "\"information\"" :: Maybe ActivityType) `shouldBe` Just Information
      (decode "\"action_required\"" :: Maybe ActivityType) `shouldBe` Just ActionRequired
      (decode "\"fyi\"" :: Maybe ActivityType) `shouldBe` Just FYI
      (decode "\"event\"" :: Maybe ActivityType) `shouldBe` Just Event

  describe "Urgency" $ do
    it "parses all urgency levels" $ do
      (decode "\"high\"" :: Maybe Urgency) `shouldBe` Just High
      (decode "\"normal\"" :: Maybe Urgency) `shouldBe` Just Normal
      (decode "\"low\"" :: Maybe Urgency) `shouldBe` Just Low
```

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test 2>&1 | grep -E "(ClassificationSpec|not found|error)" | head -5`
Expected: FAIL - module Domain.Classification not found

**Step 3: Write the Classification module**

Create `wisp-srv/src/Domain/Classification.hs`:

```haskell
module Domain.Classification
  ( Classification(..)
  , ActivityType(..)
  , Urgency(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withText, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

data ActivityType
  = Request
  | Information
  | ActionRequired
  | FYI
  | Event
  deriving (Eq, Show, Generic)

instance ToJSON ActivityType where
  toJSON Request = "request"
  toJSON Information = "information"
  toJSON ActionRequired = "action_required"
  toJSON FYI = "fyi"
  toJSON Event = "event"

instance FromJSON ActivityType where
  parseJSON = withText "ActivityType" $ \case
    "request" -> pure Request
    "information" -> pure Information
    "action_required" -> pure ActionRequired
    "fyi" -> pure FYI
    "event" -> pure Event
    other -> fail $ "Invalid activity type: " <> show other

data Urgency = High | Normal | Low
  deriving (Eq, Show, Generic)

instance ToJSON Urgency where
  toJSON High = "high"
  toJSON Normal = "normal"
  toJSON Low = "low"

instance FromJSON Urgency where
  parseJSON = withText "Urgency" $ \case
    "high" -> pure High
    "normal" -> pure Normal
    "low" -> pure Low
    other -> fail $ "Invalid urgency: " <> show other

data Classification = Classification
  { classificationPersonas :: [Text]
  , classificationActivityType :: ActivityType
  , classificationUrgency :: Urgency
  , classificationAutonomyTier :: Int
  , classificationConfidence :: Double
  , classificationSummary :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Classification where
  parseJSON = withObject "Classification" $ \v -> Classification
    <$> v .: "personas"
    <*> v .: "activity_type"
    <*> v .: "urgency"
    <*> v .: "autonomy_tier"
    <*> v .: "confidence"
    <*> v .: "summary"

instance ToJSON Classification where
  toJSON c = toJSON $
    [ ("personas", toJSON $ classificationPersonas c)
    , ("activity_type", toJSON $ classificationActivityType c)
    , ("urgency", toJSON $ classificationUrgency c)
    , ("autonomy_tier", toJSON $ classificationAutonomyTier c)
    , ("confidence", toJSON $ classificationConfidence c)
    , ("summary", toJSON $ classificationSummary c)
    ]
```

**Step 4: Update wisp-srv.cabal**

Add `Domain.Classification` to `other-modules` in executable.
Add `Domain.ClassificationSpec` to `other-modules` in test-suite.

**Step 5: Run test to verify it passes**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Domain/Classification.hs wisp-srv/test/Domain/ClassificationSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Classification domain type"
```

---

## Task 5: Receipt Domain Type and Database

**Files:**
- Create: `wisp-srv/src/Domain/Receipt.hs`
- Create: `wisp-srv/src/Infra/Db/Receipt.hs`
- Create: `wisp-srv/test/Domain/ReceiptSpec.hs`
- Create: `wisp-srv/test/Infra/Db/ReceiptSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Domain/ReceiptSpec.hs`:

```haskell
module Domain.ReceiptSpec where

import Test.Hspec
import Domain.Receipt (ReceiptAction(..))
import Data.Aeson (toJSON)

spec :: Spec
spec = describe "Receipt" $ do
  describe "ReceiptAction" $ do
    it "serializes actions to JSON" $ do
      toJSON Classified `shouldBe` "classified"
      toJSON Quarantined `shouldBe` "quarantined"
      toJSON Processed `shouldBe` "processed"
      toJSON Surfaced `shouldBe` "surfaced"
```

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test 2>&1 | grep -E "(ReceiptSpec|not found|error)" | head -5`
Expected: FAIL - module Domain.Receipt not found

**Step 3: Write the Receipt domain module**

Create `wisp-srv/src/Domain/Receipt.hs`:

```haskell
module Domain.Receipt
  ( Receipt(..)
  , ReceiptAction(..)
  , NewReceipt(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Id (EntityId(..))

data ReceiptAction
  = Classified
  | Quarantined
  | Processed
  | Surfaced
  deriving (Eq, Show)

instance ToJSON ReceiptAction where
  toJSON Classified = "classified"
  toJSON Quarantined = "quarantined"
  toJSON Processed = "processed"
  toJSON Surfaced = "surfaced"

instance FromJSON ReceiptAction where
  parseJSON = withText "ReceiptAction" $ \case
    "classified" -> pure Classified
    "quarantined" -> pure Quarantined
    "processed" -> pure Processed
    "surfaced" -> pure Surfaced
    other -> fail $ "Invalid receipt action: " <> show other

data Receipt = Receipt
  { receiptId :: EntityId
  , receiptActivityId :: EntityId
  , receiptActionTaken :: ReceiptAction
  , receiptActionDetail :: Maybe Text
  , receiptConfidence :: Maybe Double
  , receiptCreatedAt :: UTCTime
  } deriving (Show)

data NewReceipt = NewReceipt
  { newReceiptActivityId :: EntityId
  , newReceiptActionTaken :: ReceiptAction
  , newReceiptActionDetail :: Maybe Text
  , newReceiptConfidence :: Maybe Double
  } deriving (Show)
```

**Step 4: Write the Receipt DB module**

Create `wisp-srv/src/Infra/Db/Receipt.hs`:

```haskell
module Infra.Db.Receipt
  ( insertReceipt
  , getReceiptsForActivity
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Domain.Id (EntityId(..), newEntityId)
import Domain.Receipt (Receipt(..), NewReceipt(..), ReceiptAction(..))
import App.Monad (App, getConn)

instance FromRow Receipt where
  fromRow = Receipt
    <$> (EntityId <$> field)          -- id
    <*> (EntityId <$> field)          -- activity_id
    <*> (parseAction <$> field)       -- action_taken
    <*> field                          -- action_detail
    <*> field                          -- confidence
    <*> field                          -- created_at
    where
      parseAction :: Text -> ReceiptAction
      parseAction "classified" = Classified
      parseAction "quarantined" = Quarantined
      parseAction "processed" = Processed
      parseAction "surfaced" = Surfaced
      parseAction _ = Classified  -- default

-- Insert a new receipt
insertReceipt :: NewReceipt -> App EntityId
insertReceipt new = do
  conn <- getConn
  rid <- liftIO newEntityId
  let actionText = case newReceiptActionTaken new of
        Classified -> "classified" :: Text
        Quarantined -> "quarantined"
        Processed -> "processed"
        Surfaced -> "surfaced"
  _ <- liftIO $ execute conn
    "insert into receipts (id, activity_id, action_taken, action_detail, confidence) \
    \values (?, ?, ?, ?, ?)"
    ( unEntityId rid
    , unEntityId (newReceiptActivityId new)
    , actionText
    , newReceiptActionDetail new
    , newReceiptConfidence new
    )
  pure rid

-- Get all receipts for an activity
getReceiptsForActivity :: EntityId -> App [Receipt]
getReceiptsForActivity actId = do
  conn <- getConn
  liftIO $ query conn
    "select id, activity_id, action_taken, action_detail, confidence, created_at \
    \from receipts where activity_id = ? order by created_at"
    (Only $ unEntityId actId)
```

**Step 5: Create placeholder test**

Create `wisp-srv/test/Infra/Db/ReceiptSpec.hs`:

```haskell
module Infra.Db.ReceiptSpec where

import Test.Hspec

spec :: Spec
spec = describe "Receipt DB" $ do
  it "placeholder - receipt operations need integration tests" $ do
    True `shouldBe` True
```

**Step 6: Update wisp-srv.cabal**

Add to executable `other-modules`:
- `Domain.Receipt`
- `Infra.Db.Receipt`

Add to test-suite `other-modules`:
- `Domain.ReceiptSpec`
- `Infra.Db.ReceiptSpec`

**Step 7: Run test to verify it passes**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 8: Commit**

```bash
git add wisp-srv/src/Domain/Receipt.hs wisp-srv/src/Infra/Db/Receipt.hs \
        wisp-srv/test/Domain/ReceiptSpec.hs wisp-srv/test/Infra/Db/ReceiptSpec.hs \
        wisp-srv/wisp-srv.cabal
git commit -m "feat: add Receipt domain type and database operations"
```

---

## Task 6: Update Config for Claude API

**Files:**
- Modify: `wisp-srv/src/App/Config.hs`
- Modify: `wisp-srv/wisp.yaml`
- Modify: `wisp-srv/test/App/ConfigSpec.hs`

**Step 1: Update Config.hs with Claude settings**

Add to `wisp-srv/src/App/Config.hs`:

```haskell
-- Add new config type after ClassificationConfig:

data ClaudeConfig = ClaudeConfig
  { apiKey :: Text
  , model :: Text
  } deriving (Generic, Show)

instance FromJSON ClaudeConfig

-- Update Config to include claude:
data Config = Config
  { server :: ServerConfig
  , database :: DatabaseConfig
  , google :: GoogleConfig
  , polling :: PollingConfig
  , classification :: ClassificationConfig
  , claude :: ClaudeConfig          -- ADD THIS
  } deriving (Generic, Show)

-- Update loadConfig to handle ANTHROPIC_API_KEY env var:
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
      mAnthropicKey <- lookupEnv "ANTHROPIC_API_KEY"   -- ADD THIS
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
        , claude = cfg.claude                         -- ADD THIS
            { apiKey = maybe cfg.claude.apiKey T.pack mAnthropicKey
            }
        }
```

**Step 2: Update wisp.yaml**

Add to `wisp-srv/wisp.yaml`:

```yaml
claude:
  apiKey: "set-via-env"
  model: "claude-sonnet-4-20250514"
```

**Step 3: Run test to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/App/Config.hs wisp-srv/wisp.yaml
git commit -m "feat: add Claude API configuration"
```

---

## Task 7: Claude API Client

**Files:**
- Create: `wisp-srv/src/Infra/Claude/Client.hs`
- Create: `wisp-srv/test/Infra/Claude/ClientSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Infra/Claude/ClientSpec.hs`:

```haskell
module Infra.Claude.ClientSpec where

import Test.Hspec
import Infra.Claude.Client (ClaudeResponse(..), parseClaudeResponse)
import Data.Aeson (decode)

spec :: Spec
spec = describe "Claude Client" $ do
  describe "parseClaudeResponse" $ do
    it "extracts text from Claude API response" $ do
      let json = "{\"content\":[{\"type\":\"text\",\"text\":\"Hello world\"}],\
                 \\"model\":\"claude-sonnet-4-20250514\",\"stop_reason\":\"end_turn\"}"
      case decode json :: Maybe ClaudeResponse of
        Nothing -> expectationFailure "Failed to parse Claude response"
        Just resp -> responseText resp `shouldBe` Just "Hello world"

    it "handles empty content" $ do
      let json = "{\"content\":[],\"model\":\"claude-sonnet-4-20250514\",\"stop_reason\":\"end_turn\"}"
      case decode json :: Maybe ClaudeResponse of
        Nothing -> expectationFailure "Failed to parse Claude response"
        Just resp -> responseText resp `shouldBe` Nothing
```

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test 2>&1 | grep -E "(ClientSpec|not found|error)" | head -5`
Expected: FAIL - module Infra.Claude.Client not found

**Step 3: Write the Claude Client module**

Create `wisp-srv/src/Infra/Claude/Client.hs`:

```haskell
module Infra.Claude.Client
  ( ClaudeResponse(..)
  , ClaudeContent(..)
  , callClaude
  , responseText
  , parseClaudeResponse
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.:?), object, (.=), encode)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

data ClaudeContent = ClaudeContent
  { contentType :: Text
  , contentText :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON ClaudeContent where
  parseJSON = withObject "ClaudeContent" $ \v -> ClaudeContent
    <$> v .: "type"
    <*> v .:? "text"

data ClaudeResponse = ClaudeResponse
  { responseContent :: [ClaudeContent]
  , responseModel :: Text
  , responseStopReason :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON ClaudeResponse where
  parseJSON = withObject "ClaudeResponse" $ \v -> ClaudeResponse
    <$> v .: "content"
    <*> v .: "model"
    <*> v .:? "stop_reason"

-- Extract text from response
responseText :: ClaudeResponse -> Maybe Text
responseText resp = case responseContent resp of
  [] -> Nothing
  (c:_) -> contentText c

-- For parsing
parseClaudeResponse :: ClaudeResponse -> Maybe Text
parseClaudeResponse = responseText

-- Call Claude API
callClaude :: Text -> Text -> Text -> IO (Either Text Text)
callClaude apiKey model prompt = do
  manager <- newManager tlsManagerSettings
  let url = "https://api.anthropic.com/v1/messages"
  initReq <- parseRequest url
  let reqBody = object
        [ "model" .= model
        , "max_tokens" .= (1024 :: Int)
        , "messages" .= [object ["role" .= ("user" :: Text), "content" .= prompt]]
        ]
  let req = initReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("x-api-key", encodeUtf8 apiKey)
            , ("anthropic-version", "2023-06-01")
            ]
        , requestBody = RequestBodyLBS (encode reqBody)
        }
  response <- httpLbs req manager
  let status = statusCode (responseStatus response)
  if status == 200
    then case decodeResponse (responseBody response) of
      Just txt -> pure $ Right txt
      Nothing -> pure $ Left "Failed to parse Claude response"
    else pure $ Left $ "Claude API error: " <> decodeUtf8 (toStrict $ responseBody response)
  where
    decodeResponse body = do
      resp <- Data.Aeson.decode body :: Maybe ClaudeResponse
      responseText resp
```

**Step 4: Update wisp-srv.cabal**

Add `Infra.Claude.Client` to `other-modules` in executable.
Add `Infra.Claude.ClientSpec` to `other-modules` in test-suite.

**Step 5: Run test to verify it passes**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Infra/Claude/Client.hs wisp-srv/test/Infra/Claude/ClientSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add Claude API client"
```

---

## Task 8: Classification Service

**Files:**
- Create: `wisp-srv/src/Services/Classifier.hs`
- Create: `wisp-srv/test/Services/ClassifierSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Services/ClassifierSpec.hs`:

```haskell
module Services.ClassifierSpec where

import Test.Hspec
import Services.Classifier (buildClassificationPrompt, parseClassificationResponse)
import Domain.Classification (Classification(..), ActivityType(..), Urgency(..))
import Data.Aeson (Value, object, (.=))

spec :: Spec
spec = describe "Classifier" $ do
  describe "buildClassificationPrompt" $ do
    it "includes email subject in prompt" $ do
      let raw = object ["snippet" .= ("Meeting tomorrow" :: String)]
      let prompt = buildClassificationPrompt "email" (Just "Re: Project Update") raw
      "Project Update" `shouldSatisfy` (`elem` words (show prompt))

  describe "parseClassificationResponse" $ do
    it "parses valid JSON classification" $ do
      let json = "{\"personas\":[\"work\"],\"activity_type\":\"request\",\
                 \\"urgency\":\"normal\",\"autonomy_tier\":2,\
                 \\"confidence\":0.85,\"summary\":\"Meeting request\"}"
      case parseClassificationResponse json of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right c -> do
          classificationActivityType c `shouldBe` Request
          classificationConfidence c `shouldBe` 0.85

    it "fails on invalid JSON" $ do
      let json = "not valid json"
      case parseClassificationResponse json of
        Left _ -> pure ()  -- Expected
        Right _ -> expectationFailure "Should have failed"
```

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test 2>&1 | grep -E "(ClassifierSpec|not found|error)" | head -5`
Expected: FAIL - module Services.Classifier not found

**Step 3: Write the Classifier service**

Create `wisp-srv/src/Services/Classifier.hs`:

```haskell
module Services.Classifier
  ( classifyActivity
  , buildClassificationPrompt
  , parseClassificationResponse
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import App.Monad (App, getConfig)
import App.Config (Config(..))
import Domain.Activity (Activity(..), ActivitySource(..))
import Domain.Classification (Classification)
import Infra.Claude.Client (callClaude)

-- Classify an activity using Claude
classifyActivity :: Activity -> App (Either Text Classification)
classifyActivity activity = do
  cfg <- getConfig
  let apiKey = cfg.claude.apiKey
  let model = cfg.claude.model
  let source = case activitySource activity of
        Email -> "email"
        Calendar -> "calendar"
  let prompt = buildClassificationPrompt source (activityTitle activity) (activityRaw activity)
  result <- liftIO $ callClaude apiKey model prompt
  pure $ case result of
    Left err -> Left err
    Right responseText -> parseClassificationResponse responseText

-- Build the classification prompt
buildClassificationPrompt :: Text -> Maybe Text -> Value -> Text
buildClassificationPrompt source mTitle raw = T.unlines
  [ "You are classifying an incoming " <> source <> " for a personal assistant."
  , ""
  , "Analyze the following and respond with ONLY a JSON object (no markdown, no explanation):"
  , ""
  , "Title: " <> maybe "(none)" id mTitle
  , ""
  , "Raw data:"
  , TL.toStrict $ TLE.decodeUtf8 $ encode raw
  , ""
  , "Respond with this exact JSON structure:"
  , "{"
  , "  \"personas\": [\"work\"|\"home\"|\"personal\"],  // which life areas this relates to"
  , "  \"activity_type\": \"request\"|\"information\"|\"action_required\"|\"fyi\"|\"event\","
  , "  \"urgency\": \"high\"|\"normal\"|\"low\","
  , "  \"autonomy_tier\": 1-4,  // 1=ignore, 2=note, 3=draft response, 4=needs attention"
  , "  \"confidence\": 0.0-1.0,  // how confident you are in this classification"
  , "  \"summary\": \"Brief 1-sentence summary\""
  , "}"
  , ""
  , "Guidelines for autonomy_tier:"
  , "- Tier 1: Automated notifications, newsletters, receipts - can be silently processed"
  , "- Tier 2: FYI items, updates, non-urgent info - note but don't surface"
  , "- Tier 3: Requests needing response, calendar invites - draft action for review"
  , "- Tier 4: Urgent items, important people, time-sensitive - surface immediately"
  ]

-- Parse classification response from Claude
parseClassificationResponse :: Text -> Either Text Classification
parseClassificationResponse txt =
  case Data.Aeson.decode (TLE.encodeUtf8 $ TL.fromStrict txt) of
    Just c -> Right c
    Nothing -> Left $ "Failed to parse classification: " <> T.take 200 txt

-- Need this import
import qualified Data.Aeson (decode)
```

**Step 4: Update wisp-srv.cabal**

Add `Services.Classifier` to `other-modules` in executable.
Add `Services.ClassifierSpec` to `other-modules` in test-suite.

**Step 5: Run test to verify it passes**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Services/Classifier.hs wisp-srv/test/Services/ClassifierSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add classification service with Claude integration"
```

---

## Task 9: People Resolution Service

**Files:**
- Create: `wisp-srv/src/Services/PeopleResolver.hs`
- Create: `wisp-srv/test/Services/PeopleResolverSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Services/PeopleResolverSpec.hs`:

```haskell
module Services.PeopleResolverSpec where

import Test.Hspec
import Services.PeopleResolver (extractDisplayName)

spec :: Spec
spec = describe "PeopleResolver" $ do
  describe "extractDisplayName" $ do
    it "extracts name from 'Name <email>' format" $ do
      extractDisplayName "John Doe <john@example.com>" `shouldBe` Just "John Doe"

    it "extracts name with quotes" $ do
      extractDisplayName "\"Jane Smith\" <jane@example.com>" `shouldBe` Just "Jane Smith"

    it "returns Nothing for plain email" $ do
      extractDisplayName "john@example.com" `shouldBe` Nothing

    it "handles empty name before angle brackets" $ do
      extractDisplayName "<john@example.com>" `shouldBe` Nothing
```

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test 2>&1 | grep -E "(PeopleResolverSpec|not found|error)" | head -5`
Expected: FAIL - module Services.PeopleResolver not found

**Step 3: Write the PeopleResolver service**

Create `wisp-srv/src/Services/PeopleResolver.hs`:

```haskell
module Services.PeopleResolver
  ( resolvePersonForActivity
  , extractDisplayName
  , extractEmail
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import App.Monad (App)
import Domain.Id (EntityId)
import Domain.Activity (Activity(..))
import Domain.Person (Person(..))
import Infra.Db.Person (upsertPerson, getPersonByEmail, updatePersonContact)

-- Resolve or create person for an activity's sender
resolvePersonForActivity :: Activity -> App (Maybe Person)
resolvePersonForActivity activity = case activitySenderEmail activity of
  Nothing -> pure Nothing
  Just senderField -> do
    let email = extractEmail senderField
    let displayName = extractDisplayName senderField
    -- Upsert creates if not exists, updates display_name if we have a better one
    person <- upsertPerson email displayName
    -- Update contact stats
    updatePersonContact (personId person)
    pure $ Just person

-- Extract display name from "Name <email>" format
extractDisplayName :: Text -> Maybe Text
extractDisplayName field =
  let trimmed = T.strip field
  in if "<" `T.isInfixOf` trimmed
     then
       let namePart = T.strip $ T.takeWhile (/= '<') trimmed
           -- Remove surrounding quotes if present
           unquoted = T.dropAround (`elem` ['"', '\'']) namePart
       in if T.null unquoted then Nothing else Just unquoted
     else Nothing

-- Extract email from "Name <email>" or plain "email" format
extractEmail :: Text -> Text
extractEmail field =
  let trimmed = T.strip field
  in if "<" `T.isInfixOf` trimmed
     then
       let afterLt = T.drop 1 $ T.dropWhile (/= '<') trimmed
           email = T.takeWhile (/= '>') afterLt
       in T.strip email
     else trimmed
```

**Step 4: Update wisp-srv.cabal**

Add `Services.PeopleResolver` to `other-modules` in executable.
Add `Services.PeopleResolverSpec` to `other-modules` in test-suite.

**Step 5: Run test to verify it passes**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Services/PeopleResolver.hs wisp-srv/test/Services/PeopleResolverSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add people resolution service"
```

---

## Task 10: Routing Service

**Files:**
- Create: `wisp-srv/src/Services/Router.hs`
- Create: `wisp-srv/test/Services/RouterSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Services/RouterSpec.hs`:

```haskell
module Services.RouterSpec where

import Test.Hspec
import Services.Router (determineStatus)
import Domain.Activity (ActivityStatus(..))
import Domain.Classification (Classification(..), ActivityType(..), Urgency(..))

spec :: Spec
spec = describe "Router" $ do
  describe "determineStatus" $ do
    it "quarantines low confidence items" $ do
      let c = mkClassification 0.3 2
      determineStatus 0.5 c `shouldBe` Quarantined

    it "processes tier 1 silently" $ do
      let c = mkClassification 0.9 1
      determineStatus 0.5 c `shouldBe` Processed

    it "processes tier 2 as noted" $ do
      let c = mkClassification 0.9 2
      determineStatus 0.5 c `shouldBe` Processed

    it "keeps tier 3 pending for review" $ do
      let c = mkClassification 0.9 3
      determineStatus 0.5 c `shouldBe` Pending

    it "surfaces tier 4 items" $ do
      let c = mkClassification 0.9 4
      determineStatus 0.5 c `shouldBe` Surfaced

mkClassification :: Double -> Int -> Classification
mkClassification conf tier = Classification
  { classificationPersonas = ["work"]
  , classificationActivityType = Request
  , classificationUrgency = Normal
  , classificationAutonomyTier = tier
  , classificationConfidence = conf
  , classificationSummary = "Test"
  }
```

**Step 2: Run test to verify it fails**

Run: `cabal test wisp-srv-test 2>&1 | grep -E "(RouterSpec|not found|error)" | head -5`
Expected: FAIL - module Services.Router not found

**Step 3: Write the Router service**

Create `wisp-srv/src/Services/Router.hs`:

```haskell
module Services.Router
  ( routeActivity
  , determineStatus
  ) where

import App.Monad (App, getConfig)
import App.Config (Config(..), ClassificationConfig(..))
import Domain.Id (EntityId)
import Domain.Activity (Activity(..), ActivityStatus(..))
import Domain.Classification (Classification(..))
import Domain.Receipt (NewReceipt(..), ReceiptAction(..))
import Infra.Db.Activity (updateActivityStatus)
import Infra.Db.Receipt (insertReceipt)

-- Route an activity based on its classification
routeActivity :: Activity -> Classification -> App ActivityStatus
routeActivity activity classification = do
  cfg <- getConfig
  let threshold = cfg.classification.confidenceThreshold
  let newStatus = determineStatus threshold classification

  -- Update activity status
  updateActivityStatus (activityId activity) newStatus

  -- Log receipt
  let action = statusToAction newStatus
  let detail = buildActionDetail classification newStatus
  _ <- insertReceipt NewReceipt
    { newReceiptActivityId = activityId activity
    , newReceiptActionTaken = action
    , newReceiptActionDetail = Just detail
    , newReceiptConfidence = Just (classificationConfidence classification)
    }

  pure newStatus

-- Determine status based on confidence and tier
determineStatus :: Double -> Classification -> ActivityStatus
determineStatus threshold classification
  | classificationConfidence classification < threshold = Quarantined
  | classificationAutonomyTier classification == 1 = Processed
  | classificationAutonomyTier classification == 2 = Processed
  | classificationAutonomyTier classification == 3 = Pending
  | classificationAutonomyTier classification >= 4 = Surfaced
  | otherwise = Pending

-- Map status to receipt action
statusToAction :: ActivityStatus -> ReceiptAction
statusToAction Quarantined = Quarantined
statusToAction Processed = Processed
statusToAction Surfaced = Surfaced
statusToAction _ = Classified

-- Build human-readable action detail
buildActionDetail :: Classification -> ActivityStatus -> Text
buildActionDetail c status =
  let tierDesc = case classificationAutonomyTier c of
        1 -> "Tier 1: Silent processing"
        2 -> "Tier 2: Noted"
        3 -> "Tier 3: Needs review"
        4 -> "Tier 4: Surfaced for attention"
        _ -> "Unknown tier"
      statusDesc = case status of
        Quarantined -> "Low confidence, quarantined for review"
        Processed -> "Processed automatically"
        Surfaced -> "Surfaced for user attention"
        Pending -> "Awaiting review"
        Archived -> "Archived"
  in tierDesc <> ". " <> statusDesc <> ". Summary: " <> classificationSummary c

-- Need Text import
import Data.Text (Text)
```

**Step 4: Update wisp-srv.cabal**

Add `Services.Router` to `other-modules` in executable.
Add `Services.RouterSpec` to `other-modules` in test-suite.

**Step 5: Run test to verify it passes**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 6: Commit**

```bash
git add wisp-srv/src/Services/Router.hs wisp-srv/test/Services/RouterSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add routing service with receipt logging"
```

---

## Task 11: Update Activity with Classification Fields

**Files:**
- Modify: `wisp-srv/src/Domain/Activity.hs`
- Modify: `wisp-srv/src/Infra/Db/Activity.hs`

**Step 1: Update Activity domain type**

Add classification fields to `wisp-srv/src/Domain/Activity.hs`:

```haskell
-- Update Activity record to include classification fields:
data Activity = Activity
  { activityId :: EntityId
  , activityAccountId :: EntityId
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
  -- Classification fields (populated by classifier):
  , activityPersonas :: Maybe [Text]
  , activityType :: Maybe Text
  , activityUrgency :: Maybe Text
  , activityAutonomyTier :: Maybe Int
  , activityConfidence :: Maybe Double
  , activityPersonId :: Maybe EntityId
  } deriving (Show)
```

**Step 2: Update Activity DB operations**

Update `wisp-srv/src/Infra/Db/Activity.hs`:

```haskell
-- Update FromRow instance:
instance FromRow Activity where
  fromRow = Activity
    <$> (EntityId <$> field)          -- id
    <*> (EntityId <$> field)          -- account_id
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
    <*> (fmap fromPGArray <$> field)  -- personas
    <*> field                          -- activity_type
    <*> field                          -- urgency
    <*> field                          -- autonomy_tier
    <*> field                          -- confidence
    <*> (fmap EntityId <$> field)     -- person_id
    where
      -- ... existing parseSource and parseStatus

-- Add updateActivityClassification function:
updateActivityClassification :: EntityId -> Classification -> Maybe EntityId -> App ()
updateActivityClassification aid classification mPersonId = do
  conn <- getConn
  let typeText = case classificationActivityType classification of
        Request -> "request" :: Text
        Information -> "information"
        ActionRequired -> "action_required"
        FYI -> "fyi"
        Event -> "event"
  let urgencyText = case classificationUrgency classification of
        High -> "high" :: Text
        Normal -> "normal"
        Low -> "low"
  _ <- liftIO $ execute conn
    "update activities set \
    \  personas = ?, activity_type = ?, urgency = ?, \
    \  autonomy_tier = ?, confidence = ?, summary = ?, \
    \  person_id = ?, updated_at = now() \
    \where id = ?"
    ( PGArray (classificationPersonas classification)
    , typeText
    , urgencyText
    , classificationAutonomyTier classification
    , classificationConfidence classification
    , classificationSummary classification
    , fmap unEntityId mPersonId
    , unEntityId aid
    )
  pure ()

-- Update all SELECT queries to include new fields:
-- "select id, account_id, source, source_id, raw, status, title, summary, \
-- \sender_email, starts_at, ends_at, created_at, \
-- \personas, activity_type, urgency, autonomy_tier, confidence, person_id \
-- \from activities ..."
```

Add imports:
```haskell
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Domain.Classification (Classification(..), ActivityType(..), Urgency(..))
```

Add export:
```haskell
  , updateActivityClassification
```

**Step 3: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Domain/Activity.hs wisp-srv/src/Infra/Db/Activity.hs
git commit -m "feat: add classification fields to Activity"
```

---

## Task 12: Classification Pipeline Orchestrator

**Files:**
- Create: `wisp-srv/src/Services/Pipeline.hs`
- Create: `wisp-srv/test/Services/PipelineSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the failing test**

Create `wisp-srv/test/Services/PipelineSpec.hs`:

```haskell
module Services.PipelineSpec where

import Test.Hspec

spec :: Spec
spec = describe "Pipeline" $ do
  it "placeholder - pipeline needs integration tests" $ do
    True `shouldBe` True
```

**Step 2: Write the Pipeline orchestrator**

Create `wisp-srv/src/Services/Pipeline.hs`:

```haskell
module Services.Pipeline
  ( processPendingActivities
  , processActivity
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import App.Monad (App)
import Domain.Activity (Activity(..), ActivityStatus(..))
import Domain.Person (Person(..))
import Infra.Db.Activity (getActivitiesByStatus, updateActivityClassification)
import Services.Classifier (classifyActivity)
import Services.PeopleResolver (resolvePersonForActivity)
import Services.Router (routeActivity)

-- Process all pending activities through the pipeline
processPendingActivities :: Int -> App [(Text, Either Text ActivityStatus)]
processPendingActivities limit = do
  activities <- getActivitiesByStatus Pending limit
  forM activities $ \activity -> do
    result <- processActivity activity
    let actId = T.pack $ show (activityId activity)
    pure (actId, result)

-- Process a single activity through classify -> resolve -> route
processActivity :: Activity -> App (Either Text ActivityStatus)
processActivity activity = do
  -- Step 1: Classify
  liftIO $ putStrLn $ "Classifying activity: " <> show (activityId activity)
  classifyResult <- classifyActivity activity
  case classifyResult of
    Left err -> do
      liftIO $ putStrLn $ "Classification failed: " <> T.unpack err
      pure $ Left err
    Right classification -> do
      liftIO $ putStrLn $ "Classification: tier=" <> show (classificationAutonomyTier classification)
                       <> ", confidence=" <> show (classificationConfidence classification)

      -- Step 2: Resolve person
      mPerson <- resolvePersonForActivity activity
      let mPersonId = fmap personId mPerson
      liftIO $ putStrLn $ "Resolved person: " <> show (fmap personEmail mPerson)

      -- Step 3: Update activity with classification
      updateActivityClassification (activityId activity) classification mPersonId

      -- Step 4: Route
      newStatus <- routeActivity activity classification
      liftIO $ putStrLn $ "Routed to status: " <> show newStatus

      pure $ Right newStatus

-- Need this import
import Domain.Classification (Classification(..))
```

**Step 3: Update wisp-srv.cabal**

Add `Services.Pipeline` to `other-modules` in executable.
Add `Services.PipelineSpec` to `other-modules` in test-suite.

**Step 4: Run test to verify it compiles**

Run: `cabal test wisp-srv-test 2>&1 | tail -10`
Expected: PASS

**Step 5: Commit**

```bash
git add wisp-srv/src/Services/Pipeline.hs wisp-srv/test/Services/PipelineSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add classification pipeline orchestrator"
```

---

## Task 13: Integrate Pipeline into Scheduler

**Files:**
- Modify: `wisp-srv/src/Services/Scheduler.hs`

**Step 1: Update Scheduler to run pipeline after polling**

Update `wisp-srv/src/Services/Scheduler.hs`:

```haskell
-- Add import:
import Services.Pipeline (processPendingActivities)

-- Update runPollCycle to include classification:
runPollCycle :: App ()
runPollCycle = do
  logInfo "Starting poll cycle"

  -- Poll Gmail for all accounts
  gmailResults <- pollAllGmail
  case gmailResults of
    [] -> logInfo "Gmail: no accounts configured"
    _ -> do
      let totalMsgs = sum [n | (_, Right n) <- gmailResults]
      logInfo $ "Gmail: imported " <> T.pack (show totalMsgs) <> " messages total"
      mapM_ logAccountResult $ map (\(e, r) -> ("Gmail", e, r)) gmailResults

  -- Poll Calendar for all accounts
  calResults <- pollAllCalendar
  case calResults of
    [] -> logInfo "Calendar: no accounts configured"
    _ -> do
      let totalEvents = sum [n | (_, Right n) <- calResults]
      logInfo $ "Calendar: imported " <> T.pack (show totalEvents) <> " events total"
      mapM_ logAccountResult $ map (\(e, r) -> ("Calendar", e, r)) calResults

  -- Process pending activities through classification pipeline
  logInfo "Running classification pipeline..."
  pipelineResults <- processPendingActivities 50  -- Process up to 50 at a time
  let successes = length [() | (_, Right _) <- pipelineResults]
  let failures = length [() | (_, Left _) <- pipelineResults]
  logInfo $ "Pipeline: processed " <> T.pack (show successes) <> " activities, "
         <> T.pack (show failures) <> " failures"

  logInfo "Poll cycle complete"
```

**Step 2: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-srv/src/Services/Scheduler.hs
git commit -m "feat: integrate classification pipeline into scheduler"
```

---

## Task 14: Add Manual Classify Endpoint

**Files:**
- Create: `wisp-srv/src/Http/Handlers/Pipeline.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create Pipeline HTTP handler**

Create `wisp-srv/src/Http/Handlers/Pipeline.hs`:

```haskell
module Http.Handlers.Pipeline
  ( postRunPipeline
  , postClassifyActivity
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Network.HTTP.Types.Status (status400, status404, status500)
import Web.Scotty.Trans (ActionT, json, status, captureParam)
import App.Env (Env)
import Domain.Id (EntityId(..))
import Infra.Db.Activity (getActivity)
import Services.Pipeline (processPendingActivities, processActivity)

-- POST /pipeline/run - Run pipeline on pending activities
postRunPipeline :: ActionT (ReaderT Env IO) ()
postRunPipeline = do
  results <- lift $ processPendingActivities 100
  let successes = length [() | (_, Right _) <- results]
  let failures = length [() | (_, Left _) <- results]
  json $ object
    [ "processed" .= successes
    , "failed" .= failures
    , "total" .= length results
    ]

-- POST /activities/:id/classify - Manually classify a single activity
postClassifyActivity :: ActionT (ReaderT Env IO) ()
postClassifyActivity = do
  actId <- captureParam "id"
  mActivity <- lift $ getActivity (EntityId actId)
  case mActivity of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Activity not found" :: Text)]
    Just activity -> do
      result <- lift $ processActivity activity
      case result of
        Left err -> do
          status status500
          json $ object ["error" .= err]
        Right newStatus -> json $ object
          [ "status" .= ("classified" :: Text)
          , "new_status" .= show newStatus
          ]
```

**Step 2: Update Routes**

Add to `wisp-srv/src/Http/Routes.hs`:

```haskell
-- Add import:
import Http.Handlers.Pipeline (postRunPipeline, postClassifyActivity)

-- Add routes in routes function:
  post "/pipeline/run" postRunPipeline
  post "/activities/:id/classify" postClassifyActivity
```

**Step 3: Update wisp-srv.cabal**

Add `Http.Handlers.Pipeline` to `other-modules` in executable.

**Step 4: Run to verify it compiles**

Run: `cabal build wisp-srv 2>&1 | tail -10`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Pipeline.hs wisp-srv/src/Http/Routes.hs wisp-srv/wisp-srv.cabal
git commit -m "feat: add HTTP endpoints for classification pipeline"
```

---

## Task 15: Add CLI classify Command

**Files:**
- Modify: `wisp-cli/app/Main.hs`

**Step 1: Add classify command to CLI**

Update `wisp-cli/app/Main.hs`:

```haskell
-- Add to Command data type:
data Command
  = Auth
  | Status
  | Poll
  | Classify  -- ADD THIS
  deriving (Eq, Show)

-- Add to command parser:
parseCommand :: [String] -> Maybe Command
parseCommand ["auth"] = Just Auth
parseCommand ["status"] = Just Status
parseCommand ["poll"] = Just Poll
parseCommand ["classify"] = Just Classify  -- ADD THIS
parseCommand _ = Nothing

-- Add to main dispatch:
main :: IO ()
main = do
  args <- getArgs
  case parseCommand args of
    Just Auth -> runAuth
    Just Status -> runStatus
    Just Poll -> runPoll
    Just Classify -> runClassify  -- ADD THIS
    Nothing -> printUsage

-- Add the runClassify function:
runClassify :: IO ()
runClassify = do
  manager <- newManager defaultManagerSettings
  TIO.putStrLn "Running classification pipeline..."

  req <- parseRequest $ baseUrl <> "/pipeline/run"
  let postReq = req { method = "POST" }
  response <- httpLbs postReq manager

  case decode (responseBody response) of
    Just (Object obj) -> do
      case (KM.lookup "processed" obj, KM.lookup "failed" obj) of
        (Just (Number processed), Just (Number failed)) -> do
          TIO.putStrLn $ "Processed: " <> showT (round processed :: Int)
          TIO.putStrLn $ "Failed:    " <> showT (round failed :: Int)
        _ -> TIO.putStrLn "Unexpected response format"
    _ -> TIO.putStrLn "Failed to parse response"

-- Update printUsage:
printUsage :: IO ()
printUsage = do
  TIO.putStrLn "Usage: wisp <command>"
  TIO.putStrLn ""
  TIO.putStrLn "Commands:"
  TIO.putStrLn "  auth      - Authenticate with Google"
  TIO.putStrLn "  status    - Show current status"
  TIO.putStrLn "  poll      - Trigger a poll cycle"
  TIO.putStrLn "  classify  - Run classification pipeline"
```

**Step 2: Run to verify it compiles**

Run: `cabal build wisp-cli 2>&1 | tail -10`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add wisp-cli/app/Main.hs
git commit -m "feat: add classify command to CLI"
```

---

## Task 16: Integration Test

**Step 1: Build everything**

Run: `cabal build all 2>&1 | tail -10`
Expected: Build succeeds

**Step 2: Run tests**

Run: `cabal test all 2>&1 | tail -20`
Expected: All tests pass

**Step 3: Manual verification checklist**

1. Run migrations: Apply `008_people.sql` to database
2. Start server: `cabal run wisp-srv`
3. Ensure ANTHROPIC_API_KEY is set in environment
4. Trigger a poll: `wisp poll` (imports some emails/events)
5. Run classification: `wisp classify`
6. Check activities: `curl localhost:8080/activities` - should show classification fields
7. Check that receipts are logged
8. Verify low-confidence items are quarantined

**Step 4: Final commit**

```bash
git add -A
git commit -m "feat: complete classification pipeline implementation"
```

---

## Summary

This plan implements the classification pipeline:

1. **People table** (Tasks 1-3): New table and operations for contact management
2. **Classification types** (Task 4): Domain types for LLM output structure
3. **Receipt logging** (Task 5): Audit trail of all actions
4. **Claude API** (Tasks 6-7): Configuration and HTTP client for Claude
5. **Core services** (Tasks 8-10): Classifier, PeopleResolver, Router
6. **Activity updates** (Task 11): Add classification fields to Activity
7. **Pipeline orchestrator** (Task 12): Ties everything together
8. **Integration** (Tasks 13-15): Scheduler, HTTP endpoints, CLI
9. **Testing** (Task 16): Verification

The pipeline flow:
```
Activity (pending) → Classify (LLM) → Resolve Person → Route → Activity (processed/quarantined/surfaced)
                                                            ↓
                                                       Receipt logged
```
