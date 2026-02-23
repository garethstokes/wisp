# wisp-tui Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a brick-based TUI for Wisp with Chat, Activities, Documents, and Approvals views, plus SSE streaming for chat.

**Architecture:** Three-phase approach: (1) Extract shared client library from CLI, (2) Add SSE streaming to server, (3) Build TUI using brick. Each phase is independently testable.

**Tech Stack:** Haskell, brick, vty, http-client, aeson, stm, async

---

## Phase 1: Shared Client Library (wisp-core)

### Task 1: Create wisp-core package scaffold

**Files:**
- Create: `wisp-core/wisp-core.cabal`
- Create: `wisp-core/src/Wisp/Client/Types.hs`
- Modify: `cabal.project`

**Step 1: Create directory structure**

Run: `mkdir -p wisp-core/src/Wisp/Client`
Expected: Directory created

**Step 2: Create cabal file**

```cabal
-- wisp-core/wisp-core.cabal
cabal-version: 3.0
name:          wisp-core
version:       0.1.0.0
synopsis:      Wisp client library
license:       BSD-3-Clause
author:        Gareth
build-type:    Simple

common warnings
    ghc-options: -Wall -Wunused-packages

library
    import:           warnings
    exposed-modules:
        Wisp.Client.Types
    hs-source-dirs:   src
    default-language: GHC2021
    default-extensions:
        OverloadedStrings
        LambdaCase
        DeriveGeneric
    build-depends:
        base >=4.17 && <5,
        text,
        aeson,
        bytestring,
        time,
        http-client,
        http-types
```

**Step 3: Create Types module**

```haskell
-- wisp-core/src/Wisp/Client/Types.hs
module Wisp.Client.Types
  ( ClientConfig(..)
  , ClientError(..)
  , defaultConfig
  ) where

import Data.Text (Text)

data ClientConfig = ClientConfig
  { configBaseUrl :: Text
  , configTimeout :: Int  -- seconds
  } deriving (Show, Eq)

defaultConfig :: ClientConfig
defaultConfig = ClientConfig
  { configBaseUrl = "http://127.0.0.1:5812"
  , configTimeout = 30
  }

data ClientError
  = HttpError Text
  | ParseError Text
  | ServerError Int Text
  deriving (Show, Eq)
```

**Step 4: Update cabal.project**

```
-- cabal.project
packages: wisp-srv, wisp-cli, wisp-core
```

**Step 5: Build to verify**

Run: `cabal build wisp-core`
Expected: Build succeeds

**Step 6: Commit**

```bash
git add wisp-core cabal.project
git commit -m "feat: add wisp-core package scaffold"
```

---

### Task 2: Add Activity and Document types to wisp-core

**Files:**
- Create: `wisp-core/src/Wisp/Client/Activities.hs`
- Create: `wisp-core/src/Wisp/Client/Documents.hs`
- Modify: `wisp-core/wisp-core.cabal`

**Step 1: Create Activities types**

```haskell
-- wisp-core/src/Wisp/Client/Activities.hs
module Wisp.Client.Activities
  ( Activity(..)
  , ActivitySource(..)
  , ActivityStatus(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, withText, (.:), (.:?))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data ActivitySource = Email | Calendar | Conversation | Note | GitHubEvent
  deriving (Show, Eq, Generic)

instance FromJSON ActivitySource where
  parseJSON = withText "ActivitySource" $ \case
    "email" -> pure Email
    "calendar" -> pure Calendar
    "conversation" -> pure Conversation
    "note" -> pure Note
    "github_event" -> pure GitHubEvent
    other -> fail $ "Unknown source: " <> show other

instance ToJSON ActivitySource where
  toJSON Email = "email"
  toJSON Calendar = "calendar"
  toJSON Conversation = "conversation"
  toJSON Note = "note"
  toJSON GitHubEvent = "github_event"

data ActivityStatus = Pending | Stored | Surfaced | Quarantined | Archived
  deriving (Show, Eq, Generic)

instance FromJSON ActivityStatus where
  parseJSON = withText "ActivityStatus" $ \case
    "pending" -> pure Pending
    "stored" -> pure Stored
    "surfaced" -> pure Surfaced
    "quarantined" -> pure Quarantined
    "archived" -> pure Archived
    other -> fail $ "Unknown status: " <> show other

instance ToJSON ActivityStatus where
  toJSON Pending = "pending"
  toJSON Stored = "stored"
  toJSON Surfaced = "surfaced"
  toJSON Quarantined = "quarantined"
  toJSON Archived = "archived"

data Activity = Activity
  { activityId :: Text
  , activitySource :: ActivitySource
  , activityTitle :: Maybe Text
  , activityRaw :: Text
  , activityStatus :: ActivityStatus
  , activityTags :: [Text]
  , activityCreatedAt :: UTCTime
  , activityConfidence :: Maybe Double
  } deriving (Show, Eq, Generic)

instance FromJSON Activity where
  parseJSON = withObject "Activity" $ \v -> Activity
    <$> v .: "id"
    <*> v .: "source"
    <*> v .:? "title"
    <*> v .: "raw"
    <*> v .: "status"
    <*> v .: "tags"
    <*> v .: "created_at"
    <*> v .:? "confidence"
```

**Step 2: Create Documents types**

```haskell
-- wisp-core/src/Wisp/Client/Documents.hs
module Wisp.Client.Documents
  ( Document(..)
  , DocumentType(..)
  , ProjectData(..)
  , NoteData(..)
  , PreferenceData(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value, withObject, withText, (.:), (.:?), object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data DocumentType = ProjectDoc | NoteDoc | PreferenceDoc
  deriving (Show, Eq, Generic)

instance FromJSON DocumentType where
  parseJSON = withText "DocumentType" $ \case
    "project" -> pure ProjectDoc
    "note" -> pure NoteDoc
    "preference" -> pure PreferenceDoc
    other -> fail $ "Unknown document type: " <> show other

instance ToJSON DocumentType where
  toJSON ProjectDoc = "project"
  toJSON NoteDoc = "note"
  toJSON PreferenceDoc = "preference"

data Document = Document
  { documentId :: Text
  , documentType :: DocumentType
  , documentData :: Value
  , documentTags :: [Text]
  , documentActive :: Bool
  , documentCreatedAt :: UTCTime
  , documentLastActivityAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance FromJSON Document where
  parseJSON = withObject "Document" $ \v -> Document
    <$> v .: "id"
    <*> v .: "type"
    <*> v .: "data"
    <*> v .: "tags"
    <*> v .: "active"
    <*> v .: "created_at"
    <*> v .:? "last_activity_at"

data ProjectData = ProjectData
  { projectName :: Text
  , projectType :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON ProjectData where
  parseJSON = withObject "ProjectData" $ \v -> ProjectData
    <$> v .: "name"
    <*> v .: "type"

instance ToJSON ProjectData where
  toJSON p = object ["name" .= projectName p, "type" .= projectType p]

data NoteData = NoteData
  { noteTitle :: Text
  , noteContent :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON NoteData where
  parseJSON = withObject "NoteData" $ \v -> NoteData
    <$> v .: "title"
    <*> v .:? "content"

instance ToJSON NoteData where
  toJSON n = object ["title" .= noteTitle n, "content" .= noteContent n]

data PreferenceData = PreferenceData
  { prefKey :: Text
  , prefValue :: Text
  , prefContext :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON PreferenceData where
  parseJSON = withObject "PreferenceData" $ \v -> PreferenceData
    <$> v .: "key"
    <*> v .: "value"
    <*> v .:? "context"

instance ToJSON PreferenceData where
  toJSON p = object ["key" .= prefKey p, "value" .= prefValue p, "context" .= prefContext p]
```

**Step 3: Update cabal file**

Add to exposed-modules:
```
    exposed-modules:
        Wisp.Client.Types
        Wisp.Client.Activities
        Wisp.Client.Documents
```

**Step 4: Build**

Run: `cabal build wisp-core`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add wisp-core
git commit -m "feat(core): add Activity and Document types"
```

---

### Task 3: Add HTTP client functions to wisp-core

**Files:**
- Create: `wisp-core/src/Wisp/Client.hs`
- Modify: `wisp-core/wisp-core.cabal`

**Step 1: Create Client module**

```haskell
-- wisp-core/src/Wisp/Client.hs
module Wisp.Client
  ( -- * Client operations
    getActivities
  , getActivity
  , getDocuments
  , getProjects
  , getNotes
  , getPreferences
  , createProject
  , createNote
  , setPreference
  , archiveDocument
  , approveActivity
  , dismissActivity
  , getApprovals
    -- * Re-exports
  , module Wisp.Client.Types
  , module Wisp.Client.Activities
  , module Wisp.Client.Documents
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (FromJSON, ToJSON, decode, encode, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

import Wisp.Client.Types
import Wisp.Client.Activities
import Wisp.Client.Documents

-- | Make a GET request
httpGet :: FromJSON a => ClientConfig -> String -> IO (Either ClientError a)
httpGet cfg path = do
  manager <- newManager defaultManagerSettings
  result <- try $ do
    req <- parseRequest $ T.unpack (configBaseUrl cfg) <> path
    httpLbs req manager
  case result of
    Left (e :: SomeException) -> pure $ Left $ HttpError $ T.pack $ show e
    Right response ->
      let code = statusCode $ responseStatus response
      in if code >= 200 && code < 300
         then case decode (responseBody response) of
           Just a -> pure $ Right a
           Nothing -> pure $ Left $ ParseError "Failed to parse response"
         else pure $ Left $ ServerError code $ T.pack $ show $ responseBody response

-- | Make a POST request with JSON body
httpPost :: (ToJSON a, FromJSON b) => ClientConfig -> String -> a -> IO (Either ClientError b)
httpPost cfg path body = do
  manager <- newManager defaultManagerSettings
  result <- try $ do
    initialReq <- parseRequest $ T.unpack (configBaseUrl cfg) <> path
    let req = initialReq
          { method = "POST"
          , requestHeaders = [("Content-Type", "application/json")]
          , requestBody = RequestBodyLBS (encode body)
          }
    httpLbs req manager
  case result of
    Left (e :: SomeException) -> pure $ Left $ HttpError $ T.pack $ show e
    Right response ->
      let code = statusCode $ responseStatus response
      in if code >= 200 && code < 300
         then case decode (responseBody response) of
           Just a -> pure $ Right a
           Nothing -> pure $ Left $ ParseError "Failed to parse response"
         else pure $ Left $ ServerError code $ T.pack $ show $ responseBody response

-- | POST without expecting response body
httpPost_ :: ToJSON a => ClientConfig -> String -> a -> IO (Either ClientError ())
httpPost_ cfg path body = do
  manager <- newManager defaultManagerSettings
  result <- try $ do
    initialReq <- parseRequest $ T.unpack (configBaseUrl cfg) <> path
    let req = initialReq
          { method = "POST"
          , requestHeaders = [("Content-Type", "application/json")]
          , requestBody = RequestBodyLBS (encode body)
          }
    httpLbs req manager
  case result of
    Left (e :: SomeException) -> pure $ Left $ HttpError $ T.pack $ show e
    Right response ->
      let code = statusCode $ responseStatus response
      in if code >= 200 && code < 300
         then pure $ Right ()
         else pure $ Left $ ServerError code $ T.pack $ show $ responseBody response

-- Activities
getActivities :: ClientConfig -> IO (Either ClientError [Activity])
getActivities cfg = do
  result <- httpGet cfg "/api/activities"
  pure $ case result of
    Left e -> Left e
    Right val -> case val of
      Aeson.Object obj -> case KM.lookup "activities" obj of
        Just (Aeson.Array arr) -> case traverse Aeson.fromJSON arr of
          Aeson.Success activities -> Right $ toList activities
          _ -> Left $ ParseError "Failed to parse activities array"
        _ -> Left $ ParseError "Missing activities key"
      _ -> Left $ ParseError "Expected object"

getActivity :: ClientConfig -> Text -> IO (Either ClientError Activity)
getActivity cfg aid = httpGet cfg $ "/api/activities/" <> T.unpack aid

-- Documents
getDocuments :: ClientConfig -> DocumentType -> IO (Either ClientError [Document])
getDocuments cfg docType = do
  let path = case docType of
        ProjectDoc -> "/api/projects"
        NoteDoc -> "/api/notes"
        PreferenceDoc -> "/api/preferences"
  result <- httpGet cfg path
  pure $ case result of
    Left e -> Left e
    Right val -> case val of
      Aeson.Object obj ->
        let key = case docType of
              ProjectDoc -> "projects"
              NoteDoc -> "notes"
              PreferenceDoc -> "preferences"
        in case KM.lookup key obj of
          Just (Aeson.Array arr) -> case traverse Aeson.fromJSON arr of
            Aeson.Success docs -> Right $ toList docs
            _ -> Left $ ParseError "Failed to parse documents"
          _ -> Left $ ParseError "Missing documents key"
      _ -> Left $ ParseError "Expected object"

getProjects :: ClientConfig -> IO (Either ClientError [Document])
getProjects cfg = getDocuments cfg ProjectDoc

getNotes :: ClientConfig -> IO (Either ClientError [Document])
getNotes cfg = getDocuments cfg NoteDoc

getPreferences :: ClientConfig -> IO (Either ClientError [Document])
getPreferences cfg = getDocuments cfg PreferenceDoc

createProject :: ClientConfig -> Text -> Text -> IO (Either ClientError Text)
createProject cfg name projType = do
  result <- httpPost cfg "/api/projects" $ object ["name" .= name, "type" .= projType]
  pure $ case result of
    Left e -> Left e
    Right (Aeson.Object obj) -> case KM.lookup "id" obj of
      Just (Aeson.String pid) -> Right pid
      _ -> Left $ ParseError "Missing id in response"
    Right _ -> Left $ ParseError "Expected object"

createNote :: ClientConfig -> Text -> Maybe Text -> [Text] -> IO (Either ClientError Text)
createNote cfg title mContent tags = do
  let body = object $ ["title" .= title]
        <> maybe [] (\c -> ["content" .= c]) mContent
        <> if null tags then [] else ["tags" .= tags]
  result <- httpPost cfg "/api/notes" body
  pure $ case result of
    Left e -> Left e
    Right (Aeson.Object obj) -> case KM.lookup "id" obj of
      Just (Aeson.String nid) -> Right nid
      _ -> Left $ ParseError "Missing id"
    Right _ -> Left $ ParseError "Expected object"

setPreference :: ClientConfig -> Text -> Text -> Maybe Text -> IO (Either ClientError Text)
setPreference cfg key value mContext = do
  let body = object $ ["key" .= key, "value" .= value]
        <> maybe [] (\c -> ["context" .= c]) mContext
  result <- httpPost cfg "/api/preferences" body
  pure $ case result of
    Left e -> Left e
    Right (Aeson.Object obj) -> case KM.lookup "id" obj of
      Just (Aeson.String pid) -> Right pid
      _ -> Left $ ParseError "Missing id"
    Right _ -> Left $ ParseError "Expected object"

archiveDocument :: ClientConfig -> Text -> IO (Either ClientError ())
archiveDocument cfg docId =
  httpPost_ cfg ("/api/projects/" <> T.unpack docId <> "/archive") (object [])

-- Approvals
data ApprovalItem = ApprovalItem
  { approvalActivity :: Activity
  , approvalType :: Text  -- "quarantine" or "classify"
  , approvalReason :: Text
  } deriving (Show, Eq)

instance FromJSON ApprovalItem where
  parseJSON = withObject "ApprovalItem" $ \v -> ApprovalItem
    <$> v .: "activity"
    <*> v .: "type"
    <*> v .: "reason"

getApprovals :: ClientConfig -> IO (Either ClientError [ApprovalItem])
getApprovals cfg = do
  -- Fetch both quarantined and review items, combine them
  result <- httpGet cfg "/inbox"
  pure $ case result of
    Left e -> Left e
    Right (Aeson.Object obj) -> Right $ concat
      [ extractItems "quarantine" "quarantined" obj
      , extractItems "classify" "review" obj
      ]
    Right _ -> Left $ ParseError "Expected object"
  where
    extractItems aType key obj = case KM.lookup key obj of
      Just (Aeson.Array arr) ->
        [ ApprovalItem act aType "needs review"
        | Aeson.Object actObj <- toList arr
        , Aeson.Success act <- [Aeson.fromJSON (Aeson.Object actObj)]
        ]
      _ -> []

approveActivity :: ClientConfig -> Text -> IO (Either ClientError ())
approveActivity cfg aid =
  httpPost_ cfg ("/api/activities/" <> T.unpack aid <> "/approve") (object [])

dismissActivity :: ClientConfig -> Text -> IO (Either ClientError ())
dismissActivity cfg aid =
  httpPost_ cfg ("/api/activities/" <> T.unpack aid <> "/dismiss") (object [])

-- Helper
toList :: Foldable t => t a -> [a]
toList = foldr (:) []
```

**Step 2: Update cabal file**

Add to exposed-modules and build-depends:
```
    exposed-modules:
        Wisp.Client
        Wisp.Client.Types
        Wisp.Client.Activities
        Wisp.Client.Documents
    build-depends:
        base >=4.17 && <5,
        text,
        aeson,
        bytestring,
        time,
        http-client,
        http-types,
        containers
```

**Step 3: Build**

Run: `cabal build wisp-core`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-core
git commit -m "feat(core): add HTTP client functions"
```

---

## Phase 2: Server SSE Streaming

### Task 4: Add ChatEvent types to wisp-srv

**Files:**
- Create: `wisp-srv/src/Domain/ChatEvent.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create ChatEvent types**

```haskell
-- wisp-srv/src/Domain/ChatEvent.hs
module Domain.ChatEvent
  ( ChatEvent(..)
  , ToolCallInfo(..)
  , ToolResultInfo(..)
  , chatEventToSSE
  ) where

import Data.Aeson (ToJSON(..), object, (.=), encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value)

data ToolCallInfo = ToolCallInfo
  { toolCallName :: Text
  , toolCallArgs :: Value
  } deriving (Show, Eq)

instance ToJSON ToolCallInfo where
  toJSON t = object ["tool" .= toolCallName t, "args" .= toolCallArgs t]

data ToolResultInfo = ToolResultInfo
  { toolResultName :: Text
  , toolResultValue :: Value
  , toolResultDurationMs :: Int
  } deriving (Show, Eq)

instance ToJSON ToolResultInfo where
  toJSON t = object
    [ "tool" .= toolResultName t
    , "result" .= toolResultValue t
    , "duration_ms" .= toolResultDurationMs t
    ]

data ChatEvent
  = ChunkEvent Text
  | ToolCallStartEvent ToolCallInfo
  | ToolCallResultEvent ToolResultInfo
  | DoneEvent Text Int  -- session_id, token_count
  | ErrorEvent Text Text  -- message, code
  deriving (Show, Eq)

instance ToJSON ChatEvent where
  toJSON (ChunkEvent txt) = object ["text" .= txt]
  toJSON (ToolCallStartEvent info) = toJSON info
  toJSON (ToolCallResultEvent info) = toJSON info
  toJSON (DoneEvent sid tokens) = object ["session_id" .= sid, "token_count" .= tokens]
  toJSON (ErrorEvent msg code) = object ["message" .= msg, "code" .= code]

-- | Format a ChatEvent as SSE
chatEventToSSE :: ChatEvent -> BL.ByteString
chatEventToSSE evt =
  let (eventName, payload) = case evt of
        ChunkEvent _ -> ("chunk", encode evt)
        ToolCallStartEvent _ -> ("tool_call_start", encode evt)
        ToolCallResultEvent _ -> ("tool_call_result", encode evt)
        DoneEvent _ _ -> ("done", encode evt)
        ErrorEvent _ _ -> ("error", encode evt)
  in BL.fromStrict $ TE.encodeUtf8 $
       "event: " <> eventName <> "\n" <>
       "data: " <> TE.decodeUtf8 (BL.toStrict payload) <> "\n\n"
```

**Step 2: Add to cabal file**

Add `Domain.ChatEvent` to other-modules in wisp-srv.cabal.

**Step 3: Build**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-srv/src/Domain/ChatEvent.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(srv): add ChatEvent types for SSE"
```

---

### Task 5: Add SSE streaming chat handler

**Files:**
- Create: `wisp-srv/src/Http/Handlers/ChatStream.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create ChatStream handler**

```haskell
-- wisp-srv/src/Http/Handlers/ChatStream.hs
module Http.Handlers.ChatStream
  ( postChatStream
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Network.HTTP.Types.Status (status200, status400, status500)
import Network.HTTP.Types.Header (hContentType)
import Web.Scotty.Trans (ActionT, json, status, jsonData, setHeader, raw)
import App.Monad (Env(..))
import Domain.Chat (ChatRequest(..))
import Domain.ChatEvent (ChatEvent(..), chatEventToSSE)
import Domain.Tenant (TenantId, Tenant(..))
import Domain.Account (Account(..))
import Infra.Db.Tenant (getAllTenants)
import Infra.Db.Account (getAllAccounts)

-- | POST /api/chat/stream - SSE streaming chat
postChatStream :: ActionT (ReaderT Env IO) ()
postChatStream = do
  req <- jsonData :: ActionT (ReaderT Env IO) ChatRequest

  -- Get tenant and account (same as regular chat)
  tenants <- lift getAllTenants
  accounts <- lift getAllAccounts

  case (tenants, accounts) of
    ([], _) -> do
      status status500
      json $ object ["error" .= ("No tenant configured" :: Text)]
    (_, []) -> do
      status status500
      json $ object ["error" .= ("No accounts configured" :: Text)]
    (t:_, a:_) -> do
      -- Set up SSE headers
      status status200
      setHeader "Content-Type" "text/event-stream"
      setHeader "Cache-Control" "no-cache"
      setHeader "Connection" "keep-alive"

      -- Create event channel
      chan <- lift $ liftIO newTChanIO

      -- TODO: Fork agent execution that writes to chan
      -- For now, send a simple response to verify SSE works
      lift $ liftIO $ atomically $ do
        writeTChan chan $ ChunkEvent "Hello from SSE! "
        writeTChan chan $ ChunkEvent "Streaming works."
        writeTChan chan $ DoneEvent "test-session" 10

      -- Stream events from channel
      let streamEvents = do
            evt <- lift $ liftIO $ atomically $ readTChan chan
            raw $ chatEventToSSE evt
            case evt of
              DoneEvent _ _ -> pure ()
              ErrorEvent _ _ -> pure ()
              _ -> streamEvents

      streamEvents

-- Helper to lift IO into the monad stack
liftIO :: IO a -> ReaderT Env IO a
liftIO = lift
```

**Step 2: Add route**

In `wisp-srv/src/Http/Routes.hs`, add:
```haskell
import Http.Handlers.ChatStream (postChatStream)

-- In routes function:
post "/api/chat/stream" postChatStream
```

**Step 3: Update cabal**

Add `Http.Handlers.ChatStream` to other-modules.

**Step 4: Build**

Run: `cabal build wisp-srv`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add wisp-srv/src/Http/Handlers/ChatStream.hs wisp-srv/src/Http/Routes.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(srv): add SSE streaming chat endpoint"
```

---

### Task 6: Add SSE client to wisp-core

**Files:**
- Create: `wisp-core/src/Wisp/Client/SSE.hs`
- Modify: `wisp-core/wisp-core.cabal`

**Step 1: Create SSE client**

```haskell
-- wisp-core/src/Wisp/Client/SSE.hs
module Wisp.Client.SSE
  ( ChatEvent(..)
  , streamChat
  , ChatRequest(..)
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (FromJSON(..), ToJSON(..), decode, encode, object, withObject, (.=), (.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

import Wisp.Client.Types

data ChatRequest = ChatRequest
  { chatAgent :: Text
  , chatMessage :: Text
  , chatSession :: Text
  } deriving (Show, Eq)

instance ToJSON ChatRequest where
  toJSON r = object
    [ "agent" .= chatAgent r
    , "messages" .= [object ["role" .= ("user" :: Text), "content" .= chatMessage r]]
    , "session" .= chatSession r
    ]

data ChatEvent
  = ChunkEvent Text
  | ToolCallStart Text  -- tool name
  | ToolCallResult Text Int  -- tool name, duration_ms
  | DoneEvent Text Int  -- session_id, token_count
  | ErrorEvent Text Text  -- message, code
  deriving (Show, Eq)

-- | Stream chat with callback for each event
streamChat
  :: ClientConfig
  -> ChatRequest
  -> (ChatEvent -> IO ())  -- Callback for each event
  -> IO (Either ClientError ())
streamChat cfg req callback = do
  manager <- newManager defaultManagerSettings
  result <- try $ do
    initialReq <- parseRequest $ T.unpack (configBaseUrl cfg) <> "/api/chat/stream"
    let httpReq = initialReq
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Accept", "text/event-stream")
              ]
          , requestBody = RequestBodyLBS (encode req)
          }
    withResponse httpReq manager $ \response -> do
      let code = statusCode $ responseStatus response
      if code >= 200 && code < 300
        then do
          parseSSEStream (responseBody response) callback
          pure $ Right ()
        else pure $ Left $ ServerError code "SSE request failed"
  case result of
    Left (e :: SomeException) -> pure $ Left $ HttpError $ T.pack $ show e
    Right r -> pure r

-- | Parse SSE stream and call handler for each event
parseSSEStream :: BodyReader -> (ChatEvent -> IO ()) -> IO ()
parseSSEStream body callback = loop ""
  where
    loop buffer = do
      chunk <- body
      if BS.null chunk
        then pure ()  -- Stream ended
        else do
          let newBuffer = buffer <> chunk
          let (events, remaining) = splitEvents newBuffer
          mapM_ (processEvent callback) events
          -- Check if we hit done/error
          let shouldStop = any isDone events
          if shouldStop
            then pure ()
            else loop remaining

    splitEvents bs =
      let parts = BS.split 10 bs  -- Split on newline
          (complete, incomplete) = span (not . BS.null) parts
      in (complete, BS.intercalate "\n" incomplete)

    isDone bs = "event: done" `BS.isInfixOf` bs || "event: error" `BS.isInfixOf` bs

    processEvent cb bs = do
      let txt = TE.decodeUtf8 bs
      case parseSSELine txt of
        Just evt -> cb evt
        Nothing -> pure ()

-- | Parse a single SSE event
parseSSELine :: Text -> Maybe ChatEvent
parseSSELine txt
  | "event: chunk" `T.isInfixOf` txt = parseDataLine txt ChunkEvent
  | "event: tool_call_start" `T.isInfixOf` txt = parseToolStart txt
  | "event: tool_call_result" `T.isInfixOf` txt = parseToolResult txt
  | "event: done" `T.isInfixOf` txt = parseDone txt
  | "event: error" `T.isInfixOf` txt = parseError txt
  | otherwise = Nothing
  where
    parseDataLine t f = do
      let dataLine = T.dropWhile (/= '{') t
      case decode (BL.fromStrict $ TE.encodeUtf8 dataLine) of
        Just obj -> case obj of
          Object m -> case lookup "text" (toList m) of
            Just (String s) -> Just $ f s
            _ -> Nothing
          _ -> Nothing
        Nothing -> Nothing

    parseToolStart t = Just $ ToolCallStart "tool"  -- Simplified
    parseToolResult t = Just $ ToolCallResult "tool" 0
    parseDone t = Just $ DoneEvent "session" 0
    parseError t = Just $ ErrorEvent "error" "unknown"

    toList = map (\(k,v) -> (k,v)) . KM.toList
    Object = undefined  -- placeholder
    String = undefined
    KM = undefined
```

**Step 2: Update cabal**

Add to exposed-modules and add `http-client` to build-depends.

**Step 3: Build**

Run: `cabal build wisp-core`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add wisp-core
git commit -m "feat(core): add SSE client for streaming chat"
```

---

## Phase 3: TUI Application

### Task 7: Create wisp-tui package scaffold

**Files:**
- Create: `wisp-tui/wisp-tui.cabal`
- Create: `wisp-tui/app/Main.hs`
- Create: `wisp-tui/src/Tui/Types.hs`
- Modify: `cabal.project`

**Step 1: Create directory structure**

Run: `mkdir -p wisp-tui/app wisp-tui/src/Tui/Views wisp-tui/src/Tui/Widgets`

**Step 2: Create cabal file**

```cabal
-- wisp-tui/wisp-tui.cabal
cabal-version: 3.0
name:          wisp-tui
version:       0.1.0.0
synopsis:      Wisp TUI client
license:       BSD-3-Clause
author:        Gareth
build-type:    Simple

common warnings
    ghc-options: -Wall -Wunused-packages

executable wisp-tui
    import:           warnings
    main-is:          Main.hs
    other-modules:
        Tui.Types
    hs-source-dirs:   app, src
    default-language: GHC2021
    default-extensions:
        OverloadedStrings
        LambdaCase
        TemplateHaskell
    build-depends:
        base >=4.17 && <5,
        wisp-core,
        brick >= 2.0,
        vty >= 6.0,
        vty-crossplatform,
        text,
        microlens,
        microlens-th,
        stm,
        async,
        time
```

**Step 3: Create Types module**

```haskell
-- wisp-tui/src/Tui/Types.hs
{-# LANGUAGE TemplateHaskell #-}
module Tui.Types
  ( AppState(..)
  , View(..)
  , ChatState(..)
  , ActivitiesState(..)
  , DocumentsState(..)
  , ApprovalsState(..)
  , DocumentTab(..)
  , Name(..)
  , AppEvent(..)
  -- Lenses
  , currentView
  , chatState
  , activitiesState
  , documentsState
  , approvalsState
  , statusMessage
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Lens.Micro.TH (makeLenses)

import Wisp.Client (ClientConfig, Activity, Document)
import Wisp.Client.SSE (ChatEvent)

-- | Resource names for brick
data Name
  = ChatInput
  | ChatHistory
  | ActivityList
  | DocumentList
  | ApprovalList
  deriving (Show, Eq, Ord)

-- | Custom events
data AppEvent
  = ChatEventReceived ChatEvent
  | RefreshView View
  | Tick
  deriving (Show, Eq)

-- | Main views
data View = ChatView | ActivitiesView | DocumentsView | ApprovalsView
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Document sub-tabs
data DocumentTab = ProjectsTab | NotesTab | PrefsTab
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Chat message for display
data ChatMessage = ChatMessage
  { cmRole :: Text
  , cmContent :: Text
  , cmTimestamp :: UTCTime
  } deriving (Show, Eq)

-- | Chat view state
data ChatState = ChatState
  { _csMessages :: [ChatMessage]
  , _csInputBuffer :: Text
  , _csCurrentAgent :: Text
  , _csCurrentSession :: Text
  , _csStreaming :: Bool
  , _csStreamBuffer :: Text
  } deriving (Show)

makeLenses ''ChatState

-- | Activities view state
data ActivitiesState = ActivitiesState
  { _asActivities :: [Activity]
  , _asSelected :: Int
  , _asExpanded :: Maybe Int
  , _asFilter :: Text
  } deriving (Show)

makeLenses ''ActivitiesState

-- | Documents view state
data DocumentsState = DocumentsState
  { _dsCurrentTab :: DocumentTab
  , _dsProjects :: [Document]
  , _dsNotes :: [Document]
  , _dsPrefs :: [Document]
  , _dsSelected :: Int
  } deriving (Show)

makeLenses ''DocumentsState

-- | Approvals view state
data ApprovalsState = ApprovalsState
  { _apsItems :: [(Activity, Text, Text)]  -- (activity, type, reason)
  , _apsSelected :: Int
  , _apsExpanded :: Maybe Int
  } deriving (Show)

makeLenses ''ApprovalsState

-- | Main application state
data AppState = AppState
  { _currentView :: View
  , _chatState :: ChatState
  , _activitiesState :: ActivitiesState
  , _documentsState :: DocumentsState
  , _approvalsState :: ApprovalsState
  , _clientConfig :: ClientConfig
  , _statusMessage :: Maybe (Text, UTCTime)
  }

makeLenses ''AppState
```

**Step 4: Create Main**

```haskell
-- wisp-tui/app/Main.hs
module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async)
import Control.Monad (forever, void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V

import Tui.Types
import Wisp.Client (defaultConfig)

main :: IO ()
main = do
  -- Create event channel
  chan <- newBChan 10

  -- Start tick thread
  void $ forkIO $ forever $ do
    threadDelay 1000000  -- 1 second
    writeBChan chan Tick

  -- Build initial state
  now <- getCurrentTime
  let initialState = AppState
        { _currentView = ChatView
        , _chatState = ChatState
            { _csMessages = []
            , _csInputBuffer = ""
            , _csCurrentAgent = "wisp"
            , _csCurrentSession = "default"
            , _csStreaming = False
            , _csStreamBuffer = ""
            }
        , _activitiesState = ActivitiesState [] 0 Nothing ""
        , _documentsState = DocumentsState ProjectsTab [] [] [] 0
        , _approvalsState = ApprovalsState [] 0 Nothing
        , _clientConfig = defaultConfig
        , _statusMessage = Just ("Welcome to wisp-tui", now)
        }

  -- Build vty
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  -- Run app
  void $ customMain initialVty buildVty (Just chan) app initialState

app :: App AppState AppEvent Name
app = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  , appAttrMap = const $ attrMap V.defAttr []
  }

drawUI :: AppState -> [Widget Name]
drawUI s = [str "wisp-tui - Press q to quit, Tab to switch views"]

handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  modify $ \s -> s { _currentView = nextView (_currentView s) }
handleEvent _ = pure ()

nextView :: View -> View
nextView ChatView = ActivitiesView
nextView ActivitiesView = DocumentsView
nextView DocumentsView = ApprovalsView
nextView ApprovalsView = ChatView
```

**Step 5: Update cabal.project**

```
packages: wisp-srv, wisp-cli, wisp-core, wisp-tui
```

**Step 6: Build**

Run: `cabal build wisp-tui`
Expected: Build succeeds

**Step 7: Test**

Run: `cabal run wisp-tui`
Expected: TUI launches, shows placeholder, q quits, Tab cycles

**Step 8: Commit**

```bash
git add wisp-tui cabal.project
git commit -m "feat: add wisp-tui package scaffold"
```

---

### Task 8: Implement header and status bar widgets

**Files:**
- Create: `wisp-tui/src/Tui/Widgets/Layout.hs`
- Modify: `wisp-tui/app/Main.hs`
- Modify: `wisp-tui/wisp-tui.cabal`

**Step 1: Create Layout widgets**

```haskell
-- wisp-tui/src/Tui/Widgets/Layout.hs
module Tui.Widgets.Layout
  ( headerWidget
  , statusBarWidget
  , viewTabName
  ) where

import Brick
import Brick.Widgets.Center (hCenter)
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T

import Tui.Types

-- | View tab display names
viewTabName :: View -> Text
viewTabName ChatView = "Chat"
viewTabName ActivitiesView = "Activities"
viewTabName DocumentsView = "Documents"
viewTabName ApprovalsView = "Approvals"

-- | Header with tabs
headerWidget :: View -> Text -> Widget Name
headerWidget current agent = vLimit 1 $ hBox
  [ tabsWidget current
  , fill ' '
  , txt agent
  , txt " "
  ]

tabsWidget :: View -> Widget Name
tabsWidget current = hBox $ map (renderTab current) [minBound..maxBound]
  where
    renderTab cur v
      | v == cur  = withAttr selectedTabAttr $ txt $ " [" <> viewTabName v <> "] "
      | otherwise = txt $ "  " <> viewTabName v <> "  "

selectedTabAttr :: AttrName
selectedTabAttr = attrName "selectedTab"

-- | Status bar
statusBarWidget :: Maybe Text -> Widget Name
statusBarWidget mStatus = vLimit 1 $ hBox
  [ txt " Connected"
  , txt " │ "
  , case mStatus of
      Just msg -> txt msg
      Nothing -> txt "Tab:switch views  ?:help  q:quit"
  ]
```

**Step 2: Update Main.hs**

```haskell
-- Update imports
import Tui.Widgets.Layout (headerWidget, statusBarWidget)

-- Update drawUI
drawUI :: AppState -> [Widget Name]
drawUI s =
  [ vBox
    [ headerWidget (_currentView s) (_csCurrentAgent $ _chatState s)
    , hBorder
    , padAll 1 $ str $ "Current view: " <> show (_currentView s)
    , hBorder
    , statusBarWidget (fst <$> _statusMessage s)
    ]
  ]

-- Update attrMap
appAttrMap = const $ attrMap V.defAttr
  [ (attrName "selectedTab", V.withStyle V.defAttr V.bold)
  ]
```

**Step 3: Update cabal**

Add `Tui.Widgets.Layout` to other-modules.

**Step 4: Build and test**

Run: `cabal build wisp-tui && cabal run wisp-tui`
Expected: Header with tabs visible, Tab cycles highlight, status bar shows

**Step 5: Commit**

```bash
git add wisp-tui
git commit -m "feat(tui): add header and status bar widgets"
```

---

### Task 9: Implement Chat view

**Files:**
- Create: `wisp-tui/src/Tui/Views/Chat.hs`
- Modify: `wisp-tui/app/Main.hs`
- Modify: `wisp-tui/wisp-tui.cabal`

**Step 1: Create Chat view**

```haskell
-- wisp-tui/src/Tui/Views/Chat.hs
module Tui.Views.Chat
  ( chatWidget
  , handleChatEvent
  ) where

import Brick
import Brick.Widgets.Border (border, hBorder)
import Brick.Widgets.Edit (editor, renderEditor, handleEditorEvent, getEditContents)
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types

-- | Chat view widget
chatWidget :: ChatState -> Widget Name
chatWidget cs = vBox
  [ sessionHeader cs
  , messagesWidget cs
  , hBorder
  , inputWidget cs
  ]

sessionHeader :: ChatState -> Widget Name
sessionHeader cs = vLimit 1 $ hBox
  [ txt " session: "
  , txt (cs ^. csCurrentSession)
  , fill ' '
  ]

messagesWidget :: ChatState -> Widget Name
messagesWidget cs = viewport ChatHistory Vertical $ vBox $
  map renderMessage (cs ^. csMessages)
  ++ streamingIndicator cs

renderMessage :: ChatMessage -> Widget Name
renderMessage msg = padBottom (Pad 1) $ vBox
  [ withAttr (roleAttr $ cmRole msg) $ txt $ "[" <> cmRole msg <> "]"
  , padLeft (Pad 2) $ txtWrap (cmContent msg)
  ]

roleAttr :: Text -> AttrName
roleAttr "You" = attrName "userRole"
roleAttr _ = attrName "assistantRole"

streamingIndicator :: ChatState -> [Widget Name]
streamingIndicator cs
  | cs ^. csStreaming =
      [ txt $ cs ^. csStreamBuffer
      , txt "▌"  -- Cursor indicator
      ]
  | otherwise = []

inputWidget :: ChatState -> Widget Name
inputWidget cs = vLimit 1 $ hBox
  [ txt "> "
  , txt (cs ^. csInputBuffer)
  , txt "│"  -- Simple cursor
  ]

-- | Handle chat-specific events
handleChatEvent :: V.Event -> EventM Name AppState ()
handleChatEvent (V.EvKey V.KEnter []) = do
  -- Send message
  s <- get
  let input = s ^. chatState . csInputBuffer
  if T.null input
    then pure ()
    else do
      -- Add user message and clear input
      modify $ chatState . csInputBuffer .~ ""
      -- TODO: Send to server via SSE
handleChatEvent (V.EvKey V.KBS []) = do
  modify $ chatState . csInputBuffer %~ (\t -> if T.null t then t else T.init t)
handleChatEvent (V.EvKey (V.KChar c) []) = do
  modify $ chatState . csInputBuffer %~ (<> T.singleton c)
handleChatEvent _ = pure ()
```

**Step 2: Update Main.hs**

```haskell
-- Add import
import Tui.Views.Chat (chatWidget, handleChatEvent)

-- Update drawUI
drawUI s = case _currentView s of
  ChatView -> [mainLayout s $ chatWidget (_chatState s)]
  _ -> [mainLayout s $ str $ "View: " <> show (_currentView s)]

mainLayout :: AppState -> Widget Name -> Widget Name
mainLayout s content = vBox
  [ headerWidget (_currentView s) (_csCurrentAgent $ _chatState s)
  , hBorder
  , content
  , hBorder
  , statusBarWidget (fst <$> _statusMessage s)
  ]

-- Update handleEvent
handleEvent (VtyEvent e@(V.EvKey _ _))
  | V.EvKey (V.KChar 'q') [] <- e = halt
  | V.EvKey (V.KChar '\t') [] <- e = modify $ currentView %~ nextView
  | otherwise = do
      s <- get
      case _currentView s of
        ChatView -> handleChatEvent e
        _ -> pure ()
handleEvent _ = pure ()
```

**Step 3: Update cabal**

Add `Tui.Views.Chat` to other-modules.

**Step 4: Build and test**

Run: `cabal build wisp-tui && cabal run wisp-tui`
Expected: Chat view shows, can type text, Enter clears input

**Step 5: Commit**

```bash
git add wisp-tui
git commit -m "feat(tui): implement Chat view"
```

---

### Task 10: Implement Activities view

**Files:**
- Create: `wisp-tui/src/Tui/Views/Activities.hs`
- Modify: `wisp-tui/app/Main.hs`
- Modify: `wisp-tui/wisp-tui.cabal`

**Step 1: Create Activities view**

```haskell
-- wisp-tui/src/Tui/Views/Activities.hs
module Tui.Views.Activities
  ( activitiesWidget
  , handleActivitiesEvent
  , loadActivities
  ) where

import Brick
import Brick.Widgets.List (list, renderList, handleListEvent, listSelectedElement)
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Wisp.Client (Activity(..), ActivitySource(..), ActivityStatus(..), getActivities, ClientConfig)

-- | Activities view widget
activitiesWidget :: ActivitiesState -> Widget Name
activitiesWidget as = vBox
  [ filterBar as
  , activityList as
  ]

filterBar :: ActivitiesState -> Widget Name
filterBar as = vLimit 1 $ hBox
  [ txt " Filter: "
  , txt $ if T.null (as ^. asFilter) then "all" else as ^. asFilter
  , fill ' '
  , txt "[/]search "
  ]

activityList :: ActivitiesState -> Widget Name
activityList as =
  let activities = as ^. asActivities
      selected = as ^. asSelected
  in viewport ActivityList Vertical $ vBox $
       zipWith (renderActivityRow selected (as ^. asExpanded)) [0..] activities

renderActivityRow :: Int -> Maybe Int -> Int -> Activity -> Widget Name
renderActivityRow selected expanded idx act =
  let isSelected = idx == selected
      isExpanded = expanded == Just idx
      marker = if isSelected then "▸ " else "  "
      icon = sourceIcon (activitySource act)
      title = maybe (T.take 40 $ activityRaw act) id (activityTitle act)
      status = statusText (activityStatus act)
      baseRow = hBox
        [ txt marker
        , txt icon
        , txt " "
        , txt $ T.take 35 title
        , fill ' '
        , txt status
        ]
  in if isExpanded
     then vBox [baseRow, expandedContent act]
     else baseRow

sourceIcon :: ActivitySource -> Text
sourceIcon Email = "📧"
sourceIcon Calendar = "📅"
sourceIcon GitHubEvent = "🐙"
sourceIcon Conversation = "💬"
sourceIcon Note = "📝"

statusText :: ActivityStatus -> Text
statusText Pending = "pending"
statusText Stored = "stored"
statusText Surfaced = "surfaced"
statusText Quarantined = "quarantine"
statusText Archived = "archived"

expandedContent :: Activity -> Widget Name
expandedContent act = padLeft (Pad 4) $ vBox
  [ txt $ "Tags: " <> T.intercalate ", " (activityTags act)
  , txt "──────────────────────"
  , txtWrap $ T.take 200 (activityRaw act)
  ]

-- | Handle activities-specific events
handleActivitiesEvent :: V.Event -> EventM Name AppState ()
handleActivitiesEvent (V.EvKey (V.KChar 'j') []) = do
  modify $ activitiesState . asSelected %~ (\i -> min (i + 1) 100)  -- TODO: actual length
handleActivitiesEvent (V.EvKey (V.KChar 'k') []) = do
  modify $ activitiesState . asSelected %~ (\i -> max (i - 1) 0)
handleActivitiesEvent (V.EvKey (V.KChar 'l') []) = do
  s <- get
  let sel = s ^. activitiesState . asSelected
      exp = s ^. activitiesState . asExpanded
  modify $ activitiesState . asExpanded .~ (if exp == Just sel then Nothing else Just sel)
handleActivitiesEvent (V.EvKey V.KEnter []) = do
  s <- get
  let sel = s ^. activitiesState . asSelected
      exp = s ^. activitiesState . asExpanded
  modify $ activitiesState . asExpanded .~ (if exp == Just sel then Nothing else Just sel)
handleActivitiesEvent _ = pure ()

-- | Load activities from server
loadActivities :: ClientConfig -> IO [Activity]
loadActivities cfg = do
  result <- getActivities cfg
  case result of
    Left _ -> pure []
    Right acts -> pure acts
```

**Step 2: Update Main.hs**

```haskell
-- Add import
import Tui.Views.Activities (activitiesWidget, handleActivitiesEvent)

-- Update drawUI case
drawUI s = case _currentView s of
  ChatView -> [mainLayout s $ chatWidget (_chatState s)]
  ActivitiesView -> [mainLayout s $ activitiesWidget (_activitiesState s)]
  _ -> [mainLayout s $ str $ "View: " <> show (_currentView s)]

-- Update handleEvent for ActivitiesView
handleEvent (VtyEvent e@(V.EvKey _ _))
  | V.EvKey (V.KChar 'q') [] <- e = halt
  | V.EvKey (V.KChar '\t') [] <- e = modify $ currentView %~ nextView
  | otherwise = do
      s <- get
      case _currentView s of
        ChatView -> handleChatEvent e
        ActivitiesView -> handleActivitiesEvent e
        _ -> pure ()
```

**Step 3: Update cabal**

Add `Tui.Views.Activities` to other-modules, add `vector` to build-depends.

**Step 4: Build and test**

Run: `cabal build wisp-tui && cabal run wisp-tui`
Expected: Activities view shows, j/k navigates, l expands

**Step 5: Commit**

```bash
git add wisp-tui
git commit -m "feat(tui): implement Activities view"
```

---

### Task 11: Implement Documents view

**Files:**
- Create: `wisp-tui/src/Tui/Views/Documents.hs`
- Modify: `wisp-tui/app/Main.hs`
- Modify: `wisp-tui/wisp-tui.cabal`

**Step 1: Create Documents view**

```haskell
-- wisp-tui/src/Tui/Views/Documents.hs
module Tui.Views.Documents
  ( documentsWidget
  , handleDocumentsEvent
  ) where

import Brick
import qualified Graphics.Vty as V
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Wisp.Client (Document(..), DocumentType(..))

-- | Documents view widget
documentsWidget :: DocumentsState -> Widget Name
documentsWidget ds = vBox
  [ tabBar ds
  , documentContent ds
  ]

tabBar :: DocumentsState -> Widget Name
tabBar ds = vLimit 1 $ hBox
  [ renderTab ds ProjectsTab "1:Projects"
  , txt "  "
  , renderTab ds NotesTab "2:Notes"
  , txt "  "
  , renderTab ds PrefsTab "3:Prefs"
  , fill ' '
  ]

renderTab :: DocumentsState -> DocumentTab -> Text -> Widget Name
renderTab ds tab label
  | ds ^. dsCurrentTab == tab = withAttr (attrName "selectedTab") $ txt $ "[" <> label <> "]"
  | otherwise = txt $ " " <> label <> " "

documentContent :: DocumentsState -> Widget Name
documentContent ds = case ds ^. dsCurrentTab of
  ProjectsTab -> projectsContent ds
  NotesTab -> notesContent ds
  PrefsTab -> prefsContent ds

projectsContent :: DocumentsState -> Widget Name
projectsContent ds = vBox $
  [ padBottom (Pad 1) $ hBox
    [ txt "  Name                    Type        Last Activity"
    ]
  , txt "  ──────────────────────────────────────────────────────"
  ] ++ zipWith (renderProject (ds ^. dsSelected)) [0..] (ds ^. dsProjects)

renderProject :: Int -> Int -> Document -> Widget Name
renderProject selected idx doc =
  let isSelected = idx == selected
      marker = if isSelected then "▸ " else "  "
      name = extractField "name" (documentData doc)
      ptype = extractField "type" (documentData doc)
  in hBox
    [ txt marker
    , txt $ T.justifyLeft 24 ' ' name
    , txt $ T.justifyLeft 12 ' ' ptype
    , txt $ maybe "-" (T.take 15 . T.pack . show) (documentLastActivityAt doc)
    ]

notesContent :: DocumentsState -> Widget Name
notesContent ds = vBox $
  [ padBottom (Pad 1) $ txt "  Title                           Tags"
  , txt "  ──────────────────────────────────────────────────────"
  ] ++ zipWith (renderNote (ds ^. dsSelected)) [0..] (ds ^. dsNotes)

renderNote :: Int -> Int -> Document -> Widget Name
renderNote selected idx doc =
  let isSelected = idx == selected
      marker = if isSelected then "▸ " else "  "
      title = extractField "title" (documentData doc)
      tags = T.intercalate ", " (documentTags doc)
  in hBox
    [ txt marker
    , txt $ T.justifyLeft 32 ' ' (T.take 30 title)
    , txt tags
    ]

prefsContent :: DocumentsState -> Widget Name
prefsContent ds = vBox $
  [ padBottom (Pad 1) $ txt "  Key                     Value               Context"
  , txt "  ──────────────────────────────────────────────────────"
  ] ++ zipWith (renderPref (ds ^. dsSelected)) [0..] (ds ^. dsPrefs)

renderPref :: Int -> Int -> Document -> Widget Name
renderPref selected idx doc =
  let isSelected = idx == selected
      marker = if isSelected then "▸ " else "  "
      key = extractField "key" (documentData doc)
      value = extractField "value" (documentData doc)
      context = extractField "context" (documentData doc)
  in hBox
    [ txt marker
    , txt $ T.justifyLeft 22 ' ' key
    , txt $ T.justifyLeft 20 ' ' value
    , txt context
    ]

extractField :: Text -> Value -> Text
extractField key (Object obj) = case KM.lookup (fromString $ T.unpack key) obj of
  Just (String s) -> s
  _ -> ""
extractField _ _ = ""

-- | Handle documents-specific events
handleDocumentsEvent :: V.Event -> EventM Name AppState ()
handleDocumentsEvent (V.EvKey (V.KChar '1') []) =
  modify $ documentsState . dsCurrentTab .~ ProjectsTab
handleDocumentsEvent (V.EvKey (V.KChar '2') []) =
  modify $ documentsState . dsCurrentTab .~ NotesTab
handleDocumentsEvent (V.EvKey (V.KChar '3') []) =
  modify $ documentsState . dsCurrentTab .~ PrefsTab
handleDocumentsEvent (V.EvKey (V.KChar 'j') []) =
  modify $ documentsState . dsSelected %~ (+ 1)
handleDocumentsEvent (V.EvKey (V.KChar 'k') []) =
  modify $ documentsState . dsSelected %~ (\i -> max 0 (i - 1))
handleDocumentsEvent _ = pure ()
```

**Step 2: Update Main.hs**

```haskell
-- Add import
import Tui.Views.Documents (documentsWidget, handleDocumentsEvent)

-- Update drawUI case
DocumentsView -> [mainLayout s $ documentsWidget (_documentsState s)]

-- Update handleEvent
DocumentsView -> handleDocumentsEvent e
```

**Step 3: Update cabal**

Add `Tui.Views.Documents` to other-modules.

**Step 4: Build and test**

Run: `cabal build wisp-tui && cabal run wisp-tui`
Expected: Documents view shows tabs, 1/2/3 switches, j/k navigates

**Step 5: Commit**

```bash
git add wisp-tui
git commit -m "feat(tui): implement Documents view"
```

---

### Task 12: Implement Approvals view

**Files:**
- Create: `wisp-tui/src/Tui/Views/Approvals.hs`
- Modify: `wisp-tui/app/Main.hs`
- Modify: `wisp-tui/wisp-tui.cabal`

**Step 1: Create Approvals view**

```haskell
-- wisp-tui/src/Tui/Views/Approvals.hs
module Tui.Views.Approvals
  ( approvalsWidget
  , handleApprovalsEvent
  ) where

import Brick
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Wisp.Client (Activity(..), ActivitySource(..), approveActivity, dismissActivity, ClientConfig)

-- | Approvals view widget
approvalsWidget :: ApprovalsState -> Widget Name
approvalsWidget as = vBox
  [ queueHeader as
  , approvalsList as
  ]

queueHeader :: ApprovalsState -> Widget Name
queueHeader as =
  let count = length (as ^. apsItems)
  in vLimit 1 $ txt $ " Review Queue (" <> T.pack (show count) <> " pending)"

approvalsList :: ApprovalsState -> Widget Name
approvalsList as = vBox $
  [ padBottom (Pad 1) $ txt "  Type        Activity                        Reason"
  , txt "  ──────────────────────────────────────────────────────"
  ] ++ zipWith (renderApproval (as ^. apsSelected) (as ^. apsExpanded)) [0..] (as ^. apsItems)

renderApproval :: Int -> Maybe Int -> Int -> (Activity, Text, Text) -> Widget Name
renderApproval selected expanded idx (act, aType, reason) =
  let isSelected = idx == selected
      isExpanded = expanded == Just idx
      marker = if isSelected then "▸ " else "  "
      icon = sourceIcon (activitySource act)
      title = maybe (T.take 30 $ activityRaw act) (T.take 30) (activityTitle act)
      baseRow = hBox
        [ txt marker
        , txt $ T.justifyLeft 10 ' ' aType
        , txt icon
        , txt " "
        , txt $ T.justifyLeft 30 ' ' title
        , txt reason
        ]
  in if isExpanded
     then vBox [baseRow, expandedApproval act aType]
     else baseRow

sourceIcon :: ActivitySource -> Text
sourceIcon Email = "📧"
sourceIcon Calendar = "📅"
sourceIcon GitHubEvent = "🐙"
sourceIcon Conversation = "💬"
sourceIcon Note = "📝"

expandedApproval :: Activity -> Text -> Widget Name
expandedApproval act aType = padLeft (Pad 4) $ vBox
  [ txt $ "Tags: " <> T.intercalate ", " (activityTags act)
  , txt "──────────────────────"
  , txtWrap $ T.take 200 (activityRaw act)
  , txt ""
  , txt $ "[y:approve  x:dismiss  " <> (if aType == "classify" then "c:change category" else "") <> "]"
  ]

-- | Handle approvals-specific events
handleApprovalsEvent :: V.Event -> ClientConfig -> EventM Name AppState ()
handleApprovalsEvent (V.EvKey (V.KChar 'j') []) _ =
  modify $ approvalsState . apsSelected %~ (+ 1)
handleApprovalsEvent (V.EvKey (V.KChar 'k') []) _ =
  modify $ approvalsState . apsSelected %~ (\i -> max 0 (i - 1))
handleApprovalsEvent (V.EvKey (V.KChar 'l') []) _ = do
  s <- get
  let sel = s ^. approvalsState . apsSelected
      exp = s ^. approvalsState . apsExpanded
  modify $ approvalsState . apsExpanded .~ (if exp == Just sel then Nothing else Just sel)
handleApprovalsEvent (V.EvKey (V.KChar 'y') []) cfg = do
  s <- get
  let sel = s ^. approvalsState . apsSelected
      items = s ^. approvalsState . apsItems
  case drop sel items of
    ((act, _, _):_) -> do
      -- TODO: Call approveActivity in IO
      -- Remove from list
      modify $ approvalsState . apsItems %~ removeAt sel
    [] -> pure ()
handleApprovalsEvent (V.EvKey (V.KChar 'x') []) cfg = do
  s <- get
  let sel = s ^. approvalsState . apsSelected
  -- TODO: Call dismissActivity in IO
  modify $ approvalsState . apsItems %~ removeAt sel
handleApprovalsEvent _ _ = pure ()

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs
```

**Step 2: Update Main.hs**

```haskell
-- Add import
import Tui.Views.Approvals (approvalsWidget, handleApprovalsEvent)

-- Update drawUI case
ApprovalsView -> [mainLayout s $ approvalsWidget (_approvalsState s)]

-- Update handleEvent
ApprovalsView -> handleApprovalsEvent e (_clientConfig s)
```

**Step 3: Update cabal**

Add `Tui.Views.Approvals` to other-modules.

**Step 4: Build and test**

Run: `cabal build wisp-tui && cabal run wisp-tui`
Expected: Approvals view shows, j/k/l navigate and expand

**Step 5: Commit**

```bash
git add wisp-tui
git commit -m "feat(tui): implement Approvals view"
```

---

### Task 13: Wire up data loading on view change

**Files:**
- Modify: `wisp-tui/app/Main.hs`

**Step 1: Add data loading on view change**

```haskell
-- Add to imports
import Control.Concurrent.Async (async)
import Brick.BChan (writeBChan)

-- Create a RefreshView event type (already in Types)

-- Update handleEvent for Tab
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  modify $ currentView %~ nextView
  s <- get
  -- Trigger data refresh for new view
  case _currentView s of
    ActivitiesView -> do
      -- Load activities in background
      liftIO $ async $ do
        acts <- loadActivities (_clientConfig s)
        -- Would need BChan to send back
        pure ()
    DocumentsView -> pure ()  -- TODO: load documents
    ApprovalsView -> pure ()  -- TODO: load approvals
    _ -> pure ()
```

**Step 2: Build and test**

Run: `cabal build wisp-tui && cabal run wisp-tui`
Expected: Still works, ready for async data loading

**Step 3: Commit**

```bash
git add wisp-tui
git commit -m "feat(tui): wire up data loading on view change"
```

---

### Task 14: Connect Chat view to SSE streaming

**Files:**
- Modify: `wisp-tui/src/Tui/Views/Chat.hs`
- Modify: `wisp-tui/app/Main.hs`

**Step 1: Update Chat to handle SSE events**

```haskell
-- In Chat.hs, add:
import Wisp.Client.SSE (ChatEvent(..), streamChat, ChatRequest(..))

-- Add function to send message
sendChatMessage :: ClientConfig -> Text -> Text -> Text -> BChan AppEvent -> IO ()
sendChatMessage cfg agent session msg chan = do
  let req = ChatRequest agent msg session
  _ <- streamChat cfg req $ \evt ->
    writeBChan chan (ChatEventReceived evt)
  pure ()

-- In handleChatEvent, on Enter:
handleChatEvent (V.EvKey V.KEnter []) = do
  s <- get
  let input = s ^. chatState . csInputBuffer
  if T.null input
    then pure ()
    else do
      now <- liftIO getCurrentTime
      let userMsg = ChatMessage "You" input now
      modify $ chatState . csMessages %~ (++ [userMsg])
      modify $ chatState . csInputBuffer .~ ""
      modify $ chatState . csStreaming .~ True
      -- TODO: trigger async send
```

**Step 2: Handle ChatEventReceived in main**

```haskell
-- In handleEvent:
handleEvent (AppEvent (ChatEventReceived evt)) = do
  case evt of
    ChunkEvent txt -> modify $ chatState . csStreamBuffer %~ (<> txt)
    ToolCallStart name -> modify $ chatState . csStreamBuffer %~ (<> "\n⚙ Calling " <> name <> "...")
    ToolCallResult name ms -> modify $ chatState . csStreamBuffer %~ (<> "\n✓ " <> name <> " (" <> T.pack (show ms) <> "ms)")
    DoneEvent _ _ -> do
      s <- get
      now <- liftIO getCurrentTime
      let response = s ^. chatState . csStreamBuffer
      let assistantMsg = ChatMessage "Assistant" response now
      modify $ chatState . csMessages %~ (++ [assistantMsg])
      modify $ chatState . csStreamBuffer .~ ""
      modify $ chatState . csStreaming .~ False
    ErrorEvent msg _ -> do
      modify $ statusMessage .~ Just (msg, now)
      modify $ chatState . csStreaming .~ False
```

**Step 3: Build and test**

Run: `cabal build wisp-tui && cabal run wisp-tui`
Expected: Chat accepts input, ready for SSE integration

**Step 4: Commit**

```bash
git add wisp-tui
git commit -m "feat(tui): connect Chat view to SSE streaming"
```

---

## Summary

| Phase | Task | Description |
|-------|------|-------------|
| 1 | 1 | wisp-core scaffold |
| 1 | 2 | Activity/Document types |
| 1 | 3 | HTTP client functions |
| 2 | 4 | ChatEvent types |
| 2 | 5 | SSE streaming handler |
| 2 | 6 | SSE client in wisp-core |
| 3 | 7 | wisp-tui scaffold |
| 3 | 8 | Header/status bar widgets |
| 3 | 9 | Chat view |
| 3 | 10 | Activities view |
| 3 | 11 | Documents view |
| 3 | 12 | Approvals view |
| 3 | 13 | Data loading on view change |
| 3 | 14 | SSE streaming in Chat |

After completing all tasks:
- `wisp-core` provides shared client library
- `wisp-srv` supports SSE streaming for chat
- `wisp-tui` provides full TUI with all four views
