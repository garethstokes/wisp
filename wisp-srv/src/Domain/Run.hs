{-# LANGUAGE RecordWildCards #-}

module Domain.Run
  ( RunId(..)
  , Run(..)
  , RunStatus(..)
  , RunEvent(..)
  , runStatusToText
  , runStatusFromText
  , eventTypeToText
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value, withText, withObject, object, (.=), (.:), (.:?))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import GHC.Generics (Generic)

-- | Run identifier (12-char NanoID like EntityId)
newtype RunId = RunId { unRunId :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, ToField, FromField)

-- | Run status
data RunStatus
  = Running     -- Currently executing
  | Waiting     -- Paused, waiting for external input
  | Completed   -- Successfully finished
  | Failed      -- Terminated with error
  deriving (Eq, Show, Generic)

runStatusToText :: RunStatus -> Text
runStatusToText = \case
  Running -> "running"
  Waiting -> "waiting"
  Completed -> "completed"
  Failed -> "failed"

runStatusFromText :: Text -> Maybe RunStatus
runStatusFromText = \case
  "running" -> Just Running
  "waiting" -> Just Waiting
  "completed" -> Just Completed
  "failed" -> Just Failed
  _ -> Nothing

instance ToJSON RunStatus where
  toJSON = toJSON . runStatusToText

instance FromJSON RunStatus where
  parseJSON = withText "RunStatus" $ \t ->
    case runStatusFromText t of
      Just s -> pure s
      Nothing -> fail "Invalid run status"

-- | Event types in a run
data RunEvent
  = InputEvent
      { eventId :: Text
      , eventParentEventId :: Maybe Text
      , eventTimestamp :: UTCTime
      , eventTool :: Text
      , eventData :: Value
      }
  | ContextAssembled
      { eventId :: Text
      , eventParentEventId :: Maybe Text
      , eventTimestamp :: UTCTime
      , eventContext :: Value
      }
  | LlmCalled
      { eventId :: Text
      , eventParentEventId :: Maybe Text
      , eventTimestamp :: UTCTime
      , eventModel :: Text
      , eventSystemPrompt :: Text
      , eventUserPrompt :: Text
      , eventRawResponse :: Text
      , eventInputTokens :: Maybe Int
      , eventOutputTokens :: Maybe Int
      }
  | ToolRequested
      { eventId :: Text
      , eventParentEventId :: Maybe Text
      , eventTimestamp :: UTCTime
      , eventToolName :: Text
      , eventToolArgs :: Value
      }
  | ToolSucceeded
      { eventId :: Text
      , eventParentEventId :: Maybe Text
      , eventTimestamp :: UTCTime
      , eventToolName :: Text
      , eventResult :: Value
      }
  | ToolFailed
      { eventId :: Text
      , eventParentEventId :: Maybe Text
      , eventTimestamp :: UTCTime
      , eventToolName :: Text
      , eventError :: Text
      }
  deriving (Eq, Show, Generic)

eventTypeToText :: RunEvent -> Text
eventTypeToText = \case
  InputEvent {} -> "input"
  ContextAssembled {} -> "context_assembled"
  LlmCalled {} -> "llm_called"
  ToolRequested {} -> "tool_requested"
  ToolSucceeded {} -> "tool_succeeded"
  ToolFailed {} -> "tool_failed"

instance ToJSON RunEvent where
  toJSON e = object $ case e of
    InputEvent {..} ->
      [ "type" .= ("input" :: Text)
      , "id" .= eventId
      , "parent_event_id" .= eventParentEventId
      , "timestamp" .= eventTimestamp
      , "tool" .= eventTool
      , "data" .= eventData
      ]
    ContextAssembled {..} ->
      [ "type" .= ("context_assembled" :: Text)
      , "id" .= eventId
      , "parent_event_id" .= eventParentEventId
      , "timestamp" .= eventTimestamp
      , "context" .= eventContext
      ]
    LlmCalled {..} ->
      [ "type" .= ("llm_called" :: Text)
      , "id" .= eventId
      , "parent_event_id" .= eventParentEventId
      , "timestamp" .= eventTimestamp
      , "model" .= eventModel
      , "system_prompt" .= eventSystemPrompt
      , "user_prompt" .= eventUserPrompt
      , "raw_response" .= eventRawResponse
      , "input_tokens" .= eventInputTokens
      , "output_tokens" .= eventOutputTokens
      ]
    ToolRequested {..} ->
      [ "type" .= ("tool_requested" :: Text)
      , "id" .= eventId
      , "parent_event_id" .= eventParentEventId
      , "timestamp" .= eventTimestamp
      , "tool_name" .= eventToolName
      , "tool_args" .= eventToolArgs
      ]
    ToolSucceeded {..} ->
      [ "type" .= ("tool_succeeded" :: Text)
      , "id" .= eventId
      , "parent_event_id" .= eventParentEventId
      , "timestamp" .= eventTimestamp
      , "tool_name" .= eventToolName
      , "result" .= eventResult
      ]
    ToolFailed {..} ->
      [ "type" .= ("tool_failed" :: Text)
      , "id" .= eventId
      , "parent_event_id" .= eventParentEventId
      , "timestamp" .= eventTimestamp
      , "tool_name" .= eventToolName
      , "error" .= eventError
      ]

instance FromJSON RunEvent where
  parseJSON = withObject "RunEvent" $ \v -> do
    eventType <- v .: "type"
    case (eventType :: Text) of
      "input" -> InputEvent
        <$> v .: "id"
        <*> v .:? "parent_event_id"
        <*> v .: "timestamp"
        <*> v .: "tool"
        <*> v .: "data"
      "context_assembled" -> ContextAssembled
        <$> v .: "id"
        <*> v .:? "parent_event_id"
        <*> v .: "timestamp"
        <*> v .: "context"
      "llm_called" -> LlmCalled
        <$> v .: "id"
        <*> v .:? "parent_event_id"
        <*> v .: "timestamp"
        <*> v .: "model"
        <*> v .: "system_prompt"
        <*> v .: "user_prompt"
        <*> v .: "raw_response"
        <*> v .:? "input_tokens"
        <*> v .:? "output_tokens"
      "tool_requested" -> ToolRequested
        <$> v .: "id"
        <*> v .:? "parent_event_id"
        <*> v .: "timestamp"
        <*> v .: "tool_name"
        <*> v .: "tool_args"
      "tool_succeeded" -> ToolSucceeded
        <$> v .: "id"
        <*> v .:? "parent_event_id"
        <*> v .: "timestamp"
        <*> v .: "tool_name"
        <*> v .: "result"
      "tool_failed" -> ToolFailed
        <$> v .: "id"
        <*> v .:? "parent_event_id"
        <*> v .: "timestamp"
        <*> v .: "tool_name"
        <*> v .: "error"
      _ -> fail $ "Unknown event type: " <> T.unpack eventType

-- | Full run with events
data Run = Run
  { runId :: RunId
  , runParentRunId :: Maybe RunId
  , runAgent :: Text
  , runSessionId :: Maybe Text
  , runCreatedAt :: UTCTime
  , runUpdatedAt :: UTCTime
  , runStatus :: RunStatus
  , runEvents :: [RunEvent]
  }
  deriving (Eq, Show, Generic)

instance ToJSON Run where
  toJSON Run {..} = object
    [ "id" .= runId
    , "parent_run_id" .= runParentRunId
    , "agent" .= runAgent
    , "session_id" .= runSessionId
    , "created_at" .= runCreatedAt
    , "updated_at" .= runUpdatedAt
    , "status" .= runStatus
    , "events" .= runEvents
    ]
