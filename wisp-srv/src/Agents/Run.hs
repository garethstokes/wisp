{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agents.Run
  ( withRunLogging
  , RunContext(..)
  , callClaudeLogged
  , logToolRequest
  , logToolSuccess
  , logToolFailure
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value, toJSON, object, (.=))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import App.Config (Config(..), ClaudeConfig(..))
import App.Monad (App, Env(..))
import Domain.Run
import Infra.Db.Run (createRun, appendEvent, updateRunStatus)
import Infra.Claude.Client (callClaudeWithSystem)
import Domain.Chat (ChatMessage(..), ChatResponse(..))

-- | Context passed to agent during a logged run
data RunContext = RunContext
  { rcRunId :: RunId
  , rcSessionId :: Maybe Text
  }

-- | Wrap an agent's handleChat with run logging
-- Creates a run, logs the input event, delegates to the agent,
-- and logs the outcome (completed/failed)
withRunLogging
  :: Text                                         -- ^ Agent ID (e.g., "wisp/concierge")
  -> Maybe Text                                   -- ^ Session ID
  -> [ChatMessage]                                -- ^ Input messages
  -> ([ChatMessage] -> App (Either Text ChatResponse))  -- ^ The agent's handleChat
  -> App (Either Text ChatResponse)
withRunLogging agentId mSessionId messages handleChat = do
  -- Create the run
  runId <- createRun agentId mSessionId Nothing

  -- Log input event
  now <- liftIO getCurrentTime
  let inputEvent = InputEvent
        { eventId = ""  -- Will be assigned by appendEvent
        , eventParentEventId = Nothing
        , eventTimestamp = now
        , eventTool = "message_from_user"
        , eventData = toJSON $ object
            [ "messages" .= messages
            ]
        }
  _ <- appendEvent runId inputEvent

  -- Delegate to actual handler
  result <- handleChat messages

  -- Log outcome
  case result of
    Left err -> do
      updateRunStatus runId Failed
      pure $ Left err
    Right response -> do
      now' <- liftIO getCurrentTime
      let responseEvent = ToolSucceeded
            { eventId = ""
            , eventParentEventId = Nothing
            , eventTimestamp = now'
            , eventToolName = "respond_to_user"
            , eventResult = toJSON response
            }
      _ <- appendEvent runId responseEvent
      updateRunStatus runId Completed
      pure $ Right response

-- | Call Claude with logging - logs LlmCalled event with full prompt/response
callClaudeLogged
  :: RunContext
  -> Text           -- ^ System prompt
  -> Text           -- ^ User prompt
  -> App (Either Text Text)
callClaudeLogged ctx systemPrompt userPrompt = do
  claudeCfg <- asks (claude . config)
  let modelName = model claudeCfg

  -- Call Claude
  result <- liftIO $ callClaudeWithSystem
    (apiKey claudeCfg)
    modelName
    systemPrompt
    userPrompt

  -- Log the LLM call (success or failure)
  now <- liftIO getCurrentTime
  case result of
    Left err -> do
      let event = LlmCalled
            { eventId = ""
            , eventParentEventId = Nothing
            , eventTimestamp = now
            , eventModel = modelName
            , eventSystemPrompt = systemPrompt
            , eventUserPrompt = userPrompt
            , eventRawResponse = "ERROR: " <> err
            }
      _ <- appendEvent (rcRunId ctx) event
      pure $ Left err
    Right response -> do
      let event = LlmCalled
            { eventId = ""
            , eventParentEventId = Nothing
            , eventTimestamp = now
            , eventModel = modelName
            , eventSystemPrompt = systemPrompt
            , eventUserPrompt = userPrompt
            , eventRawResponse = response
            }
      _ <- appendEvent (rcRunId ctx) event
      pure $ Right response

-- | Log a tool request event
logToolRequest
  :: RunContext
  -> Text           -- ^ Tool name
  -> Value          -- ^ Tool arguments
  -> App Text       -- ^ Returns event ID for linking
logToolRequest ctx toolName toolArgs = do
  now <- liftIO getCurrentTime
  let event = ToolRequested
        { eventId = ""
        , eventParentEventId = Nothing
        , eventTimestamp = now
        , eventToolName = toolName
        , eventToolArgs = toolArgs
        }
  appendEvent (rcRunId ctx) event

-- | Log a tool success event
logToolSuccess
  :: RunContext
  -> Text           -- ^ Tool name
  -> Value          -- ^ Result
  -> App ()
logToolSuccess ctx toolName result = do
  now <- liftIO getCurrentTime
  let event = ToolSucceeded
        { eventId = ""
        , eventParentEventId = Nothing
        , eventTimestamp = now
        , eventToolName = toolName
        , eventResult = result
        }
  _ <- appendEvent (rcRunId ctx) event
  pure ()

-- | Log a tool failure event
logToolFailure
  :: RunContext
  -> Text           -- ^ Tool name
  -> Text           -- ^ Error message
  -> App ()
logToolFailure ctx toolName errMsg = do
  now <- liftIO getCurrentTime
  let event = ToolFailed
        { eventId = ""
        , eventParentEventId = Nothing
        , eventTimestamp = now
        , eventToolName = toolName
        , eventError = errMsg
        }
  _ <- appendEvent (rcRunId ctx) event
  pure ()
