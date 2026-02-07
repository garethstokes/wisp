{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agents.Run
  ( withRunLogging
  , RunContext(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON, object, (.=))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import App.Monad (App)
import Domain.Run
import Infra.Db.Run (createRun, appendEvent, updateRunStatus)
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
