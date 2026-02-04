module Http.Handlers.Chat
  ( postChat
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Network.HTTP.Types.Status (status400, status500)
import Web.Scotty.Trans (ActionT, json, status, jsonData)
import App.Monad (Env)
import Domain.Chat (ChatRequest(..))
import Agents.Dispatcher (dispatchChat, getAgent)
import Domain.Agent (agentImplemented)

-- POST /chat
postChat :: ActionT (ReaderT Env IO) ()
postChat = do
  req <- jsonData :: ActionT (ReaderT Env IO) ChatRequest
  let agent = chatAgent req
  let messages = chatMessages req
  let tz = chatTimezone req  -- Optional timezone from client

  -- Validate agent exists
  case getAgent agent of
    Nothing -> do
      status status400
      json $ object ["error" .= ("Unknown agent: " <> agent :: Text)]
    Just agentInfo -> do
      -- Check if implemented
      if not (agentImplemented agentInfo)
        then do
          status status400
          json $ object ["error" .= ("Agent not implemented: " <> agent :: Text)]
        else do
          -- Dispatch to agent with timezone
          result <- lift $ dispatchChat agent messages tz
          case result of
            Left err -> do
              status status500
              json $ object ["error" .= err]
            Right response -> json response
