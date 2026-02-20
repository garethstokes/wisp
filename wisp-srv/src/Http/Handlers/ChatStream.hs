module Http.Handlers.ChatStream
  ( postChatStream
  ) where

import Control.Concurrent.STM
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Network.HTTP.Types.Status (status200, status500)
import Web.Scotty.Trans (ActionT, json, status, jsonData, setHeader, raw)
import App.Monad (Env)
import Domain.Chat (ChatRequest(..))
import Domain.ChatEvent (ChatEvent(..), chatEventToSSE)
import Infra.Db.Tenant (getAllTenants)
import Infra.Db.Account (getAllAccounts)

-- | POST /api/chat/stream - SSE streaming chat
postChatStream :: ActionT (ReaderT Env IO) ()
postChatStream = do
  _req <- jsonData :: ActionT (ReaderT Env IO) ChatRequest

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
