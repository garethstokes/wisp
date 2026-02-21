module Http.Handlers.ChatStream
  ( postChatStream
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Builder as BB
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types.Status (status200)
import Web.Scotty.Trans (ActionT, jsonData, setHeader, status, stream)
import App.Monad (Env)
import Agents.Dispatcher (dispatchChatStreaming)
import Domain.Account (Account(..))
import Domain.Chat (ChatRequest(..), ChatResponse(..))
import Domain.ChatEvent (ChatEvent(..), chatEventToSSE)
import Domain.Tenant (Tenant(..))
import Infra.Db.Tenant (getAllTenants)
import Infra.Db.Account (getAllAccounts)

-- | POST /api/chat/stream - SSE streaming chat
postChatStream :: ActionT (ReaderT Env IO) ()
postChatStream = do
  reqValue <- jsonData :: ActionT (ReaderT Env IO) Value
  let mSessionId = extractSessionId reqValue

  status status200
  setHeader "Content-Type" "text/event-stream"
  setHeader "Cache-Control" "no-cache"
  setHeader "Connection" "keep-alive"
  setHeader "X-Accel-Buffering" "no"

  env <- lift ask
  stream $ \write flush -> do
    -- Emit a comment immediately to flush headers and keep connection alive
    write $ BB.byteString ": stream started\n\n"
    flush

    let emit :: ChatEvent -> IO ()
        emit evt = do
          write $ BB.lazyByteString $ chatEventToSSE evt
          flush

    case Aeson.fromJSON reqValue of
      Aeson.Error err -> do
        emit $ ErrorEvent (T.pack err) "invalid_request"
      Aeson.Success req -> do
        result <- try $ runReaderT (runChat req mSessionId emit) env
        case result of
          Left (e :: SomeException) ->
            emit $ ErrorEvent (T.pack (show e)) "exception"
          Right () -> pure ()

runChat :: ChatRequest -> Maybe Text -> (ChatEvent -> IO ()) -> ReaderT Env IO ()
runChat req mSessionId emit = do
  tenants <- getAllTenants
  accounts <- getAllAccounts

  case (tenants, accounts) of
    ([], _) -> liftIO $ emit $ ErrorEvent "No tenant configured. Create one with: wisp tenant create <name>" "no_tenant"
    (_, []) -> liftIO $ emit $ ErrorEvent "No accounts configured. Run: wisp auth" "no_accounts"
    (t:_, a:_) -> do
      let agentName = chatAgent req
          messages = chatMessages req
          tz = chatTimezone req

      result <- dispatchChatStreaming (tenantId t) (accountId a) agentName messages tz mSessionId emit
      case result of
        Left err -> liftIO $ emit $ ErrorEvent err "chat_failed"
        Right resp -> do
          liftIO $ emit $ ChunkEvent (responseMessage resp)
          liftIO $ emit $ DoneEvent (fromMaybe "default" mSessionId) 0

extractSessionId :: Value -> Maybe Text
extractSessionId (Aeson.Object obj) =
  case KM.lookup "session" obj of
    Just (Aeson.String s) -> Just s
    _ -> Nothing
extractSessionId _ = Nothing
