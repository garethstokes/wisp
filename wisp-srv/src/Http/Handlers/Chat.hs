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
import Domain.Tenant (TenantId, Tenant(..))
import Domain.Account (Account(..))
import Agents.Dispatcher (dispatchChat)
import Infra.Db.Tenant (getAllTenants)
import Infra.Db.Account (getAllAccounts)

--------------------------------------------------------------------------------
-- Tenant resolution (TODO: get from auth token)
--------------------------------------------------------------------------------

-- | Get the current tenant (temporary: uses first tenant)
getCurrentTenant :: ActionT (ReaderT Env IO) (Maybe TenantId)
getCurrentTenant = do
  tenants <- lift getAllTenants
  pure $ case tenants of
    (t:_) -> Just (tenantId t)
    [] -> Nothing

-- | Get the first account for the tenant (temporary: should come from auth)
getFirstAccount :: ActionT (ReaderT Env IO) (Maybe Account)
getFirstAccount = do
  accounts <- lift getAllAccounts
  pure $ case accounts of
    (a:_) -> Just a
    [] -> Nothing

--------------------------------------------------------------------------------
-- Chat Endpoint
--------------------------------------------------------------------------------

-- POST /chat
postChat :: ActionT (ReaderT Env IO) ()
postChat = do
  req <- jsonData :: ActionT (ReaderT Env IO) ChatRequest
  let agentName = chatAgent req
  let messages = chatMessages req
  let tz = chatTimezone req  -- Optional timezone from client

  -- Get tenant
  mTenant <- getCurrentTenant
  case mTenant of
    Nothing -> do
      status status500
      json $ object ["error" .= ("No tenant configured. Create one with: wisp tenant create <name>" :: Text)]
    Just tid -> do
      -- Get account for tool execution context
      mAccount <- getFirstAccount
      case mAccount of
        Nothing -> do
          status status500
          json $ object ["error" .= ("No accounts configured. Run: wisp auth" :: Text)]
        Just account -> do
          -- Dispatch to agent
          result <- lift $ dispatchChat tid (accountId account) agentName messages tz
          case result of
            Left err -> do
              status status400
              json $ object ["error" .= err]
            Right response -> json response
