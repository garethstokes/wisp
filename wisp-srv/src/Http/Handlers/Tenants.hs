module Http.Handlers.Tenants
  ( getTenantsList
  , postTenant
  , getTenantById
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import Data.Text (Text)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status400, status404)
import Web.Scotty.Trans (ActionT, json, status, jsonData, captureParam)
import App.Monad (Env)
import Domain.Tenant (Tenant(..), TenantId(..), NewTenant(..))
import qualified Infra.Db.Tenant as DbTenant

-- | Request body for creating a tenant
data CreateTenantRequest = CreateTenantRequest
  { createTenantName :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CreateTenantRequest
instance ToJSON CreateTenantRequest

-- | GET /api/tenants - list all tenants
getTenantsList :: ActionT (ReaderT Env IO) ()
getTenantsList = do
  tenants <- lift DbTenant.getAllTenants
  json $ object
    [ "tenants" .= tenants
    , "count" .= length tenants
    ]

-- | POST /api/tenants - create a new tenant
postTenant :: ActionT (ReaderT Env IO) ()
postTenant = do
  req <- jsonData :: ActionT (ReaderT Env IO) CreateTenantRequest
  let newTenant = NewTenant { newTenantName = createTenantName req }
  tenant <- lift $ DbTenant.createTenant newTenant
  json $ object
    [ "status" .= ("created" :: Text)
    , "tenant" .= tenant
    ]

-- | GET /api/tenants/:id - get tenant by ID
getTenantById :: ActionT (ReaderT Env IO) ()
getTenantById = do
  idParam <- captureParam "id"
  case UUID.fromText idParam of
    Nothing -> do
      status status400
      json $ object ["error" .= ("Invalid tenant ID format" :: Text)]
    Just uuid -> do
      mTenant <- lift $ DbTenant.getTenantById (TenantId uuid)
      case mTenant of
        Nothing -> do
          status status404
          json $ object ["error" .= ("Tenant not found" :: Text)]
        Just tenant -> json tenant
