module Infra.Db.Tenant
  ( createTenant
  , getTenantById
  , getAllTenants
  , assignAccountToTenant
  , getAccountTenant
  ) where

import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Domain.Id (EntityId(..))
import Domain.Tenant (Tenant(..), TenantId(..), NewTenant(..))
import App.Monad (App, getConn)

-- UUID field instances for postgresql-simple
instance FromField TenantId where
  fromField f mdata = TenantId <$> fromField f mdata

instance ToField TenantId where
  toField (TenantId uuid) = toField uuid

instance FromRow Tenant where
  fromRow = Tenant
    <$> field  -- id (uuid -> TenantId)
    <*> field  -- name
    <*> field  -- created_at

-- | Create a new tenant, returns the created tenant
createTenant :: NewTenant -> App Tenant
createTenant newTenant = do
  conn <- getConn
  results <- liftIO $ query conn
    "INSERT INTO tenants (name) VALUES (?) RETURNING id, name, created_at"
    (Only $ newTenantName newTenant)
  case results of
    [tenant] -> pure tenant
    _ -> error "Failed to create tenant"

-- | Get tenant by ID
getTenantById :: TenantId -> App (Maybe Tenant)
getTenantById tid = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT id, name, created_at FROM tenants WHERE id = ?"
    (Only tid)
  pure $ case results of
    [tenant] -> Just tenant
    _ -> Nothing

-- | Get all tenants
getAllTenants :: App [Tenant]
getAllTenants = do
  conn <- getConn
  liftIO $ query_ conn
    "SELECT id, name, created_at FROM tenants ORDER BY created_at"

-- | Assign an account to a tenant
assignAccountToTenant :: EntityId -> TenantId -> App ()
assignAccountToTenant accountId tenantId = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "UPDATE accounts SET tenant_id = ? WHERE id = ?"
    (tenantId, unEntityId accountId)
  pure ()

-- | Get the tenant for an account
getAccountTenant :: EntityId -> App (Maybe Tenant)
getAccountTenant accountId = do
  conn <- getConn
  results <- liftIO $ query conn
    "SELECT t.id, t.name, t.created_at \
    \FROM tenants t \
    \JOIN accounts a ON a.tenant_id = t.id \
    \WHERE a.id = ?"
    (Only $ unEntityId accountId)
  pure $ case results of
    [tenant] -> Just tenant
    _ -> Nothing
