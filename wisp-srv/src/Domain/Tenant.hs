module Domain.Tenant
  ( Tenant(..)
  , TenantId(..)
  , NewTenant(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withText)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID

-- | Tenant ID wraps a UUID
newtype TenantId = TenantId { unTenantId :: UUID }
  deriving (Show, Eq)

instance ToJSON TenantId where
  toJSON (TenantId uuid) = toJSON (UUID.toText uuid)

instance FromJSON TenantId where
  parseJSON = withText "TenantId" $ \t ->
    case UUID.fromText t of
      Just uuid -> pure (TenantId uuid)
      Nothing -> fail $ "Invalid UUID: " <> T.unpack t

-- | A tenant owns accounts, agents, skills, and knowledge
data Tenant = Tenant
  { tenantId :: TenantId
  , tenantName :: Text
  , tenantCreatedAt :: UTCTime
  } deriving (Show, Eq)

instance ToJSON Tenant where
  toJSON t = object
    [ "id" .= tenantId t
    , "name" .= tenantName t
    , "created_at" .= tenantCreatedAt t
    ]

-- | Data needed to create a new tenant
newtype NewTenant = NewTenant
  { newTenantName :: Text
  } deriving (Show, Eq)
