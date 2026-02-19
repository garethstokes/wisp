module Domain.Account
  ( Account(..)
  , AccountProvider(..)
  , NewAccount(..)
  , accountIdentifier
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value, object, withText, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Id (EntityId(..))
import GHC.Generics (Generic)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson as Aeson

data AccountProvider = Google | GitHub
  deriving (Eq, Show, Generic)

instance ToJSON AccountProvider where
  toJSON Google = "google"
  toJSON GitHub = "github"

instance FromJSON AccountProvider where
  parseJSON = withText "AccountProvider" $ \case
    "google" -> pure Google
    "github" -> pure GitHub
    other -> fail $ "Unknown provider: " <> show other

data Account = Account
  { accountId :: EntityId
  , accountProvider :: AccountProvider
  , accountDisplayName :: Maybe Text
  , accountDetails :: Value  -- JSON: provider-specific data
  , accountCreatedAt :: UTCTime
  } deriving (Show, Eq)

instance ToJSON Account where
  toJSON a = object
    [ "id" .= unEntityId (accountId a)
    , "provider" .= accountProvider a
    , "display_name" .= accountDisplayName a
    , "details" .= accountDetails a
    , "created_at" .= accountCreatedAt a
    ]

-- Helper to get primary identifier (email for Google, username for GitHub)
accountIdentifier :: Account -> Maybe Text
accountIdentifier acc = case accountDetails acc of
  Aeson.Object obj -> case accountProvider acc of
    Google -> case KM.lookup "email" obj of
      Just (Aeson.String e) -> Just e
      _ -> Nothing
    GitHub -> case KM.lookup "username" obj of
      Just (Aeson.String u) -> Just u
      _ -> Nothing
  _ -> Nothing

data NewAccount = NewAccount
  { newAccountProvider :: AccountProvider
  , newAccountDisplayName :: Maybe Text
  , newAccountDetails :: Value
  } deriving (Show, Eq)
