module Domain.Account
  ( Account(..)
  , NewAccount(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Id (EntityId(..))

data Account = Account
  { accountId :: EntityId
  , accountEmail :: Text
  , accountDisplayName :: Maybe Text
  , accountCreatedAt :: UTCTime
  } deriving (Show, Eq)

instance ToJSON Account where
  toJSON a = object
    [ "id" .= unEntityId (accountId a)
    , "email" .= accountEmail a
    , "display_name" .= accountDisplayName a
    , "created_at" .= accountCreatedAt a
    ]

data NewAccount = NewAccount
  { newAccountEmail :: Text
  , newAccountDisplayName :: Maybe Text
  } deriving (Show, Eq)
