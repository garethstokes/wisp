module Domain.Schema
  ( Schema(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Schema = Schema
  { name :: Text
  , version :: Int
  } deriving (Generic, Show, Eq)

instance ToJSON Schema
instance FromJSON Schema
