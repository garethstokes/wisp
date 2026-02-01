module Domain.Id
  ( EntityId(..)
  , newEntityId
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.FromField (FromField)
import Data.NanoID (customNanoID, defaultAlphabet, unNanoID)
import System.Random.MWC (createSystemRandom)
import Data.ByteString.Char8 (unpack)

newtype EntityId = EntityId { unEntityId :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, ToField, FromField)

-- | Generate a new 12-character EntityId using NanoID
newEntityId :: IO EntityId
newEntityId = do
  gen <- createSystemRandom
  nid <- customNanoID defaultAlphabet 12 gen
  pure $ EntityId (T.pack $ unpack $ unNanoID nid)
