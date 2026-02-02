module Domain.Person
  ( Person(..)
  , NewPerson(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Id (EntityId(..))

data Person = Person
  { personId :: EntityId
  , personEmail :: Text
  , personDisplayName :: Maybe Text
  , personPersonas :: Maybe [Text]
  , personRelationship :: Maybe Text
  , personOrganisation :: Maybe Text
  , personNotes :: Maybe Text
  , personFirstContact :: Maybe UTCTime
  , personLastContact :: Maybe UTCTime
  , personContactCount :: Int
  , personCreatedAt :: UTCTime
  } deriving (Show, Eq)

instance ToJSON Person where
  toJSON p = object
    [ "id" .= unEntityId (personId p)
    , "email" .= personEmail p
    , "display_name" .= personDisplayName p
    , "personas" .= personPersonas p
    , "relationship" .= personRelationship p
    , "organisation" .= personOrganisation p
    , "notes" .= personNotes p
    , "first_contact" .= personFirstContact p
    , "last_contact" .= personLastContact p
    , "contact_count" .= personContactCount p
    , "created_at" .= personCreatedAt p
    ]

data NewPerson = NewPerson
  { newPersonEmail :: Text
  , newPersonDisplayName :: Maybe Text
  } deriving (Show, Eq)
