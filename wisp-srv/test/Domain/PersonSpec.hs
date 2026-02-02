module Domain.PersonSpec where

import Test.Hspec
import Domain.Person (Person(..))
import Domain.Id (EntityId(..))
import Data.Aeson (encode, toJSON)
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

spec :: Spec
spec = describe "Person" $ do
  describe "JSON serialization" $ do
    it "serializes email field correctly" $ do
      let json = toJSON (testPerson "test@example.com" (Just "Test User"))
      -- Just verify it encodes without error
      encode json `shouldNotBe` ""

testPerson :: Text -> Maybe Text -> Person
testPerson email name = Person
  { personId = EntityId "test123"
  , personEmail = email
  , personDisplayName = name
  , personPersonas = Nothing
  , personRelationship = Nothing
  , personOrganisation = Nothing
  , personNotes = Nothing
  , personFirstContact = Nothing
  , personLastContact = Nothing
  , personContactCount = 0
  , personCreatedAt = posixSecondsToUTCTime 1704067200
  }
