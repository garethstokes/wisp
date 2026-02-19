module Domain.TenantSpec where

import Test.Hspec
import Domain.Tenant
import Data.Aeson (encode, decode)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.UUID as UUID

spec :: Spec
spec = describe "Tenant" $ do
  describe "TenantId" $ do
    it "serializes to JSON as UUID string" $ do
      let Just uuid = UUID.fromString "550e8400-e29b-41d4-a716-446655440000"
          tid = TenantId uuid
          json = LBS.unpack (encode tid)
      json `shouldBe` "\"550e8400-e29b-41d4-a716-446655440000\""

    it "deserializes from JSON UUID string" $ do
      let json = "\"550e8400-e29b-41d4-a716-446655440000\""
          Just tid = decode json :: Maybe TenantId
          Just expectedUuid = UUID.fromString "550e8400-e29b-41d4-a716-446655440000"
      tid `shouldBe` TenantId expectedUuid

    it "fails to deserialize invalid UUID" $ do
      let json = "\"not-a-uuid\""
          result = decode json :: Maybe TenantId
      result `shouldBe` Nothing

  describe "Tenant JSON serialization" $ do
    it "encodes Tenant with expected keys" $ do
      let Just uuid = UUID.fromString "550e8400-e29b-41d4-a716-446655440000"
          testTime = posixSecondsToUTCTime 1704067200
          tenant = Tenant
            { tenantId = TenantId uuid
            , tenantName = "My Workspace"
            , tenantCreatedAt = testTime
            }
          json = LBS.unpack (encode tenant)
      json `shouldContain` "\"id\""
      json `shouldContain` "\"name\""
      json `shouldContain` "\"created_at\""
      json `shouldContain` "\"My Workspace\""
