module Domain.AccountSpec where

import Test.Hspec
import Domain.Account
import Domain.Id (EntityId(..))
import Data.Aeson (encode)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.ByteString.Lazy.Char8 as LBS

spec :: Spec
spec = describe "Account" $ do
  describe "JSON serialization" $ do
    it "encodes Account with snake_case keys" $ do
      let testTime = posixSecondsToUTCTime 1704067200  -- 2024-01-01 00:00:00 UTC
          account = Account
            { accountId = EntityId "abc123"
            , accountEmail = "test@example.com"
            , accountDisplayName = Just "Test User"
            , accountCreatedAt = testTime
            }
          json = LBS.unpack (encode account)
      -- Verify snake_case keys are present
      json `shouldContain` "\"id\""
      json `shouldContain` "\"email\""
      json `shouldContain` "\"display_name\""
      json `shouldContain` "\"created_at\""
      -- Verify values are present
      json `shouldContain` "\"abc123\""
      json `shouldContain` "\"test@example.com\""
      json `shouldContain` "\"Test User\""

    it "encodes Account with null display_name when Nothing" $ do
      let testTime = posixSecondsToUTCTime 1704067200
          account = Account
            { accountId = EntityId "xyz789"
            , accountEmail = "nobody@example.com"
            , accountDisplayName = Nothing
            , accountCreatedAt = testTime
            }
          json = LBS.unpack (encode account)
      json `shouldContain` "\"display_name\":null"
