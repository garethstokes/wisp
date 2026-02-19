module Domain.AccountSpec where

import Test.Hspec
import Domain.Account
import Domain.Id (EntityId(..))
import Data.Aeson (encode, decode, object, (.=))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.ByteString.Lazy.Char8 as LBS

spec :: Spec
spec = describe "Account" $ do
  describe "AccountProvider" $ do
    it "serializes Google provider to JSON" $ do
      let json = LBS.unpack (encode Google)
      json `shouldBe` "\"google\""

    it "serializes GitHub provider to JSON" $ do
      let json = LBS.unpack (encode GitHub)
      json `shouldBe` "\"github\""

    it "parses provider from JSON" $ do
      decode "\"google\"" `shouldBe` Just Google
      decode "\"github\"" `shouldBe` Just GitHub

  describe "JSON serialization" $ do
    it "encodes Account with provider and details" $ do
      let testTime = posixSecondsToUTCTime 1704067200
          details = object ["email" .= ("test@example.com" :: String)]
          account = Account
            { accountId = EntityId "abc123"
            , accountProvider = Google
            , accountDisplayName = Just "Test User"
            , accountDetails = details
            , accountCreatedAt = testTime
            }
          json = LBS.unpack (encode account)
      json `shouldContain` "\"provider\":\"google\""
      json `shouldContain` "\"details\""
      json `shouldContain` "\"test@example.com\""

    it "encodes GitHub account with username in details" $ do
      let testTime = posixSecondsToUTCTime 1704067200
          details = object ["username" .= ("garethstokes" :: String)]
          account = Account
            { accountId = EntityId "xyz789"
            , accountProvider = GitHub
            , accountDisplayName = Just "Gareth"
            , accountDetails = details
            , accountCreatedAt = testTime
            }
          json = LBS.unpack (encode account)
      json `shouldContain` "\"provider\":\"github\""
      json `shouldContain` "\"garethstokes\""
