module Infra.Google.AuthSpec where

import Test.Hspec
import Infra.Google.Auth (buildAuthUrl, OAuthConfig(..), UserInfo(..), userEmail, userName)
import Data.Aeson (decode)
import Data.Text (isInfixOf)

spec :: Spec
spec = describe "Google Auth" $ do
  describe "buildAuthUrl" $ do
    it "includes client_id in URL" $ do
      let cfg = OAuthConfig
            { oauthClientId = "test-client-id"
            , oauthClientSecret = "test-secret"
            , oauthRedirectUri = "http://localhost:8080/auth/google/callback"
            }
      let url = buildAuthUrl cfg
      "test-client-id" `isInfixOf` url `shouldBe` True

    it "includes required scopes" $ do
      let cfg = OAuthConfig
            { oauthClientId = "test-id"
            , oauthClientSecret = "test-secret"
            , oauthRedirectUri = "http://localhost:8080/auth/google/callback"
            }
      let url = buildAuthUrl cfg
      "gmail.readonly" `isInfixOf` url `shouldBe` True
      "calendar.readonly" `isInfixOf` url `shouldBe` True

  describe "UserInfo parsing" $ do
    it "parses userinfo response" $ do
      let json = "{\"email\":\"test@gmail.com\",\"name\":\"Test User\"}"
      case decode json :: Maybe UserInfo of
        Nothing -> expectationFailure "Failed to parse UserInfo"
        Just ui -> do
          userEmail ui `shouldBe` "test@gmail.com"
          userName ui `shouldBe` Just "Test User"
