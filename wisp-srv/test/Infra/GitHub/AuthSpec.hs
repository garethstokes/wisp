module Infra.GitHub.AuthSpec where

import Test.Hspec
import Infra.GitHub.Auth
import Data.Aeson (decode)
import Data.Text (isInfixOf)

spec :: Spec
spec = describe "GitHub Auth" $ do
  describe "buildGitHubAuthUrl" $ do
    it "includes client_id in URL" $ do
      let cfg = GitHubOAuthConfig
            { ghClientId = "test-client-id"
            , ghClientSecret = "test-secret"
            , ghRedirectUri = "http://localhost:8080/auth/github/callback"
            }
      let url = buildGitHubAuthUrl cfg
      "test-client-id" `isInfixOf` url `shouldBe` True

    it "includes read:user scope" $ do
      let cfg = GitHubOAuthConfig
            { ghClientId = "test-id"
            , ghClientSecret = "test-secret"
            , ghRedirectUri = "http://localhost:8080/auth/github/callback"
            }
      let url = buildGitHubAuthUrl cfg
      "read%3Auser" `isInfixOf` url `shouldBe` True

  describe "GitHubUser parsing" $ do
    it "parses user response" $ do
      let json = "{\"login\":\"garethstokes\",\"name\":\"Gareth Stokes\",\"avatar_url\":\"https://example.com/avatar.png\"}"
      case decode json :: Maybe GitHubUser of
        Nothing -> expectationFailure "Failed to parse GitHubUser"
        Just u -> do
          ghUserLogin u `shouldBe` "garethstokes"
          ghUserName u `shouldBe` Just "Gareth Stokes"
