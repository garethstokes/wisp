module Infra.GitHub.CommitExtractionSpec where

import Test.Hspec
import Infra.GitHub.Events (extractCommitsFromPayload, CommitInfo(..))
import Data.Aeson (object, (.=), Value)

spec :: Spec
spec = describe "extractCommitsFromPayload" $ do
  it "extracts commits from PushEvent payload" $ do
    let payload = object
          [ "commits" .=
            [ object
              [ "sha" .= ("abc123def456" :: String)
              , "message" .= ("feat: add new feature" :: String)
              , "author" .= object ["name" .= ("John Doe" :: String)]
              , "url" .= ("https://github.com/owner/repo/commit/abc123def456" :: String)
              ]
            , object
              [ "sha" .= ("789xyz012abc" :: String)
              , "message" .= ("fix: bug fix" :: String)
              , "author" .= object ["name" .= ("Jane Doe" :: String)]
              , "url" .= ("https://github.com/owner/repo/commit/789xyz012abc" :: String)
              ]
            ]
          ]

    let commits = extractCommitsFromPayload payload

    length commits `shouldBe` 2
    commitSha (head commits) `shouldBe` "abc123def456"
    commitMessage (head commits) `shouldBe` "feat: add new feature"
    commitAuthor (head commits) `shouldBe` "John Doe"

  it "returns empty list for missing commits array" $ do
    let payload = object []
    extractCommitsFromPayload payload `shouldBe` []
