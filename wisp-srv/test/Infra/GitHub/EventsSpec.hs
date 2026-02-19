module Infra.GitHub.EventsSpec where

import Test.Hspec
import Infra.GitHub.Events
import Data.Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS

spec :: Spec
spec = describe "GitHub Events" $ do
  describe "GitHubEvent parsing" $ do
    it "parses PushEvent" $ do
      let json = LBS.pack $ unlines
            [ "{"
            , "  \"id\": \"12345678901\","
            , "  \"type\": \"PushEvent\","
            , "  \"actor\": { \"login\": \"garethstokes\" },"
            , "  \"repo\": { \"name\": \"org/repo\" },"
            , "  \"payload\": { \"commits\": [] },"
            , "  \"created_at\": \"2025-02-19T10:00:00Z\""
            , "}"
            ]
      case eitherDecode json :: Either String GitHubEvent of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right e -> do
          ghEventId e `shouldBe` "12345678901"
          ghEventType e `shouldBe` "PushEvent"
          ghEventRepo e `shouldBe` "org/repo"

    it "parses event list" $ do
      let json = LBS.pack "[{\"id\":\"1\",\"type\":\"PushEvent\",\"actor\":{\"login\":\"u\"},\"repo\":{\"name\":\"o/r\"},\"payload\":{},\"created_at\":\"2025-01-01T00:00:00Z\"}]"
      case eitherDecode json :: Either String [GitHubEvent] of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right events -> length events `shouldBe` 1

  describe "extractTitle" $ do
    it "generates title for PushEvent" $ do
      let json = LBS.pack "{\"id\":\"1\",\"type\":\"PushEvent\",\"actor\":{\"login\":\"u\"},\"repo\":{\"name\":\"org/repo\"},\"payload\":{},\"created_at\":\"2025-01-01T00:00:00Z\"}"
      case decode json :: Maybe GitHubEvent of
        Nothing -> expectationFailure "Failed to parse"
        Just e -> extractEventTitle e `shouldBe` "PushEvent to org/repo"
