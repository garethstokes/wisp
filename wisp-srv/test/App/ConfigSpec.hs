module App.ConfigSpec where

import Test.Hspec
import App.Config
import Data.Yaml (decodeEither')
import qualified Data.ByteString.Char8 as BS

spec :: Spec
spec = describe "Config" $ do
  it "parses from YAML" $ do
    let yaml = BS.unlines
          [ "server:"
          , "  host: 127.0.0.1"
          , "  port: 8080"
          , "database:"
          , "  url: postgres://localhost/wisp"
          , "google:"
          , "  clientId: test-id"
          , "  clientSecret: test-secret"
          , "polling:"
          , "  intervalMinutes: 5"
          , "classification:"
          , "  confidenceThreshold: 0.5"
          , "claude:"
          , "  apiKey: test-api-key"
          , "  model: claude-sonnet-4-20250514"
          ]
    case decodeEither' yaml of
      Left err -> expectationFailure $ show err
      Right (cfg :: Config) -> do
        cfg.server.host `shouldBe` "127.0.0.1"
        cfg.server.port `shouldBe` 8080
        cfg.database.url `shouldBe` "postgres://localhost/wisp"

  it "parses notification config" $ do
    let yaml = BS.unlines
          [ "enabled: true"
          , "default_interval_hours: 4"
          , "urgent_interval_hours: 2"
          , "urgent_threshold_count: 3"
          , "quiet_hours_start: \"22:00\""
          , "quiet_hours_end: \"08:00\""
          , "vip_emails:"
          , "  - \"vip@example.com\""
          ]
    case decodeEither' yaml of
      Left err -> expectationFailure $ show err
      Right (cfg :: NotificationConfig) -> do
        cfg.enabled `shouldBe` True
        cfg.defaultIntervalHours `shouldBe` 4
        cfg.urgentIntervalHours `shouldBe` 2
        cfg.urgentThresholdCount `shouldBe` 3
        cfg.quietHoursStart `shouldBe` "22:00"
        cfg.quietHoursEnd `shouldBe` "08:00"
        cfg.vipEmails `shouldBe` ["vip@example.com"]
