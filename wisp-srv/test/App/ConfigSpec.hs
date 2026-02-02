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
