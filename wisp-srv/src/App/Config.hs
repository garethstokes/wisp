module App.Config
  ( Config(..)
  , ServerConfig(..)
  , DatabaseConfig(..)
  , GoogleConfig(..)
  , PollingConfig(..)
  , ClassificationConfig(..)
  , ClaudeConfig(..)
  , loadConfig
  ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)

data ServerConfig = ServerConfig
  { host :: Text
  , port :: Int
  } deriving (Generic, Show)

instance FromJSON ServerConfig

data DatabaseConfig = DatabaseConfig
  { url :: Text
  } deriving (Generic, Show)

instance FromJSON DatabaseConfig

data GoogleConfig = GoogleConfig
  { clientId :: Text
  , clientSecret :: Text
  } deriving (Generic)

instance FromJSON GoogleConfig

instance Show GoogleConfig where
  show c = "GoogleConfig {clientId = " <> show (clientId c) <> ", clientSecret = \"<redacted>\"}"

data PollingConfig = PollingConfig
  { intervalMinutes :: Int
  } deriving (Generic, Show)

instance FromJSON PollingConfig

data ClassificationConfig = ClassificationConfig
  { confidenceThreshold :: Double
  , workerCount :: Maybe Int  -- Number of classification workers (Nothing = auto-detect CPU count)
  } deriving (Generic, Show)

instance FromJSON ClassificationConfig

data ClaudeConfig = ClaudeConfig
  { apiKey :: Text
  , model :: Text
  } deriving (Generic)

instance FromJSON ClaudeConfig

instance Show ClaudeConfig where
  show c = "ClaudeConfig {apiKey = \"<redacted>\", model = " <> show (model c) <> "}"

data Config = Config
  { server :: ServerConfig
  , database :: DatabaseConfig
  , google :: GoogleConfig
  , polling :: PollingConfig
  , classification :: ClassificationConfig
  , claude :: ClaudeConfig
  } deriving (Generic, Show)

instance FromJSON Config

loadConfig :: FilePath -> IO Config
loadConfig path = do
  result <- decodeFileEither path
  case result of
    Left err -> error $ "Config error: " <> show err
    Right cfg -> do
      mDbUrl <- lookupEnv "DATABASE_URL"
      mPort <- lookupEnv "PORT"
      mGoogleClientId <- lookupEnv "GOOGLE_CLIENT_ID"
      mGoogleClientSecret <- lookupEnv "GOOGLE_CLIENT_SECRET"
      mAnthropicApiKey <- lookupEnv "ANTHROPIC_API_KEY"
      pure cfg
        { database = cfg.database
            { url = maybe cfg.database.url T.pack mDbUrl
            }
        , server = cfg.server
            { port = maybe cfg.server.port read mPort
            }
        , google = cfg.google
            { clientId = maybe cfg.google.clientId T.pack mGoogleClientId
            , clientSecret = maybe cfg.google.clientSecret T.pack mGoogleClientSecret
            }
        , claude = cfg.claude
            { apiKey = maybe cfg.claude.apiKey T.pack mAnthropicApiKey
            }
        }
