module App.Config
  ( Config(..)
  , ServerConfig(..)
  , DatabaseConfig(..)
  , GoogleConfig(..)
  , GitHubConfig(..)
  , TavilyConfig(..)
  , PollingConfig(..)
  , ClassificationConfig(..)
  , ClaudeConfig(..)
  , NotificationConfig(..)
  , loadConfig
  ) where

import Data.Aeson (FromJSON(..), withObject, (.:?), (.!=))
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
  show c = "GoogleConfig {clientId = " <> show (c.clientId) <> ", clientSecret = \"<redacted>\"}"

data GitHubConfig = GitHubConfig
  { clientId :: Text
  , clientSecret :: Text
  } deriving (Generic)

instance FromJSON GitHubConfig

instance Show GitHubConfig where
  show c = "GitHubConfig {clientId = " <> show (c.clientId) <> ", clientSecret = \"<redacted>\"}"

data TavilyConfig = TavilyConfig
  { apiKey :: Text
  } deriving (Generic)

instance FromJSON TavilyConfig

instance Show TavilyConfig where
  show _ = "TavilyConfig {apiKey = \"<redacted>\"}"

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

data NotificationConfig = NotificationConfig
  { enabled :: Bool
  , defaultIntervalHours :: Int
  , urgentIntervalHours :: Int
  , urgentThresholdCount :: Int
  , quietHoursStart :: Text
  , quietHoursEnd :: Text
  , vipEmails :: [Text]
  } deriving (Generic, Show)

instance FromJSON NotificationConfig where
  parseJSON = withObject "NotificationConfig" $ \v -> NotificationConfig
    <$> v .:? "enabled" .!= False
    <*> v .:? "default_interval_hours" .!= 4
    <*> v .:? "urgent_interval_hours" .!= 2
    <*> v .:? "urgent_threshold_count" .!= 3
    <*> v .:? "quiet_hours_start" .!= "22:00"
    <*> v .:? "quiet_hours_end" .!= "08:00"
    <*> v .:? "vip_emails" .!= []

data Config = Config
  { server :: ServerConfig
  , database :: DatabaseConfig
  , google :: GoogleConfig
  , github :: Maybe GitHubConfig
  , tavily :: Maybe TavilyConfig
  , polling :: PollingConfig
  , classification :: ClassificationConfig
  , claude :: ClaudeConfig
  , notifications :: Maybe NotificationConfig
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
      mGitHubClientId <- lookupEnv "GITHUB_CLIENT_ID"
      mGitHubClientSecret <- lookupEnv "GITHUB_CLIENT_SECRET"
      mAnthropicApiKey <- lookupEnv "ANTHROPIC_API_KEY"
      mTavilyApiKey <- lookupEnv "TAVILY_API_KEY"

      let githubConfig = case (mGitHubClientId, mGitHubClientSecret) of
            (Just cid, Just cs) -> Just $ GitHubConfig (T.pack cid) (T.pack cs)
            _ -> cfg.github

      let tavilyConfig = case mTavilyApiKey of
            Just key -> Just $ TavilyConfig (T.pack key)
            Nothing -> cfg.tavily

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
        , github = githubConfig
        , tavily = tavilyConfig
        , claude = cfg.claude
            { apiKey = maybe cfg.claude.apiKey T.pack mAnthropicApiKey
            }
        }
