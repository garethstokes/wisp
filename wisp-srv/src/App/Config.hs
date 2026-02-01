module App.Config
  ( Config(..)
  , ServerConfig(..)
  , DatabaseConfig(..)
  , GoogleConfig(..)
  , PollingConfig(..)
  , ClassificationConfig(..)
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
  } deriving (Generic, Show)

instance FromJSON GoogleConfig

data PollingConfig = PollingConfig
  { intervalMinutes :: Int
  } deriving (Generic, Show)

instance FromJSON PollingConfig

data ClassificationConfig = ClassificationConfig
  { confidenceThreshold :: Double
  } deriving (Generic, Show)

instance FromJSON ClassificationConfig

data Config = Config
  { server :: ServerConfig
  , database :: DatabaseConfig
  , google :: GoogleConfig
  , polling :: PollingConfig
  , classification :: ClassificationConfig
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
      pure cfg
        { database = cfg.database
            { url = maybe cfg.database.url T.pack mDbUrl
            }
        , server = cfg.server
            { port = maybe cfg.server.port read mPort
            }
        }
