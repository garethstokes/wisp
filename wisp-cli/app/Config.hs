module Config
  ( CliConfig(..)
  , loadCliConfig
  , getConfigPath
  ) where

import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither)
import System.Directory (doesFileExist, getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>))

data CliConfig = CliConfig
  { timezone :: Text  -- IANA timezone name like "Europe/London"
  } deriving (Show)

instance FromJSON CliConfig where
  parseJSON = withObject "CliConfig" $ \v -> CliConfig
    <$> v .: "timezone"

-- Get the config file path (~/.config/wisp/cli.yaml)
getConfigPath :: IO FilePath
getConfigPath = do
  configDir <- getXdgDirectory XdgConfig "wisp"
  pure $ configDir </> "cli.yaml"

-- Load CLI config with explicit error messages
loadCliConfig :: IO CliConfig
loadCliConfig = do
  path <- getConfigPath
  exists <- doesFileExist path
  if not exists
    then error $ unlines
      [ "CLI config not found. Create " <> path <> " with:"
      , ""
      , "  timezone: \"Europe/London\""
      , ""
      , "Use your IANA timezone (e.g., America/New_York, Asia/Tokyo)"
      ]
    else do
      result <- decodeFileEither path
      case result of
        Left err -> error $ "Failed to parse " <> path <> ": " <> show err
        Right cfg -> do
          -- Validate timezone is not empty
          if T.null (timezone cfg)
            then error $ "timezone not set in " <> path
            else pure cfg
