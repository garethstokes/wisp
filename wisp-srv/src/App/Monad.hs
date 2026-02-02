module App.Monad
  ( Env(..)
  , App
  , runApp
  , getConfig
  , getConn
  , getLogger
  , getClassificationQueue
  ) where

import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Database.PostgreSQL.Simple (Connection)
import System.Log.FastLogger (LoggerSet)
import App.Config (Config)
import Services.ClassificationQueue (ClassificationQueue)

data Env = Env
  { config :: Config
  , dbConn :: Connection
  , logger :: LoggerSet
  , classificationQueue :: ClassificationQueue
  }

type App a = ReaderT Env IO a

runApp :: Env -> App a -> IO a
runApp = flip runReaderT

getConfig :: App Config
getConfig = asks (.config)

getConn :: App Connection
getConn = asks (.dbConn)

getLogger :: App LoggerSet
getLogger = asks (.logger)

getClassificationQueue :: App ClassificationQueue
getClassificationQueue = asks (.classificationQueue)
