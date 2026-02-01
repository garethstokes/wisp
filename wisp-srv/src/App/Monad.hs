module App.Monad
  ( Env(..)
  , App
  , runApp
  , getConfig
  , getConn
  , getLogger
  ) where

import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Database.PostgreSQL.Simple (Connection)
import System.Log.FastLogger (LoggerSet)
import App.Config (Config)

data Env = Env
  { config :: Config
  , dbConn :: Connection
  , logger :: LoggerSet
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
