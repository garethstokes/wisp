module App.Env
  ( module App.Config
  , module App.Monad
  , buildEnv
  ) where

import Database.PostgreSQL.Simple (connectPostgreSQL)
import Data.Text.Encoding (encodeUtf8)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import App.Config
import App.Monad

buildEnv :: Config -> IO Env
buildEnv cfg = do
  conn <- connectPostgreSQL (encodeUtf8 cfg.database.url)
  lgr <- newStdoutLoggerSet defaultBufSize
  pure Env
    { config = cfg
    , dbConn = conn
    , logger = lgr
    }
