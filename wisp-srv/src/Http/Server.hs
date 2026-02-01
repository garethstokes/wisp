module Http.Server
  ( startServer
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Web.Scotty.Trans (scottyT)
import App.Config (Config(..), ServerConfig(..))
import App.Monad (App, getConfig)
import Http.Routes (routes)

startServer :: App ()
startServer = do
  env <- ask
  cfg <- getConfig
  let port = cfg.server.port
  liftIO $ putStrLn $ "Starting server on port " <> show port
  liftIO $ scottyT port (`runReaderT` env) routes
