module Http.Server
  ( startServer
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.Time (getCurrentTime)
import Web.Scotty.Trans (scottyT, middleware)
import Network.Wai (Middleware, rawPathInfo, requestMethod)
import qualified Data.ByteString.Char8 as BS
import App.Config (Config(..), ServerConfig(..))
import App.Monad (App, getConfig)
import Http.Routes (routes)

-- | Simple request logging middleware
requestLogger :: Middleware
requestLogger app req respond = do
  now <- getCurrentTime
  let method = BS.unpack $ requestMethod req
      path = BS.unpack $ rawPathInfo req
  putStrLn $ "[" <> show now <> "] " <> method <> " " <> path
  app req respond

startServer :: App ()
startServer = do
  env <- ask
  cfg <- getConfig
  let port = cfg.server.port
  liftIO $ putStrLn $ "Starting server on port " <> show port
  liftIO $ scottyT port (`runReaderT` env) $ do
    middleware requestLogger
    routes
