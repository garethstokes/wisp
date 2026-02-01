module Main where

import System.Environment (getArgs)
import App.Config (loadConfig)
import App.Env (buildEnv)
import App.Monad (runApp)
import Http.Server (startServer)
import Infra.Db.Migrations (runMigrations)

main :: IO ()
main = do
  args <- getArgs
  let configPath = case args of
        [p] -> p
        _ -> "wisp.yaml"

  putStrLn "Loading configuration..."
  config <- loadConfig configPath

  putStrLn "Building environment..."
  env <- buildEnv config

  putStrLn "Running migrations..."
  runApp env $ runMigrations "migrations"

  putStrLn "Starting wisp-srv..."
  runApp env startServer
