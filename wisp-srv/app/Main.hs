module Main where

import System.Environment (getArgs)
import System.FilePath (takeDirectory, (</>))
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
      configDir = takeDirectory configPath
      migrationsPath = if null configDir
                       then "migrations"
                       else configDir </> "migrations"

  putStrLn "Loading configuration..."
  config <- loadConfig configPath

  putStrLn "Building environment..."
  env <- buildEnv config

  putStrLn "Running migrations..."
  runApp env $ runMigrations migrationsPath

  putStrLn "Starting wisp-srv..."
  runApp env startServer
