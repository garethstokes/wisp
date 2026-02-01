module Infra.Db.Migrations
  ( runMigrations
  , getMigrationFiles
  , parseMigrationNumber
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(..))
import System.Directory (listDirectory)
import System.FilePath ((</>), takeFileName)
import Text.Read (readMaybe)
import App.Monad (App, getConn)

-- Parse migration number from filename like "001_entities.sql"
parseMigrationNumber :: String -> Maybe Int
parseMigrationNumber name =
  case T.splitOn "_" (T.pack name) of
    (numPart : _) -> readMaybe (T.unpack numPart)
    _ -> Nothing

-- Get all migration files sorted by number
getMigrationFiles :: FilePath -> IO [(Int, FilePath)]
getMigrationFiles dir = do
  files <- listDirectory dir
  let sqlFiles = filter (\f -> ".sql" `T.isSuffixOf` T.pack f) files
  let numbered = mapMaybe (\f -> (,dir </> f) <$> parseMigrationNumber f) sqlFiles
  pure $ sortOn fst numbered

-- Track applied migrations
ensureMigrationsTable :: Connection -> IO ()
ensureMigrationsTable conn = do
  _ <- execute_ conn
    "create table if not exists schema_migrations (\
    \  version int primary key,\
    \  applied_at timestamptz not null default now()\
    \)"
  pure ()

getAppliedMigrations :: Connection -> IO [Int]
getAppliedMigrations conn = do
  results <- query_ conn "select version from schema_migrations order by version"
  pure $ map fromOnly results

markMigrationApplied :: Connection -> Int -> IO ()
markMigrationApplied conn version = do
  _ <- execute conn "insert into schema_migrations (version) values (?)" (Only version)
  pure ()

-- Run all pending migrations
runMigrations :: FilePath -> App ()
runMigrations dir = do
  conn <- getConn
  liftIO $ do
    ensureMigrationsTable conn
    applied <- getAppliedMigrations conn
    migrations <- getMigrationFiles dir
    let pending = filter (\(v, _) -> v `notElem` applied) migrations
    forM_ pending $ \(version, path) -> do
      putStrLn $ "Running migration " <> show version <> ": " <> takeFileName path
      sql <- TIO.readFile path
      withTransaction conn $ do
        _ <- execute_ conn (Query $ TE.encodeUtf8 sql)
        markMigrationApplied conn version
      putStrLn $ "  Applied migration " <> show version
