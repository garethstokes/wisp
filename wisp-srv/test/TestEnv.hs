module TestEnv
  ( testConfig
  , withTestEnv
  ) where

import Database.PostgreSQL.Simple
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import App.Config
import App.Monad

testConfig :: Config
testConfig = Config
  { server = ServerConfig
      { host = "127.0.0.1"
      , port = 8080
      }
  , database = DatabaseConfig
      { url = "postgres://localhost/wisp_test"
      }
  , google = GoogleConfig
      { clientId = "test-client-id"
      , clientSecret = "test-client-secret"
      }
  , polling = PollingConfig
      { intervalMinutes = 5
      }
  , classification = ClassificationConfig
      { confidenceThreshold = 0.5
      }
  }

-- Run an action with a test environment, rolling back after
withTestEnv :: (Env -> IO a) -> IO a
withTestEnv action = do
  conn <- connectPostgreSQL "postgres://localhost/wisp_test"
  _ <- execute_ conn "begin"
  lgr <- newStdoutLoggerSet defaultBufSize
  let env = Env
        { config = testConfig
        , dbConn = conn
        , logger = lgr
        }
  result <- action env
  _ <- execute_ conn "rollback"
  pure result
