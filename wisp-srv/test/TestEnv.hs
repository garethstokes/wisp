module TestEnv
  ( testConfig
  , withTestEnv
  , runTestApp
  ) where

import Database.PostgreSQL.Simple
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import App.Config
import App.Monad
import Services.ClassificationQueue (newClassificationQueue)

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
      , workerCount = Just 1  -- Use 1 worker for tests
      }
  , claude = ClaudeConfig
      { apiKey = "test-api-key"
      , model = "claude-3-5-haiku-latest"
      }
  , github = Nothing
  , tavily = Nothing
  , notifications = Nothing
  }

-- Run an action with a test environment, rolling back after
withTestEnv :: (Env -> IO a) -> IO a
withTestEnv action = do
  conn <- connectPostgreSQL "postgres://localhost/wisp_test"
  _ <- execute_ conn "begin"
  -- Insert a test account so foreign key constraints are satisfied
  _ <- execute_ conn
    "INSERT INTO accounts (id, email, display_name) \
    \VALUES ('test-account', 'test@example.com', 'Test User') \
    \ON CONFLICT (id) DO NOTHING"
  lgr <- newStdoutLoggerSet defaultBufSize
  cq <- newClassificationQueue
  let env = Env
        { config = testConfig
        , dbConn = conn
        , logger = lgr
        , classificationQueue = cq
        }
  result <- action env
  _ <- execute_ conn "rollback"
  pure result

-- Run an App action in the test environment
runTestApp :: Env -> App a -> IO a
runTestApp env action = runApp env action
