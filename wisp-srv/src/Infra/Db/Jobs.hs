module Infra.Db.Jobs
  ( Job(..)
  , enqueue
  , enqueueDelayed
  , claimJob
  , completeJob
  , failJob
  , requeueJob
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, Value, toJSON, decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Simple
import Domain.Id (EntityId(..), newEntityId)
import App.Monad (App, getConn)

data Job = Job
  { jobId :: EntityId
  , jobPayload :: Value
  } deriving (Show)

-- Enqueue a new job
enqueue :: (ToJSON a) => Text -> a -> App EntityId
enqueue jobType payload = do
  conn <- getConn
  jid <- liftIO newEntityId
  _ <- liftIO $ execute conn
    "insert into jobs (id, job_type, payload) values (?, ?, ?)"
    (unEntityId jid, jobType, toJSON payload)
  pure jid

-- Enqueue with delay
enqueueDelayed :: (ToJSON a) => Text -> a -> NominalDiffTime -> App EntityId
enqueueDelayed jobType payload delay = do
  conn <- getConn
  jid <- liftIO newEntityId
  _ <- liftIO $ execute conn
    "insert into jobs (id, job_type, payload, run_after) \
    \values (?, ?, ?, now() + ?)"
    (unEntityId jid, jobType, toJSON payload, delay)
  pure jid

-- Claim next available job (FOR UPDATE SKIP LOCKED)
claimJob :: Text -> App (Maybe Job)
claimJob jobType = do
  conn <- getConn
  liftIO $ withTransaction conn $ do
    results <- query conn
      "select id, payload from jobs \
      \where job_type = ? \
      \  and status = 'queued' \
      \  and (run_after is null or run_after <= now()) \
      \order by created_at \
      \limit 1 \
      \for update skip locked"
      (Only jobType)
    case results of
      [(jid, payload)] -> do
        _ <- execute conn
          "update jobs set status = 'claimed', claimed_at = now(), \
          \attempts = attempts + 1 where id = ?"
          (Only (jid :: Text))
        pure $ case decode (fromStrict payload) of
          Just p -> Just (Job (EntityId jid) p)
          Nothing -> Nothing
      _ -> pure Nothing

-- Mark job complete
completeJob :: EntityId -> App ()
completeJob jid = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "update jobs set status = 'completed' where id = ?"
    (Only $ unEntityId jid)
  pure ()

-- Mark job failed
failJob :: EntityId -> Text -> App ()
failJob jid err = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "update jobs set status = 'failed', last_error = ? where id = ?"
    (err, unEntityId jid)
  pure ()

-- Requeue failed job (if under max attempts)
requeueJob :: EntityId -> App Bool
requeueJob jid = do
  conn <- getConn
  n <- liftIO $ execute conn
    "update jobs set status = 'queued', claimed_at = null \
    \where id = ? and attempts < max_attempts"
    (Only $ unEntityId jid)
  pure (n > 0)
