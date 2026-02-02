-- src/Services/ClassificationQueue.hs
module Services.ClassificationQueue
  ( ClassificationQueue
  , newClassificationQueue
  , enqueueActivity
  , enqueueActivities
  , dequeueActivity
  , queueLength
  ) where

import Control.Concurrent.STM (TQueue, TVar, newTQueueIO, newTVarIO, atomically, writeTQueue, readTQueue, modifyTVar', readTVar)
import Domain.Id (EntityId)

-- | Queue of activity IDs waiting to be classified, with a count for monitoring
data ClassificationQueue = ClassificationQueue
  { queue :: TQueue EntityId
  , queueSize :: TVar Int
  }

-- | Create a new empty classification queue
newClassificationQueue :: IO ClassificationQueue
newClassificationQueue = ClassificationQueue <$> newTQueueIO <*> newTVarIO 0

-- | Add a single activity to the classification queue
enqueueActivity :: ClassificationQueue -> EntityId -> IO ()
enqueueActivity cq aid = atomically $ do
  writeTQueue (queue cq) aid
  modifyTVar' (queueSize cq) (+1)

-- | Add multiple activities to the classification queue
enqueueActivities :: ClassificationQueue -> [EntityId] -> IO ()
enqueueActivities cq aids = atomically $ do
  mapM_ (writeTQueue (queue cq)) aids
  modifyTVar' (queueSize cq) (+ length aids)

-- | Take the next activity from the queue (blocks if empty)
dequeueActivity :: ClassificationQueue -> IO EntityId
dequeueActivity cq = atomically $ do
  aid <- readTQueue (queue cq)
  modifyTVar' (queueSize cq) (subtract 1)
  pure aid

-- | Get current queue length (for monitoring/status)
queueLength :: ClassificationQueue -> IO Int
queueLength cq = atomically $ readTVar (queueSize cq)
