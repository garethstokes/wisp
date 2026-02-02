module Services.ClassificationQueueSpec where

import Test.Hspec
import Control.Concurrent.Async (async, wait)
import qualified Data.Text as T
import Domain.Id (EntityId(..))
import Services.ClassificationQueue

spec :: Spec
spec = describe "ClassificationQueue" $ do
  describe "basic operations" $ do
    it "enqueues and dequeues in FIFO order" $ do
      queue <- newClassificationQueue
      let ids = map EntityId ["a", "b", "c"]
      enqueueActivities queue ids

      len <- queueLength queue
      len `shouldBe` 3

      id1 <- dequeueActivity queue
      id1 `shouldBe` EntityId "a"

      id2 <- dequeueActivity queue
      id2 `shouldBe` EntityId "b"

      len2 <- queueLength queue
      len2 `shouldBe` 1

    it "tracks queue length correctly with single enqueue" $ do
      queue <- newClassificationQueue
      enqueueActivity queue (EntityId "x")

      len <- queueLength queue
      len `shouldBe` 1

      _ <- dequeueActivity queue
      len2 <- queueLength queue
      len2 `shouldBe` 0

  describe "concurrent access" $ do
    it "handles concurrent enqueue and dequeue" $ do
      queue <- newClassificationQueue
      let ids = map (EntityId . T.pack . show) [1..100 :: Int]

      -- Enqueue all
      enqueueActivities queue ids

      -- Dequeue concurrently
      results <- mapM (\_ -> async $ dequeueActivity queue) [1..100 :: Int]
      dequeued <- mapM wait results

      -- All items should be dequeued
      length dequeued `shouldBe` 100

      -- Queue should be empty
      len <- queueLength queue
      len `shouldBe` 0
