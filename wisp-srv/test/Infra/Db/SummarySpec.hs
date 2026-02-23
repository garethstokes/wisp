module Infra.Db.SummarySpec (spec) where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Domain.Summary (Summary(..), SummaryId(..))
import Infra.Db.Summary
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = describe "Infra.Db.Summary" $ around withTestEnv $ do
  describe "insertSummary" $ do
    it "creates a summary for sessions" $ \env -> runTestApp env $ do
      summary <- insertSummary "wisp" ["sess-1", "sess-2"] "Discussed inbox management"
      liftIO $ summaryAgentId summary `shouldBe` "wisp"
      liftIO $ summaryContent summary `shouldBe` "Discussed inbox management"
      liftIO $ summarySessionIds summary `shouldBe` ["sess-1", "sess-2"]

  describe "getRecentSummaries" $ do
    it "returns summaries for agent" $ \env -> runTestApp env $ do
      _ <- insertSummary "wisp" ["sess-1"] "Calendar discussion"
      summaries <- getRecentSummaries "wisp" 10
      liftIO $ length summaries `shouldBe` 1
