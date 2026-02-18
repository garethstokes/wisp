module Infra.Db.SoulSpec (spec) where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Domain.Soul (Soul(..))
import Infra.Db.Soul
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = describe "Infra.Db.Soul" $ around withTestEnv $ do
  describe "getOrCreateSoul" $ do
    it "creates empty soul if none exists" $ \env -> runTestApp env $ do
      soul <- getOrCreateSoul "wisp/concierge"
      liftIO $ soulAgentId soul `shouldBe` "wisp/concierge"
      liftIO $ soulPersonality soul `shouldBe` ""
      liftIO $ soulInsights soul `shouldBe` []

    it "returns existing soul" $ \env -> runTestApp env $ do
      _ <- getOrCreateSoul "wisp/concierge"
      _ <- updateSoulPersonality "wisp/concierge" "Formal tone"
      soul <- getOrCreateSoul "wisp/concierge"
      liftIO $ soulPersonality soul `shouldBe` "Formal tone"

  describe "addInsight" $ do
    it "appends insight to soul" $ \env -> runTestApp env $ do
      _ <- getOrCreateSoul "wisp/scheduler"
      addInsight "wisp/scheduler" "Prefers mornings"
      soul <- getOrCreateSoul "wisp/scheduler"
      liftIO $ soulInsights soul `shouldContain` ["Prefers mornings"]
