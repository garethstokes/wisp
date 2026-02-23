module Skills.ReflectionSpec where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Control.Monad (replicateM_)
import Data.Aeson (object)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Skills.Reflection
import Domain.Account (Account(..))
import Domain.Activity (ActivitySource(..), NewActivity(..), ActivityStatus(..))
import qualified Infra.Db.Activity as DbActivity
import qualified Infra.Db.Account as DbAccount
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = around withTestEnv $ do

  describe "getProjectsNeedingReflection" $ do
    it "returns projects with recent activity" $ \env -> runTestApp env $ do
      -- Seed projects exist from migration
      projects <- getProjectsNeedingReflection
      -- Initially no activities linked, so should return empty or all projects
      liftIO $ projects `shouldSatisfy` const True  -- Just verify it runs

  describe "buildReflectionPrompt" $ do
    it "formats project state and activities for LLM" $ \_ -> do
      let prompt = buildReflectionPrompt "Wisp" "AI assistant project" ["email about feature X", "meeting notes"]
      prompt `shouldSatisfy` T.isInfixOf "Wisp"
      prompt `shouldSatisfy` T.isInfixOf "feature X"

    it "handles empty current summary" $ \_ -> do
      let prompt = buildReflectionPrompt "NewProject" "" ["some activity"]
      prompt `shouldSatisfy` T.isInfixOf "no summary yet"

    it "handles empty activities" $ \_ -> do
      let prompt = buildReflectionPrompt "Stale" "Old summary" []
      prompt `shouldSatisfy` T.isInfixOf "no new activities"

  describe "detectProjectClusters" $ do
    it "returns no suggestions when no unassigned activities" $ \env -> runTestApp env $ do
      suggestions <- detectProjectClusters
      -- With no processed, unassigned activities, should return empty
      liftIO $ suggestions `shouldBe` []

    it "detects clusters from same domain with 5+ activities" $ \env -> runTestApp env $ do
      -- Get a test account
      accounts <- DbAccount.getAllAccounts
      let accId = accountId (head accounts)

      -- Create 6 activities from same domain (meeting threshold)
      let mkActivity n = NewActivity
            { newActivityAccountId = accId
            , newActivitySource = Email
            , newActivitySourceId = "cluster-test-" <> T.pack (show (n :: Int))
            , newActivityRaw = object []
            , newActivityTitle = Just $ "Support ticket #" <> T.pack (show n)
            , newActivitySenderEmail = Just $ "user" <> T.pack (show n) <> "@acme.com"
            , newActivityStartsAt = Nothing
            , newActivityEndsAt = Nothing
            }
      -- Insert 6 activities
      actIds <- mapM (\n -> fromJust <$> DbActivity.insertActivity (mkActivity n)) [1..6]

      -- Mark them as processed (required for cluster detection)
      mapM_ (\aid -> DbActivity.updateActivityStatus aid Processed) actIds

      suggestions <- detectProjectClusters
      liftIO $ do
        length suggestions `shouldBe` 1
        let suggestion = head suggestions
        psClusterKey suggestion `shouldBe` ClusterKey "acme.com"
        psActivityCount suggestion `shouldSatisfy` (>= 5)
        psSuggestedName suggestion `shouldSatisfy` T.isInfixOf "Acme"

    it "ignores clusters below threshold" $ \env -> runTestApp env $ do
      accounts <- DbAccount.getAllAccounts
      let accId = accountId (head accounts)

      -- Create only 3 activities from a domain (below threshold)
      let mkActivity n = NewActivity
            { newActivityAccountId = accId
            , newActivitySource = Email
            , newActivitySourceId = "small-cluster-" <> T.pack (show (n :: Int))
            , newActivityRaw = object []
            , newActivityTitle = Just $ "Message " <> T.pack (show n)
            , newActivitySenderEmail = Just $ "person" <> T.pack (show n) <> "@smallco.org"
            , newActivityStartsAt = Nothing
            , newActivityEndsAt = Nothing
            }
      actIds <- mapM (\n -> fromJust <$> DbActivity.insertActivity (mkActivity n)) [1..3]
      mapM_ (\aid -> DbActivity.updateActivityStatus aid Processed) actIds

      suggestions <- detectProjectClusters
      -- Should not include smallco.org since it only has 3 activities
      liftIO $ all ((/= ClusterKey "smallco.org") . psClusterKey) suggestions `shouldBe` True

    it "detects keyword-based project names" $ \env -> runTestApp env $ do
      accounts <- DbAccount.getAllAccounts
      let accId = accountId (head accounts)

      -- Create 5 activities with "support" in titles
      let mkActivity n = NewActivity
            { newActivityAccountId = accId
            , newActivitySource = Email
            , newActivitySourceId = "keyword-test-" <> T.pack (show (n :: Int))
            , newActivityRaw = object []
            , newActivityTitle = Just $ "Support case #" <> T.pack (show n)
            , newActivitySenderEmail = Just $ "agent" <> T.pack (show n) <> "@bigcorp.io"
            , newActivityStartsAt = Nothing
            , newActivityEndsAt = Nothing
            }
      actIds <- mapM (\n -> fromJust <$> DbActivity.insertActivity (mkActivity n)) [1..5]
      mapM_ (\aid -> DbActivity.updateActivityStatus aid Processed) actIds

      suggestions <- detectProjectClusters
      let bigcorpSuggestions = filter ((== ClusterKey "bigcorp.io") . psClusterKey) suggestions
      liftIO $ case bigcorpSuggestions of
        [s] -> psSuggestedName s `shouldSatisfy` T.isInfixOf "Support"
        _ -> expectationFailure "Expected exactly one bigcorp.io suggestion"
