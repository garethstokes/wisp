module Infra.Db.ActivitySpec where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Domain.Activity
import Domain.Id (EntityId(..))
import Infra.Db.Activity
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = around withTestEnv $ do

  describe "insertNote" $ do
    it "creates a note activity with tags" $ \env -> runTestApp env $ do
      let accountId = EntityId "test-account"
      let rawMeta = object ["origin" .= ("test" :: Text)]
      mActivityId <- insertNote accountId "Alice works at Google" ["Alice", "Work"] rawMeta
      case mActivityId of
        Just aid -> do
          mActivity <- getActivity aid
          case mActivity of
            Just activity -> do
              liftIO $ activitySource activity `shouldBe` Note
              liftIO $ activityTitle activity `shouldBe` Just "Alice works at Google"
              liftIO $ activityTags activity `shouldBe` ["alice", "work"]  -- normalized
            Nothing -> liftIO $ expectationFailure "Activity not found"
        Nothing -> liftIO $ expectationFailure "Note not created"

  describe "getActivitiesByTags" $ do
    it "returns notes matching any of the given tags" $ \env -> runTestApp env $ do
      let accountId = EntityId "test-account"
      let rawMeta = object []
      _ <- insertNote accountId "Alice info" ["alice", "family"] rawMeta
      _ <- insertNote accountId "SuperIT info" ["superit", "work"] rawMeta
      _ <- insertNote accountId "Alice at SuperIT" ["alice", "superit"] rawMeta

      aliceNotes <- getActivitiesByTags accountId ["alice"] 10
      liftIO $ length aliceNotes `shouldBe` 2

  describe "getAllTags" $ do
    it "returns unique tags across all notes" $ \env -> runTestApp env $ do
      let accountId = EntityId "test-account"
      let rawMeta = object []
      _ <- insertNote accountId "Note 1" ["alice", "work"] rawMeta
      _ <- insertNote accountId "Note 2" ["bob", "work"] rawMeta

      tags <- getAllTags accountId
      liftIO $ tags `shouldContain` ["alice"]
      liftIO $ tags `shouldContain` ["bob"]
      liftIO $ tags `shouldContain` ["work"]
