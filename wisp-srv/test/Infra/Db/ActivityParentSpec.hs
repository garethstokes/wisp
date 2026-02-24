module Infra.Db.ActivityParentSpec where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object)
import Domain.Activity (NewActivity(..), ActivitySource(..), Activity(..))
import Domain.Id (EntityId(..))
import Infra.Db.Activity (insertActivity, insertActivityWithParent, getActivity)
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = around withTestEnv $ do

  describe "Activity parent_id support" $ do
    it "inserts activity with parent_id and retrieves it" $ \env -> runTestApp env $ do
      -- First insert parent
      let parent = NewActivity
            { newActivityAccountId = EntityId "test-account"
            , newActivitySource = GitHubEvent
            , newActivitySourceId = "parent-event-123"
            , newActivityRaw = object []
            , newActivityTitle = Just "PushEvent to owner/repo"
            , newActivitySenderEmail = Nothing
            , newActivityStartsAt = Nothing
            , newActivityEndsAt = Nothing
            }
      mParentId <- insertActivity parent

      case mParentId of
        Nothing -> liftIO $ expectationFailure "Failed to insert parent"
        Just pid -> do
          -- Insert child with parent_id
          let child = NewActivity
                { newActivityAccountId = EntityId "test-account"
                , newActivitySource = GitHubEvent
                , newActivitySourceId = "commit-sha-123"
                , newActivityRaw = object []
                , newActivityTitle = Just "Commit: feat: add feature"
                , newActivitySenderEmail = Nothing
                , newActivityStartsAt = Nothing
                , newActivityEndsAt = Nothing
                }
          mChildId <- insertActivityWithParent child (Just pid)

          case mChildId of
            Nothing -> liftIO $ expectationFailure "Failed to insert child"
            Just cid -> do
              mChild <- getActivity cid
              case mChild of
                Nothing -> liftIO $ expectationFailure "Failed to retrieve child"
                Just childActivity ->
                  liftIO $ activityParentId childActivity `shouldBe` Just pid
