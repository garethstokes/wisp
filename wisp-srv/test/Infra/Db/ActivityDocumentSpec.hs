module Infra.Db.ActivityDocumentSpec where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Domain.Id (EntityId(..))
import Domain.ActivityDocument
import Infra.Db.ActivityDocument
import Infra.Db.Activity (insertActivity)
import Infra.Db.Document (insertDocument)
import Domain.Activity (NewActivity(..), ActivitySource(..))
import Domain.Document (NewDocument(..), DocumentType(..))
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = around withTestEnv $ do

  describe "linkActivityToDocument" $ do
    it "creates a link between activity and document" $ \env -> runTestApp env $ do
      -- Create test activity
      let newActivity = NewActivity
            { newActivityAccountId = EntityId "test-account"
            , newActivitySource = Email
            , newActivitySourceId = "test-email-1"
            , newActivityRaw = object ["test" .= True]
            , newActivityTitle = Just "Test email"
            , newActivitySenderEmail = Just "alice@example.com"
            , newActivityStartsAt = Nothing
            , newActivityEndsAt = Nothing
            }
      mActId <- insertActivity newActivity

      case mActId of
        Nothing -> liftIO $ expectationFailure "Failed to create activity"
        Just actId -> do
          -- Create test document
          let newDoc = NewDocument
                { newDocTenantId = Nothing
                , newDocType = ProjectDoc
                , newDocData = object ["name" .= ("Wisp" :: String), "type" .= ("work" :: String)]
                , newDocTags = ["wisp"]
                , newDocConfidence = Just 1.0
                , newDocSource = Just "user"
                , newDocSupersedesId = Nothing
                }
          docId <- insertDocument newDoc

          -- Link them
          let link = NewActivityDocument
                { newAdActivityId = actId
                , newAdDocumentId = docId
                , newAdRelationship = Project
                , newAdConfidence = Just 0.9
                , newAdSource = Classifier
                }
          linkActivityToDocument link

          -- Verify
          links <- getDocumentActivities docId 10
          liftIO $ length links `shouldBe` 1

  describe "getActivityProjects" $ do
    it "returns all projects linked to an activity" $ \env -> runTestApp env $ do
      -- Setup: create activity and two projects, link both
      let newActivity = NewActivity
            { newActivityAccountId = EntityId "test-account"
            , newActivitySource = Email
            , newActivitySourceId = "test-email-2"
            , newActivityRaw = object []
            , newActivityTitle = Just "Cross-project email"
            , newActivitySenderEmail = Nothing
            , newActivityStartsAt = Nothing
            , newActivityEndsAt = Nothing
            }
      mActId <- insertActivity newActivity

      case mActId of
        Nothing -> liftIO $ expectationFailure "Failed to create activity"
        Just actId -> do
          let doc1 = NewDocument
                { newDocTenantId = Nothing
                , newDocType = ProjectDoc
                , newDocData = object ["name" .= ("Wisp" :: String), "type" .= ("work" :: String)]
                , newDocTags = []
                , newDocConfidence = Nothing
                , newDocSource = Nothing
                , newDocSupersedesId = Nothing
                }
          docId1 <- insertDocument doc1

          let doc2 = NewDocument
                { newDocTenantId = Nothing
                , newDocType = ProjectDoc
                , newDocData = object ["name" .= ("Lune" :: String), "type" .= ("work" :: String)]
                , newDocTags = []
                , newDocConfidence = Nothing
                , newDocSource = Nothing
                , newDocSupersedesId = Nothing
                }
          docId2 <- insertDocument doc2

          linkActivityToDocument $ NewActivityDocument actId docId1 Project (Just 0.9) Classifier
          linkActivityToDocument $ NewActivityDocument actId docId2 Project (Just 0.7) Classifier

          projects <- getActivityProjects actId
          liftIO $ length projects `shouldBe` 2

  describe "unlinkActivityFromDocument" $ do
    it "removes a link between activity and document" $ \env -> runTestApp env $ do
      let newActivity = NewActivity
            { newActivityAccountId = EntityId "test-account"
            , newActivitySource = Email
            , newActivitySourceId = "test-email-3"
            , newActivityRaw = object []
            , newActivityTitle = Just "Test email for unlink"
            , newActivitySenderEmail = Nothing
            , newActivityStartsAt = Nothing
            , newActivityEndsAt = Nothing
            }
      mActId <- insertActivity newActivity

      case mActId of
        Nothing -> liftIO $ expectationFailure "Failed to create activity"
        Just actId -> do
          let newDoc = NewDocument
                { newDocTenantId = Nothing
                , newDocType = ProjectDoc
                , newDocData = object ["name" .= ("Test" :: String)]
                , newDocTags = []
                , newDocConfidence = Nothing
                , newDocSource = Nothing
                , newDocSupersedesId = Nothing
                }
          docId <- insertDocument newDoc

          -- Link and verify
          linkActivityToDocument $ NewActivityDocument actId docId Project (Just 0.8) User
          linksBefore <- getDocumentActivities docId 10
          liftIO $ length linksBefore `shouldBe` 1

          -- Unlink and verify
          unlinkActivityFromDocument actId docId Project
          linksAfter <- getDocumentActivities docId 10
          liftIO $ length linksAfter `shouldBe` 0
