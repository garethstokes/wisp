module Services.ProjectUpdaterSpec where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import qualified Data.Aeson
import Data.Maybe (fromJust)
import Domain.Id (EntityId(..))
import Domain.Activity (NewActivity(..), ActivitySource(..))
import Domain.Document (NewDocument(..), DocumentType(..), ExtendedProjectData(..), Document(..))
import Domain.ProjectClassification (ProjectAssignment(..))
import Infra.Db.Activity (insertActivity)
import qualified Infra.Db.Document
import Services.ProjectUpdater
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = around withTestEnv $ do

  describe "updateProjectFromActivity" $ do
    it "increments activity count and updates last_activity_at" $ \env -> runTestApp env $ do
      -- Create project with initial state
      let projData = object
            [ "name" .= ("Wisp" :: String)
            , "type" .= ("work" :: String)
            , "summary" .= ("" :: String)
            , "status" .= ("active" :: String)
            , "participants" .= ([] :: [String])
            , "activity_count" .= (0 :: Int)
            ]
      let newDoc = NewDocument Nothing ProjectDoc projData ["wisp"] (Just 1.0) (Just "user") Nothing Nothing
      docId <- Infra.Db.Document.insertDocument newDoc

      -- Create activity
      let newAct = NewActivity
            { newActivityAccountId = EntityId "test-account"
            , newActivitySource = Email
            , newActivitySourceId = "email-1"
            , newActivityRaw = object []
            , newActivityTitle = Just "Wisp feature request"
            , newActivitySenderEmail = Just "alice@example.com"
            , newActivityStartsAt = Nothing
            , newActivityEndsAt = Nothing
            }
      mActId <- insertActivity newAct
      let actId = fromJust mActId

      -- Update project
      let assignment = ProjectAssignment "wisp" 0.9
      updateProjectFromActivity actId docId assignment (Just "alice@example.com")

      -- Verify
      mDoc <- Infra.Db.Document.getDocumentById docId
      case mDoc of
        Just doc -> do
          let Just projState = Data.Aeson.decode (Data.Aeson.encode $ documentData doc) :: Maybe ExtendedProjectData
          liftIO $ extProjectActivityCount projState `shouldBe` 1
          liftIO $ extProjectParticipants projState `shouldContain` ["alice@example.com"]
        Nothing -> liftIO $ expectationFailure "Document not found"

    it "adds new participants without duplicating existing ones" $ \env -> runTestApp env $ do
      -- Create project with existing participant
      let projData = object
            [ "name" .= ("Wisp" :: String)
            , "type" .= ("work" :: String)
            , "participants" .= (["bob@example.com"] :: [String])
            , "activity_count" .= (5 :: Int)
            ]
      let newDoc = NewDocument Nothing ProjectDoc projData ["wisp"] (Just 1.0) (Just "user") Nothing Nothing
      docId <- Infra.Db.Document.insertDocument newDoc

      -- Create activity
      let newAct = NewActivity
            { newActivityAccountId = EntityId "test-account"
            , newActivitySource = Email
            , newActivitySourceId = "email-2"
            , newActivityRaw = object []
            , newActivityTitle = Just "Another email"
            , newActivitySenderEmail = Just "bob@example.com"  -- Same participant
            , newActivityStartsAt = Nothing
            , newActivityEndsAt = Nothing
            }
      mActId <- insertActivity newAct
      let actId = fromJust mActId

      -- Update project
      let assignment = ProjectAssignment "wisp" 0.85
      updateProjectFromActivity actId docId assignment (Just "bob@example.com")

      -- Verify - should have 6 activities but still only 1 participant
      mDoc <- Infra.Db.Document.getDocumentById docId
      case mDoc of
        Just doc -> do
          let Just projState = Data.Aeson.decode (Data.Aeson.encode $ documentData doc) :: Maybe ExtendedProjectData
          liftIO $ extProjectActivityCount projState `shouldBe` 6
          liftIO $ length (extProjectParticipants projState) `shouldBe` 1
        Nothing -> liftIO $ expectationFailure "Document not found"

  describe "linkAndUpdateProject" $ do
    it "looks up project by tag and updates it" $ \env -> runTestApp env $ do
      -- Create project with "lune" tag
      let projData = object
            [ "name" .= ("Lune" :: String)
            , "type" .= ("work" :: String)
            , "activity_count" .= (0 :: Int)
            , "participants" .= ([] :: [String])
            ]
      let newDoc = NewDocument Nothing ProjectDoc projData ["lune"] (Just 1.0) (Just "user") Nothing Nothing
      _ <- Infra.Db.Document.insertDocument newDoc

      -- Create activity
      let newAct = NewActivity
            { newActivityAccountId = EntityId "test-account"
            , newActivitySource = Email
            , newActivitySourceId = "email-3"
            , newActivityRaw = object []
            , newActivityTitle = Just "Lune update"
            , newActivitySenderEmail = Just "charlie@example.com"
            , newActivityStartsAt = Nothing
            , newActivityEndsAt = Nothing
            }
      mActId <- insertActivity newAct
      let actId = fromJust mActId

      -- Update via tag lookup
      let assignment = ProjectAssignment "lune" 0.95
      linkAndUpdateProject actId assignment (Just "charlie@example.com")

      -- Verify - find the project and check it was updated
      docs <- Infra.Db.Document.getDocumentsByTags ["lune"] True 1
      case docs of
        [doc] -> do
          let Just projState = Data.Aeson.decode (Data.Aeson.encode $ documentData doc) :: Maybe ExtendedProjectData
          liftIO $ extProjectActivityCount projState `shouldBe` 1
          liftIO $ extProjectParticipants projState `shouldContain` ["charlie@example.com"]
        _ -> liftIO $ expectationFailure "Project not found"
