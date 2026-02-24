module Infra.Db.DocumentSpec where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Domain.Document (NewDocument(..), DocumentType(..), documentId, documentData)
import Infra.Db.Document
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = around withTestEnv $ do

  describe "getProjectByTag" $ do
    it "finds project by tag" $ \env -> runTestApp env $ do
      let newDoc = NewDocument
            { newDocTenantId = Nothing
            , newDocType = ProjectDoc
            , newDocData = object ["name" .= ("Wisp" :: String), "type" .= ("work" :: String)]
            , newDocTags = ["wisp"]
            , newDocConfidence = Just 1.0
            , newDocSource = Just "user"
            , newDocSupersedesId = Nothing
            , newDocParentId = Nothing
            }
      docId <- insertDocument newDoc

      mDoc <- getProjectByTag "wisp"
      liftIO $ case mDoc of
        Just doc -> documentId doc `shouldBe` docId
        Nothing -> expectationFailure "Project not found"

    it "returns Nothing when tag does not exist" $ \env -> runTestApp env $ do
      mDoc <- getProjectByTag "nonexistent-project"
      liftIO $ mDoc `shouldBe` Nothing

    it "only finds active projects" $ \env -> runTestApp env $ do
      let newDoc = NewDocument
            { newDocTenantId = Nothing
            , newDocType = ProjectDoc
            , newDocData = object ["name" .= ("ArchivedProj" :: String)]
            , newDocTags = ["archived-test"]
            , newDocConfidence = Just 1.0
            , newDocSource = Just "user"
            , newDocSupersedesId = Nothing
            , newDocParentId = Nothing
            }
      docId <- insertDocument newDoc
      archiveDocument docId

      mDoc <- getProjectByTag "archived-test"
      liftIO $ mDoc `shouldBe` Nothing

  describe "getAllProjects" $ do
    it "returns all active projects" $ \env -> runTestApp env $ do
      -- Create a project
      let newDoc = NewDocument
            { newDocTenantId = Nothing
            , newDocType = ProjectDoc
            , newDocData = object ["name" .= ("TestProj" :: String), "type" .= ("work" :: String)]
            , newDocTags = ["testproj"]
            , newDocConfidence = Just 1.0
            , newDocSource = Just "user"
            , newDocSupersedesId = Nothing
            , newDocParentId = Nothing
            }
      _ <- insertDocument newDoc

      projects <- getAllProjects
      liftIO $ projects `shouldSatisfy` (not . null)

    it "does not include archived projects" $ \env -> runTestApp env $ do
      let newDoc = NewDocument
            { newDocTenantId = Nothing
            , newDocType = ProjectDoc
            , newDocData = object ["name" .= ("ArchivedProj2" :: String)]
            , newDocTags = ["archived-test-2"]
            , newDocConfidence = Just 1.0
            , newDocSource = Just "user"
            , newDocSupersedesId = Nothing
            , newDocParentId = Nothing
            }
      docId <- insertDocument newDoc
      archiveDocument docId

      projects <- getAllProjects
      liftIO $ all (\p -> documentId p /= docId) projects `shouldBe` True

  describe "updateProjectData" $ do
    it "updates the project data" $ \env -> runTestApp env $ do
      let newDoc = NewDocument
            { newDocTenantId = Nothing
            , newDocType = ProjectDoc
            , newDocData = object ["name" .= ("UpdateTest" :: String), "count" .= (0 :: Int)]
            , newDocTags = ["update-test"]
            , newDocConfidence = Just 1.0
            , newDocSource = Just "user"
            , newDocSupersedesId = Nothing
            , newDocParentId = Nothing
            }
      docId <- insertDocument newDoc

      -- Update the data
      let newData = object ["name" .= ("UpdateTest" :: String), "count" .= (5 :: Int)]
      updateProjectData docId newData

      -- Verify the update
      mDoc <- getDocumentById docId
      liftIO $ case mDoc of
        Just doc -> documentData doc `shouldBe` newData
        Nothing -> expectationFailure "Document not found after update"
