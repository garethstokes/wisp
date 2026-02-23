module Services.ProjectUpdater
  ( updateProjectFromActivity
  , linkAndUpdateProject
  ) where

import Data.Aeson (Result(..), toJSON, fromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub)
import Domain.Id (EntityId)
import Domain.Document (Document(..), ExtendedProjectData(..), NewDocumentLogEntry(..), LogSource(..))
import Domain.ActivityDocument (NewActivityDocument(..), LinkSource(..), Relationship(..))
import Domain.ProjectClassification (ProjectAssignment(..))
import Infra.Db.Document (getDocumentById, updateProjectData, insertDocumentLog, getProjectByTag)
import Infra.Db.ActivityDocument (linkActivityToDocument)
import App.Monad (App)

-- | Update a project document based on a newly classified activity
-- Performs immediate updates: increment count, update participants, link activity
updateProjectFromActivity :: EntityId -> EntityId -> ProjectAssignment -> Maybe Text -> App ()
updateProjectFromActivity activityId documentId assignment mSenderEmail = do
  -- Link activity to document
  let link = NewActivityDocument
        { newAdActivityId = activityId
        , newAdDocumentId = documentId
        , newAdRelationship = Project
        , newAdConfidence = Just (paConfidence assignment)
        , newAdSource = Classifier
        }
  linkActivityToDocument link

  -- Get current project state
  mDoc <- getDocumentById documentId
  case mDoc of
    Nothing -> pure ()  -- Document doesn't exist
    Just doc -> do
      -- Parse current state
      case fromJSON (documentData doc) :: Result ExtendedProjectData of
        Error _ -> pure ()  -- Invalid data, skip update
        Success projData -> do
          -- Update state
          let newParticipants = case mSenderEmail of
                Nothing -> extProjectParticipants projData
                Just email -> nub $ extProjectParticipants projData ++ [T.toLower email]
              newCount = extProjectActivityCount projData + 1
              updatedData = projData
                { extProjectParticipants = newParticipants
                , extProjectActivityCount = newCount
                }

          -- Save updated state
          updateProjectData documentId (toJSON updatedData)

          -- Log the change
          let logEntry = NewDocumentLogEntry
                { newLogDocumentId = documentId
                , newLogActivityId = Just activityId
                , newLogDescription = Just $ "Activity linked (confidence: " <> T.pack (show $ paConfidence assignment) <> ")"
                , newLogSource = LogClassifier
                , newLogAgentId = Nothing
                }
          _ <- insertDocumentLog logEntry
          pure ()

-- | Convenience function to look up project by tag and update
linkAndUpdateProject :: EntityId -> ProjectAssignment -> Maybe Text -> App ()
linkAndUpdateProject activityId assignment mSenderEmail = do
  -- Look up project by tag
  mDoc <- getProjectByTag (paName assignment)
  case mDoc of
    Nothing -> pure ()  -- Project not found, skip
    Just doc -> updateProjectFromActivity activityId (documentId doc) assignment mSenderEmail
