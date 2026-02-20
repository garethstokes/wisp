module Tui.DataLoader
  ( loadActivities
  , loadDocuments
  , loadApprovals
  , DataLoadResult(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Wisp.Client
  ( ClientConfig
  , ClientError(..)
  , Activity
  , Document
  , ApprovalItem(..)
  , getActivities
  , getProjects
  , getNotes
  , getPreferences
  , getApprovals
  )

-- | Result of a data load operation
data DataLoadResult
  = ActivitiesLoaded [Activity]
  | DocumentsLoaded [Document] [Document] [Document]  -- projects, notes, prefs
  | ApprovalsLoaded [(Activity, Text, Text)]
  | LoadError Text
  deriving (Show, Eq)

-- | Load activities from server
loadActivities :: ClientConfig -> IO DataLoadResult
loadActivities cfg = do
  result <- getActivities cfg
  pure $ case result of
    Left err -> LoadError $ "Failed to load activities: " <> showError err
    Right acts -> ActivitiesLoaded acts

-- | Load all document types from server
loadDocuments :: ClientConfig -> IO DataLoadResult
loadDocuments cfg = do
  projectsResult <- getProjects cfg
  notesResult <- getNotes cfg
  prefsResult <- getPreferences cfg

  let projects = either (const []) id projectsResult
      notes = either (const []) id notesResult
      prefs = either (const []) id prefsResult

  pure $ DocumentsLoaded projects notes prefs

-- | Load approvals from server
loadApprovals :: ClientConfig -> IO DataLoadResult
loadApprovals cfg = do
  result <- getApprovals cfg
  pure $ case result of
    Left err -> LoadError $ "Failed to load approvals: " <> showError err
    Right items -> ApprovalsLoaded
      [ (approvalActivity item, approvalType item, approvalReason item)
      | item <- items
      ]

showError :: ClientError -> Text
showError (HttpError t) = t
showError (ParseError t) = t
showError (ServerError code t) = "Server error " <> T.pack (show code) <> ": " <> t
