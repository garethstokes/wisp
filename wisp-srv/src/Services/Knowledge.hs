-- | Knowledge Service
-- Assembles knowledge context (projects, notes, preferences) for agent prompts.
module Services.Knowledge
  ( getKnowledgeContext
  , formatKnowledgeContext
  , KnowledgeContext(..)
  ) where

import Data.Aeson (Result(..), fromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Document (Document(..), DocumentType(..), NoteData(..), ProjectData(..), PreferenceData(..))
import Domain.Tenant (TenantId)
import Infra.Db.Document (getDocumentsByType, getDocumentsByTags)
import App.Monad (App)

-- | Knowledge context to inject into agent prompts
data KnowledgeContext = KnowledgeContext
  { kcProjects :: [Document]
  , kcRelevantNotes :: [Document]
  , kcPreferences :: [Document]
  } deriving (Show)

-- | Get knowledge context for an agent conversation
-- Takes optional tags to filter relevant notes
getKnowledgeContext :: TenantId -> [Text] -> App KnowledgeContext
getKnowledgeContext _tenantId mentionedTags = do
  -- Get all active projects
  projects <- getDocumentsByType ProjectDoc True 20

  -- Get preferences
  preferences <- getDocumentsByType PreferenceDoc True 50

  -- Get relevant notes based on mentioned tags
  relevantNotes <- if null mentionedTags
    then getDocumentsByType NoteDoc True 10  -- Recent notes if no tags
    else getDocumentsByTags mentionedTags True 20

  pure KnowledgeContext
    { kcProjects = projects
    , kcRelevantNotes = relevantNotes
    , kcPreferences = preferences
    }

-- | Format knowledge context as text for system prompt
formatKnowledgeContext :: KnowledgeContext -> Text
formatKnowledgeContext kc = T.unlines
  [ "## Knowledge"
  , ""
  , "### Projects"
  , if null (kcProjects kc)
      then "(no projects)"
      else T.unlines $ map formatProject (kcProjects kc)
  , ""
  , "### Preferences"
  , if null (kcPreferences kc)
      then "(no preferences)"
      else T.unlines $ map formatPreference (kcPreferences kc)
  , ""
  , "### Relevant Notes"
  , if null (kcRelevantNotes kc)
      then "(no relevant notes)"
      else T.unlines $ map formatNote (kcRelevantNotes kc)
  ]
  where
    formatProject doc = case fromJSON (documentData doc) of
      Success (pd :: ProjectData) -> "- " <> projectName pd <> " [" <> T.intercalate ", " (documentTags doc) <> "]"
      Error _ -> "- (unknown project)"

    formatPreference doc = case fromJSON (documentData doc) of
      Success (pd :: PreferenceData) -> "- " <> prefKey pd <> ": " <> prefValue pd
      Error _ -> "- (unknown preference)"

    formatNote doc = case fromJSON (documentData doc) of
      Success (nd :: NoteData) -> "- " <> noteTitle nd <> " [" <> T.intercalate ", " (documentTags doc) <> "]"
      Error _ -> "- (unknown note)"
