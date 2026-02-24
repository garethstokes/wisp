-- | Knowledge Service
-- Assembles knowledge context (projects, notes, preferences) for agent prompts.
-- Also handles note capture from chat messages.
module Services.Knowledge
  ( getKnowledgeContext
  , formatKnowledgeContext
  , KnowledgeContext(..)
  , detectNoteCommand
  , captureNote
  ) where

import Data.Aeson (Result(..), fromJSON, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Document (Document(..), DocumentType(..), NoteData(..), ProjectData(..), PreferenceData(..), NewDocument(..))
import Domain.Tenant (TenantId)
import Domain.Id (EntityId)
import Infra.Db.Document (getDocumentsByType, getDocumentsByTags, insertDocument)
import App.Monad (App)

-- | Knowledge context to inject into agent prompts
data KnowledgeContext = KnowledgeContext
  { kcProjects :: [Document]
  , kcRelevantNotes :: [Document]
  , kcPreferences :: [Document]
  , kcSessionSummaries :: [Document]  -- ^ Recent session summaries for this agent
  } deriving (Show)

-- | Get knowledge context for an agent conversation
-- Takes agent name and optional tags to filter relevant notes
getKnowledgeContext :: TenantId -> Text -> [Text] -> App KnowledgeContext
getKnowledgeContext _tenantId agentId mentionedTags = do
  -- Get all active projects
  projects <- getDocumentsByType ProjectDoc True 20

  -- Get preferences
  preferences <- getDocumentsByType PreferenceDoc True 50

  -- Get relevant notes based on mentioned tags
  relevantNotes <- if null mentionedTags
    then getDocumentsByType NoteDoc True 10  -- Recent notes if no tags
    else getDocumentsByTags mentionedTags True 20

  -- Get recent session summaries for this agent
  let summaryTags = ["session-summary", "agent:" <> agentId]
  summaries <- getDocumentsByTags summaryTags True 5

  pure KnowledgeContext
    { kcProjects = projects
    , kcRelevantNotes = relevantNotes
    , kcPreferences = preferences
    , kcSessionSummaries = summaries
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
  , ""
  , "### Recent Conversations"
  , if null (kcSessionSummaries kc)
      then "(no recent conversation history)"
      else T.unlines $ map formatSummary (kcSessionSummaries kc)
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

    formatSummary doc = case fromJSON (documentData doc) of
      Success (nd :: NoteData) -> "- " <> maybe "(no summary)" id (noteContent nd)
      Error _ -> "- (unknown summary)"

-- | Check if a message is a note command
-- Recognizes "Note: ..." and "Remember: ..." prefixes
-- Returns the note content with prefix stripped
detectNoteCommand :: Text -> Maybe Text
detectNoteCommand msg
  | "note:" `T.isPrefixOf` T.toLower msg = Just $ T.strip $ T.drop 5 msg
  | "remember:" `T.isPrefixOf` T.toLower msg = Just $ T.strip $ T.drop 9 msg
  | otherwise = Nothing

-- | Capture a note from chat
-- Creates a document with the note content as both title and body
captureNote :: TenantId -> Text -> Maybe Text -> App EntityId
captureNote tenantId noteContent mAgentId = do
  let noteData = NoteData
        { noteTitle = noteContent
        , noteContent = Nothing  -- Title is the content for simple notes
        }
      newDoc = NewDocument
        { newDocTenantId = Just tenantId
        , newDocType = NoteDoc
        , newDocData = toJSON noteData
        , newDocTags = []  -- TODO: extract tags from content
        , newDocConfidence = Just 0.9
        , newDocSource = mAgentId
        , newDocSupersedesId = Nothing
        , newDocParentId = Nothing
        }
  insertDocument newDoc
