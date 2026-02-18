-- | Base Tools Module
-- Tools available to all agents without requiring a skill.
-- These enable working with the knowledge system and skill activation.
module Skills.Base
  ( baseToolNames
  , BaseToolCall(..)
  , BaseToolResult(..)
  , parseBaseToolCall
  , executeBaseTool
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Result(..), Value(..), fromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Id (EntityId)
import App.Monad (App)
import Infra.Db.Activity (insertNote, getActivitiesByTags, getActivity)
import Skills.Registry (allSkillNames)

-- | Tools available to all agents (without skills)
baseToolNames :: [Text]
baseToolNames =
  [ "search_knowledge"
  , "read_note"
  , "add_note"
  , "activate_skill"
  ]

-- | Parsed base tool call
data BaseToolCall
  = SearchKnowledge [Text] Int    -- ^ tags, limit
  | ReadNote EntityId             -- ^ note ID
  | AddNote Text [Text]           -- ^ content, tags
  | ActivateSkill Text            -- ^ skill name
  deriving (Show, Eq)

-- | Result of executing a base tool
data BaseToolResult
  = ToolSuccess Value
  | ToolError Text
  | PermissionRequest Text Text   -- ^ action, message (for skill activation)
  deriving (Show, Eq)

instance ToJSON BaseToolResult where
  toJSON (ToolSuccess v) = object ["status" .= ("success" :: Text), "result" .= v]
  toJSON (ToolError e) = object ["status" .= ("error" :: Text), "error" .= e]
  toJSON (PermissionRequest action msg) = object
    [ "status" .= ("permission_required" :: Text)
    , "action" .= action
    , "message" .= msg
    ]

-- | Parse a tool call from tool name and arguments
parseBaseToolCall :: Text -> Value -> Either Text BaseToolCall
parseBaseToolCall "search_knowledge" args = parseSearchKnowledge args
parseBaseToolCall "read_note" args = parseReadNote args
parseBaseToolCall "add_note" args = parseAddNote args
parseBaseToolCall "activate_skill" args = parseActivateSkill args
parseBaseToolCall name _ = Left $ "Unknown base tool: " <> name

-- Internal argument types for parsing
data SearchKnowledgeArgs = SearchKnowledgeArgs
  { searchTags :: [Text]
  , searchLimit :: Maybe Int
  }

instance FromJSON SearchKnowledgeArgs where
  parseJSON = withObject "SearchKnowledgeArgs" $ \v -> SearchKnowledgeArgs
    <$> v .: "tags"
    <*> v .:? "limit"

data ReadNoteArgs = ReadNoteArgs { readNoteId :: EntityId }

instance FromJSON ReadNoteArgs where
  parseJSON = withObject "ReadNoteArgs" $ \v -> ReadNoteArgs <$> v .: "note_id"

data AddNoteArgs = AddNoteArgs
  { addNoteContent :: Text
  , addNoteTags :: Maybe [Text]
  }

instance FromJSON AddNoteArgs where
  parseJSON = withObject "AddNoteArgs" $ \v -> AddNoteArgs
    <$> v .: "content"
    <*> v .:? "tags"

data ActivateSkillArgs = ActivateSkillArgs { activateSkillName :: Text }

instance FromJSON ActivateSkillArgs where
  parseJSON = withObject "ActivateSkillArgs" $ \v -> ActivateSkillArgs <$> v .: "skill"

parseSearchKnowledge :: Value -> Either Text BaseToolCall
parseSearchKnowledge v = case fromJSON v of
  Success args -> Right $ SearchKnowledge (searchTags args) (maybe 20 id (searchLimit args))
  Error e -> Left $ "Invalid search_knowledge args: " <> toText e

parseReadNote :: Value -> Either Text BaseToolCall
parseReadNote v = case fromJSON v of
  Success args -> Right $ ReadNote (readNoteId args)
  Error e -> Left $ "Invalid read_note args: " <> toText e

parseAddNote :: Value -> Either Text BaseToolCall
parseAddNote v = case fromJSON v of
  Success args -> Right $ AddNote (addNoteContent args) (maybe [] id (addNoteTags args))
  Error e -> Left $ "Invalid add_note args: " <> toText e

parseActivateSkill :: Value -> Either Text BaseToolCall
parseActivateSkill v = case fromJSON v of
  Success args -> Right $ ActivateSkill (activateSkillName args)
  Error e -> Left $ "Invalid activate_skill args: " <> toText e

toText :: String -> Text
toText = T.pack

-- | Execute a base tool call
executeBaseTool :: EntityId -> BaseToolCall -> App BaseToolResult
executeBaseTool accountId tool = case tool of
  SearchKnowledge tags limit -> do
    notes <- getActivitiesByTags accountId tags limit
    pure $ ToolSuccess $ toJSON notes

  ReadNote noteId -> do
    mNote <- getActivity noteId
    case mNote of
      Just note -> pure $ ToolSuccess $ toJSON note
      Nothing -> pure $ ToolError "Note not found"

  AddNote content tags -> do
    let rawMeta = object ["origin" .= ("agent" :: Text)]
    mId <- insertNote accountId content tags rawMeta
    case mId of
      Just aid -> pure $ ToolSuccess $ object
        [ "note_id" .= aid
        , "status" .= ("created" :: Text)
        ]
      Nothing -> pure $ ToolError "Failed to create note"

  ActivateSkill skillName ->
    if skillName `elem` allSkillNames
      then pure $ PermissionRequest
        ("activate_skill:" <> skillName)
        ("Activate " <> skillName <> " skill?")
      else pure $ ToolError $ "Unknown skill: " <> skillName
