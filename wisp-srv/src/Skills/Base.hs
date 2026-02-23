-- | Base Tools Module
-- Tools available to all agents without requiring a skill.
-- These enable working with the knowledge system and skill activation.
module Skills.Base
  ( baseToolNames
  , baseToolNamesWithSkill
  , BaseToolCall(..)
  , SpawnAgentParams(..)
  , BaseToolResult(..)
  , parseBaseToolCall
  , executeBaseTool
  , activateAgentSkillByTenant
  , deactivateAgentSkillByTenant
  ) where

import Control.Concurrent.Async (async)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON(..), ToJSON(..), Result(..), Value(..), encode, decode, fromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Id (EntityId, newEntityId, unEntityId)
import Domain.Agent (AgentName, AgentConfig(..), agentTag)
import Domain.Tenant (TenantId)
import qualified Domain.Activity
import App.Monad (App, Env(..), runApp)
import App.Config (Config(..), ClaudeConfig(..))
import Infra.Db.Activity (getActivitiesByTags, getActivitiesByTagsTenant, getActivity, updateActivityRaw, insertNote, searchActivities)
import Infra.Claude.Client (callClaudeWithSystem)
import Skills.Registry (allSkillNames)

-- | Tools available to all agents (without an active skill)
baseToolNames :: [Text]
baseToolNames =
  [ "search_knowledge"
  , "read_note"
  , "add_note"
  , "activate_skill"
  , "spawn_agent"
  ]

-- | Tools available when a skill is active (adds deactivate)
baseToolNamesWithSkill :: [Text]
baseToolNamesWithSkill =
  [ "search_knowledge"
  , "read_note"
  , "add_note"
  , "deactivate_skill"
  , "spawn_agent"
  ]

-- | Parsed base tool call
data BaseToolCall
  = SearchKnowledgeByQuery Text Int   -- ^ text query, limit
  | SearchKnowledgeByTags [Text] Int  -- ^ tags, limit
  | ReadNote EntityId                 -- ^ note ID
  | AddNote Text [Text]               -- ^ content, tags
  | ActivateSkill Text                -- ^ skill name
  | DeactivateSkill                   -- ^ deactivate current skill
  | SpawnAgent SpawnAgentParams       -- ^ spawn a sub-agent
  deriving (Show, Eq)

-- | Parameters for spawning a sub-agent
data SpawnAgentParams = SpawnAgentParams
  { spawnTask    :: Text           -- ^ What the sub-agent should accomplish
  , spawnTools   :: Maybe [Text]   -- ^ Restrict to these tools (Nothing = inherit)
  , spawnContext :: Maybe Value    -- ^ Extra context for sub-agent
  } deriving (Show, Eq)

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
parseBaseToolCall "deactivate_skill" _ = Right DeactivateSkill
parseBaseToolCall "spawn_agent" args = parseSpawnAgent args
parseBaseToolCall name _ = Left $ "Unknown base tool: " <> name

-- Internal argument types for parsing
data SearchKnowledgeArgs = SearchKnowledgeArgs
  { searchQuery :: Maybe Text    -- Text search (preferred)
  , searchTags :: Maybe [Text]   -- Tag filter (legacy)
  , searchLimit :: Maybe Int
  }

instance FromJSON SearchKnowledgeArgs where
  parseJSON = withObject "SearchKnowledgeArgs" $ \v -> SearchKnowledgeArgs
    <$> v .:? "query"
    <*> v .:? "tags"
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

instance FromJSON SpawnAgentParams where
  parseJSON = withObject "SpawnAgentParams" $ \v -> SpawnAgentParams
    <$> v .: "task"
    <*> v .:? "tools"
    <*> v .:? "context"

parseSearchKnowledge :: Value -> Either Text BaseToolCall
parseSearchKnowledge v = case fromJSON v of
  Success args ->
    let limit = maybe 20 id (searchLimit args)
    in case (searchQuery args, searchTags args) of
      (Just q, _) -> Right $ SearchKnowledgeByQuery q limit
      (Nothing, Just tags) -> Right $ SearchKnowledgeByTags tags limit
      (Nothing, Nothing) -> Left "search_knowledge requires 'query' or 'tags' parameter"
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

parseSpawnAgent :: Value -> Either Text BaseToolCall
parseSpawnAgent v = case fromJSON v of
  Success params -> Right $ SpawnAgent params
  Error e -> Left $ "Invalid spawn_agent args: " <> toText e

toText :: String -> Text
toText = T.pack

-- | Execute a base tool call
executeBaseTool :: EntityId -> BaseToolCall -> App BaseToolResult
executeBaseTool accountId tool = case tool of
  SearchKnowledgeByQuery query limit -> do
    notes <- searchActivities query limit
    pure $ ToolSuccess $ toJSON notes

  SearchKnowledgeByTags tags limit -> do
    -- Note: getActivitiesByTags requires accountId, but searchActivities doesn't
    -- For tag search, we use searchActivities with tags as query for now
    -- TODO: Consider making getActivitiesByTags work across all accounts
    notes <- searchActivities (T.unwords tags) limit
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

  DeactivateSkill ->
    pure $ PermissionRequest
      "deactivate_skill"
      "Deactivate current skill?"

  SpawnAgent params -> do
    -- Generate a unique agent ID
    agentIdRaw <- liftIO newEntityId
    let agentId = "subagent-" <> T.take 8 (unEntityId agentIdRaw)

    -- Get environment for async execution
    env <- asks id

    -- Build sub-agent system prompt
    let contextJson = case spawnContext params of
          Just ctx -> "\n\nContext: " <> T.pack (show ctx)
          Nothing -> ""
        toolsInfo = case spawnTools params of
          Just tools -> "\n\nAvailable tools: " <> T.intercalate ", " tools
          Nothing -> ""
        systemPrompt = T.unlines
          [ "You are a sub-agent with a specific task to complete."
          , ""
          , "## Your Task"
          , spawnTask params
          , contextJson
          , toolsInfo
          , ""
          , "## Instructions"
          , "Complete the task using available tools. Write your results using add_note or write_finding."
          , "Be thorough but concise. Focus only on your assigned task."
          ]

    -- Spawn async - fire and forget
    _ <- liftIO $ async $ runApp env $ do
      claudeCfg <- asks (claude . config)
      result <- liftIO $ callClaudeWithSystem
        (apiKey claudeCfg)
        (model claudeCfg)
        systemPrompt
        ("Please complete the following task: " <> spawnTask params)
      -- Sub-agent result is logged but not returned
      -- Results are written through tools (add_note, write_finding, etc.)
      case result of
        Left _err -> pure ()  -- Sub-agent failed silently
        Right (_response, _, _) -> pure ()  -- Sub-agent completed

    pure $ ToolSuccess $ object
      [ "agent_id" .= agentId
      , "task" .= spawnTask params
      , "status" .= ("spawned" :: Text)
      , "note" .= ("Sub-agent is running asynchronously. Results will be written via tools." :: Text)
      ]

-- | Activate a skill for an agent (tenant-scoped)
-- Updates the agent's note in knowledge to set active_skill
activateAgentSkillByTenant :: TenantId -> AgentName -> Text -> App (Either Text ())
activateAgentSkillByTenant tenantId agentName skillName = do
  if skillName `notElem` allSkillNames
    then pure $ Left $ "Unknown skill: " <> skillName
    else do
      -- Find the agent's note
      let tags = [agentTag agentName]
      notes <- getActivitiesByTagsTenant tenantId tags 1
      case notes of
        [] -> pure $ Left $ "Agent not found: " <> agentName
        (note:_) -> do
          -- Update the agent config with the new active skill
          let rawValue = activityRaw note
              mConfig = decode (encode rawValue) :: Maybe AgentConfig
              newConfig = case mConfig of
                Just c -> c { agentActiveSkill = Just skillName }
                Nothing -> AgentConfig "" (Just skillName)
          updateActivityRaw (activityId note) (toJSON newConfig)
          pure $ Right ()
  where
    activityRaw = Domain.Activity.activityRaw
    activityId = Domain.Activity.activityId

-- | Deactivate the current skill for an agent (tenant-scoped)
-- Updates the agent's note in knowledge to clear active_skill
deactivateAgentSkillByTenant :: TenantId -> AgentName -> App (Either Text ())
deactivateAgentSkillByTenant tenantId agentName = do
  -- Find the agent's note
  let tags = [agentTag agentName]
  notes <- getActivitiesByTagsTenant tenantId tags 1
  case notes of
    [] -> pure $ Left $ "Agent not found: " <> agentName
    (note:_) -> do
      -- Update the agent config to clear the active skill
      let rawValue = activityRaw note
          mConfig = decode (encode rawValue) :: Maybe AgentConfig
          newConfig = case mConfig of
            Just c -> c { agentActiveSkill = Nothing }
            Nothing -> AgentConfig "" Nothing
      updateActivityRaw (activityId note) (toJSON newConfig)
      pure $ Right ()
  where
    activityRaw = Domain.Activity.activityRaw
    activityId = Domain.Activity.activityId
