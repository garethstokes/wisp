-- | Agent Core Module
-- Unified agent logic for loading, context assembly, and chat handling.
-- Agents are named personas with memory (soul) that can activate skills.
module Agents.Core
  ( Agent(..)
  , ToolExecutionResult(..)
  , loadAgent
  , buildSystemPrompt
  , loadSkillPrompt
  , executeTool
  ) where

import Data.Aeson (Value, decode, encode)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Agent (AgentName, AgentConfig(..), agentTag, emptyAgentConfig)
import qualified Domain.Agent as DA
import Domain.Skill (SkillName, skillPromptTags)
import Domain.Soul (Soul(..))
import Domain.Activity (Activity(..), activityRaw, activityTitle)
import Domain.Id (EntityId)
import Skills.Registry (Skill(..), SkillToolResult(..), getSkill, executeSkillTool)
import Skills.Base (baseToolNames, baseToolNamesWithSkill, parseBaseToolCall, executeBaseTool, BaseToolResult(..))
import Infra.Db.Activity (getActivitiesByTags)
import Infra.Db.Soul (getOrCreateSoul)
import App.Monad (App)

-- | A loaded agent ready for interaction
data Agent = Agent
  { agentName   :: AgentName
  , agentConfig :: AgentConfig
  , agentSoul   :: Soul
  , agentSkill  :: Maybe Skill  -- ^ Currently active skill (resolved from config)
  }

-- | Load an agent from knowledge
-- Looks for a note with tag "agent:NAME" containing AgentConfig JSON
loadAgent :: EntityId -> AgentName -> App (Maybe Agent)
loadAgent accountId name = do
  -- Find agent definition note
  let tags = [agentTag name]
  notes <- getActivitiesByTags accountId tags 1
  case notes of
    [] -> pure Nothing
    (note:_) -> do
      let config = case decode (encode (activityRaw note)) of
            Just c -> c
            Nothing -> emptyAgentConfig
      soul <- getOrCreateSoul name
      let mSkill = DA.agentActiveSkill config >>= getSkill
      pure $ Just Agent
        { agentName = name
        , agentConfig = config
        , agentSoul = soul
        , agentSkill = mSkill
        }

-- | Build system prompt for an agent
-- Combines personality, soul insights, and skill prompt
buildSystemPrompt :: Agent -> Maybe Text -> Text
buildSystemPrompt agent mSkillPrompt = T.unlines
  [ "You are " <> agentName agent <> ", a personal assistant."
  , ""
  , "## Your Personality"
  , if T.null (agentPersonalitySeed (agentConfig agent))
    then "Be helpful and concise."
    else agentPersonalitySeed (agentConfig agent)
  , ""
  , buildSoulSection (agentSoul agent)
  , ""
  , case mSkillPrompt of
      Just prompt -> "## Active Skill\n\n" <> prompt
      Nothing -> "## Available Actions\n\nYou can search and read knowledge, add notes, or activate a skill for specialized tasks."
  , ""
  , "## Tools"
  , T.unlines $ map ("- " <>) $ baseToolNames <> maybe [] skillToolNames (agentSkill agent)
  ]

-- | Build soul section for system prompt
buildSoulSection :: Soul -> Text
buildSoulSection soul
  | T.null (soulPersonality soul) && null (soulInsights soul) = ""
  | otherwise = T.unlines
      [ "## Insights About This User"
      , ""
      , if T.null (soulPersonality soul)
        then ""
        else "Communication style: " <> soulPersonality soul
      , ""
      , if null (soulInsights soul)
        then ""
        else T.unlines $ map ("- " <>) (soulInsights soul)
      ]

-- | Load skill prompt from knowledge
-- Looks for a note with tags [skill:NAME, prompt]
loadSkillPrompt :: EntityId -> SkillName -> App (Maybe Text)
loadSkillPrompt accountId name = do
  let tags = skillPromptTags name
  notes <- getActivitiesByTags accountId tags 1
  case notes of
    [] -> pure Nothing
    (note:_) -> pure $ activityTitle note

-- | Result of tool execution
data ToolExecutionResult
  = ToolExecSuccess Value          -- ^ Tool executed successfully
  | ToolExecError Text             -- ^ Tool execution failed
  | ToolExecPermission Text Text   -- ^ Requires user permission (action, message)
  deriving (Show, Eq)

-- | Execute a tool call for an agent
-- Routes base tools to Skills.Base, skill-specific tools to the active skill
executeTool :: Agent -> EntityId -> Text -> Value -> App ToolExecutionResult
executeTool agent accountId toolName params
  -- Check if it's a base tool
  | toolName `elem` allBaseTools = executeBaseToolCall accountId toolName params
  -- Otherwise try skill tools
  | otherwise = case agentSkill agent of
      Nothing -> pure $ ToolExecError $ "No skill active for tool: " <> toolName
      Just skill
        | toolName `elem` skillToolNames skill ->
            executeSkillToolCall skill accountId toolName params
        | otherwise ->
            pure $ ToolExecError $ "Tool not available: " <> toolName
  where
    allBaseTools = baseToolNames <> baseToolNamesWithSkill

-- | Execute a base tool call
executeBaseToolCall :: EntityId -> Text -> Value -> App ToolExecutionResult
executeBaseToolCall accountId toolName params =
  case parseBaseToolCall toolName params of
    Left err -> pure $ ToolExecError err
    Right toolCall -> do
      result <- executeBaseTool accountId toolCall
      pure $ case result of
        ToolSuccess v -> ToolExecSuccess v
        ToolError e -> ToolExecError e
        PermissionRequest action msg -> ToolExecPermission action msg

-- | Execute a skill tool call
executeSkillToolCall :: Skill -> EntityId -> Text -> Value -> App ToolExecutionResult
executeSkillToolCall skill accountId toolName params = do
  result <- executeSkillTool skill accountId toolName params
  pure $ case result of
    SkillToolSuccess v -> ToolExecSuccess v
    SkillToolError e -> ToolExecError e
