-- | Agent Core Module
-- Unified agent logic for loading, context assembly, and chat handling.
-- Agents are named personas with memory (soul) that can activate skills.
module Agents.Core
  ( Agent(..)
  , ToolExecutionResult(..)
  , loadAgentByTenant
  , buildSystemPrompt
  , loadSkillPromptByTenant
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
import Domain.Tenant (TenantId)
import Skills.Registry (Skill(..), ToolDef(..), SkillToolResult(..), getSkill, executeSkillTool, baseTools)
import Skills.Base (baseToolNames, baseToolNamesWithSkill, parseBaseToolCall, executeBaseTool, BaseToolResult(..))
import Infra.Db.Activity (getActivitiesByTagsTenant)
import Infra.Db.Soul (getOrCreateSoul)
import App.Monad (App)

-- | A loaded agent ready for interaction
data Agent = Agent
  { agentName   :: AgentName
  , agentConfig :: AgentConfig
  , agentSoul   :: Soul
  , agentSkill  :: Maybe Skill  -- ^ Currently active skill (resolved from config)
  }

-- | Load an agent from knowledge (tenant-scoped)
-- Looks for a note with tag "agent:NAME" containing AgentConfig JSON
loadAgentByTenant :: TenantId -> AgentName -> App (Maybe Agent)
loadAgentByTenant tenantId name = do
  -- Find agent definition note
  let tags = [agentTag name]
  notes <- getActivitiesByTagsTenant tenantId tags 1
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
  , "## Response Format"
  , "CRITICAL: You MUST respond with ONLY valid JSON. No markdown, no explanation, no text outside JSON."
  , ""
  , "When responding to the user (no tools needed):"
  , "{\"msg\": \"Your response to the user\"}"
  , ""
  , "When you need to call tools:"
  , "{\"msg\": \"Brief status\", \"tools\": [{\"tool\": \"tool_name\", \"param\": \"value\"}, ...]}"
  , ""
  , "After receiving tool results, respond with another JSON object (more tools or final msg)."
  , "ALWAYS output valid JSON. Never output plain text."
  , ""
  , case mSkillPrompt of
      Just prompt -> "## Active Skill\n\n" <> prompt
      Nothing -> "## Available Actions\n\nYou can search and read knowledge, add notes, or activate a skill for specialized tasks."
  , ""
  , "## Tools"
  , ""
  , formatToolDefs $ baseTools <> maybe [] skillTools (agentSkill agent)
  ]

-- | Format tool definitions as JSON examples
formatToolDefs :: [ToolDef] -> Text
formatToolDefs defs = T.unlines $ map formatTool defs
  where
    formatTool (ToolDef name schema) = name <> ":\n  " <> schema

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

-- | Load skill prompt from knowledge (tenant-scoped)
-- Looks for a note with tags [skill:NAME, prompt]
loadSkillPromptByTenant :: TenantId -> SkillName -> App (Maybe Text)
loadSkillPromptByTenant tenantId name = do
  let tags = skillPromptTags name
  notes <- getActivitiesByTagsTenant tenantId tags 1
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
-- mTimezone: Optional IANA timezone for date/time formatting in skill tools
executeTool :: Agent -> EntityId -> Text -> Value -> Maybe Text -> App ToolExecutionResult
executeTool agent accountId toolName params mTimezone
  -- Check if it's a base tool
  | toolName `elem` allBaseTools = executeBaseToolCall accountId toolName params
  -- Otherwise try skill tools
  | otherwise = case agentSkill agent of
      Nothing -> pure $ ToolExecError $ "No skill active for tool: " <> toolName
      Just skill
        | toolName `elem` skillToolNames skill ->
            executeSkillToolCall skill accountId toolName params mTimezone
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
executeSkillToolCall :: Skill -> EntityId -> Text -> Value -> Maybe Text -> App ToolExecutionResult
executeSkillToolCall skill accountId toolName params mTimezone = do
  result <- executeSkillTool skill accountId toolName params mTimezone
  pure $ case result of
    SkillToolSuccess v -> ToolExecSuccess v
    SkillToolError e -> ToolExecError e
