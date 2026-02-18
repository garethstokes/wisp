-- | Agent Core Module
-- Unified agent logic for loading, context assembly, and chat handling.
-- Agents are named personas with memory (soul) that can activate skills.
module Agents.Core
  ( Agent(..)
  , loadAgent
  , buildSystemPrompt
  , loadSkillPrompt
  ) where

import Data.Aeson (decode, encode)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Agent (AgentName, AgentConfig(..), agentTag, emptyAgentConfig)
import qualified Domain.Agent as DA
import Domain.Skill (SkillName, skillPromptTags)
import Domain.Soul (Soul(..))
import Domain.Activity (Activity(..), activityRaw, activityTitle)
import Domain.Id (EntityId)
import Skills.Registry (Skill(..), getSkill)
import Skills.Base (baseToolNames)
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
