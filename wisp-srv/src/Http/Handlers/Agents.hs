module Http.Handlers.Agents
  ( -- Legacy endpoint
    getAgents
    -- New agent endpoints
  , getAgentsList
  , getAgentByName
  , postAgentChat
  , postAgentActivateSkill
  , postAgentDeactivate
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status400, status404)
import Web.Scotty.Trans (ActionT, json, status, jsonData, captureParam)
import App.Monad (Env)
import Agents.Dispatcher (allAgents, listAgentNames)
import Agents.Core (Agent(..), loadAgent, buildSystemPrompt, loadSkillPrompt)
import Skills.Base (activateAgentSkill, deactivateAgentSkill)
import Skills.Registry (Skill(..), allSkillNames)
import Domain.Agent (AgentConfig(..))
import Domain.Soul (Soul(..))
import Domain.Chat (ChatMessage)
import Domain.Id (EntityId(..))

-- | Default account ID for now (TODO: get from auth)
defaultAccount :: EntityId
defaultAccount = EntityId "default"

--------------------------------------------------------------------------------
-- Legacy endpoint
--------------------------------------------------------------------------------

-- GET /agents (legacy - returns skill-based agents)
getAgents :: ActionT (ReaderT Env IO) ()
getAgents = json $ object ["agents" .= allAgents]

--------------------------------------------------------------------------------
-- New Agent Endpoints
--------------------------------------------------------------------------------

-- | GET /api/agents - list agent names from knowledge
getAgentsList :: ActionT (ReaderT Env IO) ()
getAgentsList = do
  names <- lift $ listAgentNames defaultAccount
  json $ object
    [ "agents" .= names
    , "count" .= length names
    ]

-- | GET /api/agents/:name - get agent details
getAgentByName :: ActionT (ReaderT Env IO) ()
getAgentByName = do
  name <- captureParam "name"
  mAgent <- lift $ loadAgent defaultAccount name
  case mAgent of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Agent not found: " <> name :: Text)]
    Just agent -> do
      json $ agentToJson agent

-- | Agent JSON representation
agentToJson :: Agent -> Value
agentToJson agent = object
  [ "name" .= agentName agent
  , "personality" .= agentPersonalitySeed (agentConfig agent)
  , "active_skill" .= fmap skillName (agentSkill agent)
  , "soul" .= soulToJson (agentSoul agent)
  , "available_skills" .= allSkillNames
  ]

soulToJson :: Soul -> Value
soulToJson soul = object
  [ "personality" .= soulPersonality soul
  , "insights" .= soulInsights soul
  ]

-- | Request body for agent chat
data AgentChatRequest = AgentChatRequest
  { agentChatMessages :: [ChatMessage]
  , agentChatTimezone :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON AgentChatRequest
instance ToJSON AgentChatRequest

-- | POST /api/agents/:name/chat - chat with an agent
postAgentChat :: ActionT (ReaderT Env IO) ()
postAgentChat = do
  name <- captureParam "name"
  _req <- jsonData :: ActionT (ReaderT Env IO) AgentChatRequest

  mAgent <- lift $ loadAgent defaultAccount name
  case mAgent of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Agent not found: " <> name :: Text)]
    Just agent -> do
      -- Load skill prompt if skill is active
      mSkillPrompt <- case agentSkill agent of
        Nothing -> pure Nothing
        Just skill -> lift $ loadSkillPrompt defaultAccount (skillName skill)

      let systemPrompt = buildSystemPrompt agent mSkillPrompt

      -- TODO: Call LLM with assembled prompt and handle tool calls
      -- For now, return the assembled context
      json $ object
        [ "status" .= ("not_implemented" :: Text)
        , "agent" .= agentName agent
        , "active_skill" .= fmap skillName (agentSkill agent)
        , "system_prompt_preview" .= take 500 (show systemPrompt)
        ]

-- | Request body for skill activation
data ActivateSkillRequest = ActivateSkillRequest
  { activateConfirm :: Bool  -- User must confirm
  } deriving (Show, Eq, Generic)

instance FromJSON ActivateSkillRequest
instance ToJSON ActivateSkillRequest

-- | POST /api/agents/:name/activate/:skill - activate a skill
postAgentActivateSkill :: ActionT (ReaderT Env IO) ()
postAgentActivateSkill = do
  name <- captureParam "name"
  skillToActivate <- captureParam "skill"
  req <- jsonData :: ActionT (ReaderT Env IO) ActivateSkillRequest

  if not (activateConfirm req)
    then do
      status status400
      json $ object
        [ "error" .= ("Confirmation required" :: Text)
        , "message" .= ("Set confirm: true to activate skill" :: Text)
        ]
    else do
      result <- lift $ activateAgentSkill defaultAccount name skillToActivate
      case result of
        Left err -> do
          status status400
          json $ object ["error" .= err]
        Right () -> do
          json $ object
            [ "status" .= ("activated" :: Text)
            , "agent" .= name
            , "skill" .= skillToActivate
            ]

-- | POST /api/agents/:name/deactivate - deactivate current skill
postAgentDeactivate :: ActionT (ReaderT Env IO) ()
postAgentDeactivate = do
  name <- captureParam "name"

  result <- lift $ deactivateAgentSkill defaultAccount name
  case result of
    Left err -> do
      status status400
      json $ object ["error" .= err]
    Right () -> do
      json $ object
        [ "status" .= ("deactivated" :: Text)
        , "agent" .= name
        ]
