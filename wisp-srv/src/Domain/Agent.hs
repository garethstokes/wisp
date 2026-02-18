module Domain.Agent
  ( -- Skill capability metadata (used by Skills/*.hs)
    AgentInfo(..)
  , ToolInfo(..)
  , ToolType(..)
    -- Agent persona types (named agents with memory)
  , AgentName
  , AgentConfig(..)
  , agentTag
  , parseAgentTag
  , emptyAgentConfig
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, withText, (.:), (.:?), (.=))
import Data.Text (Text)
import qualified Data.Text as T

data ToolType = Deterministic | Decision
  deriving (Show, Eq)

instance ToJSON ToolType where
  toJSON Deterministic = "deterministic"
  toJSON Decision = "decision"

instance FromJSON ToolType where
  parseJSON = withText "ToolType" $ \case
    "deterministic" -> pure Deterministic
    "decision" -> pure Decision
    _ -> fail "Invalid ToolType"

data ToolInfo = ToolInfo
  { toolName :: Text
  , toolType :: ToolType
  } deriving (Show, Eq)

instance ToJSON ToolInfo where
  toJSON t = object ["name" .= toolName t, "type" .= toolType t]

instance FromJSON ToolInfo where
  parseJSON = withObject "ToolInfo" $ \v -> ToolInfo
    <$> v .: "name"
    <*> v .: "type"

data AgentInfo = AgentInfo
  { agentId          :: Text
  , agentDescription :: Text
  , agentTools       :: [ToolInfo]
  , agentWorkflows   :: [Text]
  , agentImplemented :: Bool
  } deriving (Show, Eq)

instance ToJSON AgentInfo where
  toJSON a = object
    [ "id" .= agentId a
    , "description" .= agentDescription a
    , "tools" .= agentTools a
    , "workflows" .= agentWorkflows a
    , "implemented" .= agentImplemented a
    ]

instance FromJSON AgentInfo where
  parseJSON = withObject "AgentInfo" $ \v -> AgentInfo
    <$> v .: "id"
    <*> v .: "description"
    <*> v .: "tools"
    <*> v .: "workflows"
    <*> v .: "implemented"

-- | Agent persona types
-- An agent is a named persona (like "jarvis") with its own personality,
-- memory (soul), and can activate skills.

-- | Agent name, e.g. "jarvis", "wisp"
type AgentName = Text

-- | Configuration for an agent persona, stored in knowledge as a Note
data AgentConfig = AgentConfig
  { agentPersonalitySeed :: Text        -- ^ Base personality description
  , agentActiveSkill     :: Maybe Text  -- ^ Currently active skill name
  } deriving (Eq, Show)

instance ToJSON AgentConfig where
  toJSON c = object
    [ "personality_seed" .= agentPersonalitySeed c
    , "active_skill" .= agentActiveSkill c
    ]

instance FromJSON AgentConfig where
  parseJSON = withObject "AgentConfig" $ \v -> AgentConfig
    <$> v .: "personality_seed"
    <*> v .:? "active_skill"

-- | Default empty agent config
emptyAgentConfig :: AgentConfig
emptyAgentConfig = AgentConfig
  { agentPersonalitySeed = ""
  , agentActiveSkill = Nothing
  }

-- | Create the tag for an agent definition note
-- e.g. agentTag "jarvis" = "agent:jarvis"
agentTag :: AgentName -> Text
agentTag name = "agent:" <> name

-- | Parse agent name from a tag like "agent:jarvis"
parseAgentTag :: Text -> Maybe AgentName
parseAgentTag t = case T.stripPrefix "agent:" t of
  Just name | not (T.null name) -> Just name
  _ -> Nothing
