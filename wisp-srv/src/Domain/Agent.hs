module Domain.Agent
  ( AgentInfo(..)
  , ToolInfo(..)
  , ToolType(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, withText, (.:), (.=))
import Data.Text (Text)

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
