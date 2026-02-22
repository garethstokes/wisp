-- wisp-core/src/Wisp/Client/Types.hs
module Wisp.Client.Types
  ( ClientConfig(..)
  , ClientError(..)
  , defaultConfig
    -- * Skill types
  , Skill(..)
    -- * Agent types
  , AgentInfo(..)
  , AgentSoul(..)
    -- * Session types
  , SessionSummary(..)
  , ActiveSession(..)
  , ChatMessage(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value, withObject, (.:), (.:?), object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)

data ClientConfig = ClientConfig
  { configBaseUrl :: Text
  , configTimeout :: Int  -- seconds
  } deriving (Show, Eq)

defaultConfig :: ClientConfig
defaultConfig = ClientConfig
  { configBaseUrl = "http://127.0.0.1:5812"
  , configTimeout = 30
  }

data ClientError
  = HttpError Text
  | ParseError Text
  | ServerError Int Text
  deriving (Show, Eq)

-- | Skill info from the API
data Skill = Skill
  { skillName :: Text
  , skillTools :: [Text]
  , skillAvailable :: Bool
  } deriving (Show, Eq)

instance FromJSON Skill where
  parseJSON = withObject "Skill" $ \v -> Skill
    <$> v .: "name"
    <*> v .: "tools"
    <*> v .: "available"

instance ToJSON Skill where
  toJSON s = object
    [ "name" .= skillName s
    , "tools" .= skillTools s
    , "available" .= skillAvailable s
    ]

-- | Agent info from the API
data AgentInfo = AgentInfo
  { agentName :: Text
  , agentPersonality :: Text
  , agentActiveSkill :: Maybe Text
  , agentSoul :: AgentSoul
  , agentAvailableSkills :: [Text]
  } deriving (Show, Eq)

instance FromJSON AgentInfo where
  parseJSON = withObject "AgentInfo" $ \v -> AgentInfo
    <$> v .: "name"
    <*> v .: "personality"
    <*> v .:? "active_skill"
    <*> v .: "soul"
    <*> v .: "available_skills"

instance ToJSON AgentInfo where
  toJSON a = object
    [ "name" .= agentName a
    , "personality" .= agentPersonality a
    , "active_skill" .= agentActiveSkill a
    , "soul" .= agentSoul a
    , "available_skills" .= agentAvailableSkills a
    ]

-- | Agent soul (personality evolution)
data AgentSoul = AgentSoul
  { soulPersonality :: Text
  , soulInsights :: [Text]
  } deriving (Show, Eq)

instance FromJSON AgentSoul where
  parseJSON = withObject "AgentSoul" $ \v -> AgentSoul
    <$> v .: "personality"
    <*> v .: "insights"

instance ToJSON AgentSoul where
  toJSON s = object
    [ "personality" .= soulPersonality s
    , "insights" .= soulInsights s
    ]

-- | Session summary for list display
data SessionSummary = SessionSummary
  { sessionId :: Text
  , sessionAgentId :: Text
  , sessionMessageCount :: Int
  , sessionCreatedAt :: UTCTime
  , sessionEndedAt :: Maybe UTCTime
  } deriving (Show, Eq)

instance FromJSON SessionSummary where
  parseJSON = withObject "SessionSummary" $ \v -> do
    sid <- v .: "id"
    agentId <- v .: "agent_id"
    messages <- v .: "messages"
    createdAt <- v .: "created_at"
    endedAt <- v .:? "ended_at"
    pure $ SessionSummary sid agentId (length (messages :: [Value])) createdAt endedAt

instance ToJSON SessionSummary where
  toJSON s = object
    [ "id" .= sessionId s
    , "agent_id" .= sessionAgentId s
    , "message_count" .= sessionMessageCount s
    , "created_at" .= sessionCreatedAt s
    , "ended_at" .= sessionEndedAt s
    ]

-- | Chat message (for session messages)
data ChatMessage = ChatMessage
  { cmRole :: Text
  , cmContent :: Text
  , cmAgent :: Maybe Text
  } deriving (Show, Eq)

instance FromJSON ChatMessage where
  parseJSON = withObject "ChatMessage" $ \v -> ChatMessage
    <$> v .: "role"
    <*> v .: "content"
    <*> v .:? "agent"

instance ToJSON ChatMessage where
  toJSON m = object
    [ "role" .= cmRole m
    , "content" .= cmContent m
    , "agent" .= cmAgent m
    ]

-- | Active session with full messages (for TUI resume)
data ActiveSession = ActiveSession
  { asId :: Text
  , asAgentId :: Text
  , asMessages :: [ChatMessage]
  , asCreatedAt :: UTCTime
  , asLastMessageAt :: UTCTime
  } deriving (Show, Eq)

instance FromJSON ActiveSession where
  parseJSON = withObject "ActiveSession" $ \v -> ActiveSession
    <$> v .: "id"
    <*> v .: "agent_id"
    <*> v .: "messages"
    <*> v .: "created_at"
    <*> v .: "last_message_at"

instance ToJSON ActiveSession where
  toJSON s = object
    [ "id" .= asId s
    , "agent_id" .= asAgentId s
    , "messages" .= asMessages s
    , "created_at" .= asCreatedAt s
    , "last_message_at" .= asLastMessageAt s
    ]
