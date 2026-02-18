module Domain.Session
  ( Session(..)
  , SessionId(..)
  , NewSession(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), (.:?))
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Chat (ChatMessage)
import GHC.Generics (Generic)

newtype SessionId = SessionId { unSessionId :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

data Session = Session
  { sessionId :: SessionId
  , sessionAgentId :: Text
  , sessionMessages :: [ChatMessage]
  , sessionCreatedAt :: UTCTime
  , sessionEndedAt :: Maybe UTCTime
  , sessionSummarized :: Bool
  } deriving (Eq, Show)

instance ToJSON Session where
  toJSON s = object
    [ "id" .= sessionId s
    , "agent_id" .= sessionAgentId s
    , "messages" .= sessionMessages s
    , "created_at" .= sessionCreatedAt s
    , "ended_at" .= sessionEndedAt s
    , "summarized" .= sessionSummarized s
    ]

instance FromJSON Session where
  parseJSON = withObject "Session" $ \v -> Session
    <$> v .: "id"
    <*> v .: "agent_id"
    <*> v .: "messages"
    <*> v .: "created_at"
    <*> v .:? "ended_at"
    <*> v .: "summarized"

data NewSession = NewSession
  { newSessionAgentId :: Text
  } deriving (Eq, Show)
