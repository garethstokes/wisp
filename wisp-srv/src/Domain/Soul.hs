module Domain.Soul
  ( Soul(..)
  , emptySoul
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:))
import Data.Text (Text)
import Data.Time (UTCTime)

data Soul = Soul
  { soulAgentId :: Text
  , soulPersonality :: Text
  , soulInsights :: [Text]
  , soulUpdatedAt :: UTCTime
  } deriving (Eq, Show)

instance ToJSON Soul where
  toJSON s = object
    [ "agent_id" .= soulAgentId s
    , "personality" .= soulPersonality s
    , "insights" .= soulInsights s
    , "updated_at" .= soulUpdatedAt s
    ]

instance FromJSON Soul where
  parseJSON = withObject "Soul" $ \v -> Soul
    <$> v .: "agent_id"
    <*> v .: "personality"
    <*> v .: "insights"
    <*> v .: "updated_at"

-- Create an empty soul for an agent
emptySoul :: Text -> Soul
emptySoul agentId = Soul
  { soulAgentId = agentId
  , soulPersonality = ""
  , soulInsights = []
  , soulUpdatedAt = read "1970-01-01 00:00:00 UTC"
  }
