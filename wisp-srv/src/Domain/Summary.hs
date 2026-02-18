module Domain.Summary
  ( Summary(..)
  , SummaryId(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

newtype SummaryId = SummaryId { unSummaryId :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

data Summary = Summary
  { summaryId :: SummaryId
  , summaryAgentId :: Text
  , summarySessionIds :: [Text]  -- Session IDs as Text (not SessionId to avoid circular imports)
  , summaryContent :: Text
  , summaryCreatedAt :: UTCTime
  } deriving (Eq, Show)

instance ToJSON Summary where
  toJSON s = object
    [ "id" .= summaryId s
    , "agent_id" .= summaryAgentId s
    , "session_ids" .= summarySessionIds s
    , "content" .= summaryContent s
    , "created_at" .= summaryCreatedAt s
    ]

instance FromJSON Summary where
  parseJSON = withObject "Summary" $ \v -> Summary
    <$> v .: "id"
    <*> v .: "agent_id"
    <*> v .: "session_ids"
    <*> v .: "content"
    <*> v .: "created_at"
