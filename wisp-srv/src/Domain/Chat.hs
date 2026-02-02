module Domain.Chat
  ( ChatContext(..)
  , ChatRequest(..)
  , ChatResponse(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), withObject, (.:))
import Data.Text (Text)
import Domain.Activity (Activity)
import Domain.Person (Person)

-- Context assembled for the LLM
data ChatContext = ChatContext
  { contextCalendarEvents :: [Activity]
  , contextRecentActivities :: [Activity]
  , contextPendingEmails :: [Activity]
  , contextQuarantineCount :: Int
  , contextSurfacedCount :: Int
  , contextMentionedPeople :: [Person]
  } deriving (Show)

-- Incoming chat request
data ChatRequest = ChatRequest
  { chatMessage :: Text
  } deriving (Show)

instance FromJSON ChatRequest where
  parseJSON = withObject "ChatRequest" $ \v ->
    ChatRequest <$> v .: "message"

-- Chat response
data ChatResponse = ChatResponse
  { responseMessage :: Text
  } deriving (Show)

instance ToJSON ChatResponse where
  toJSON r = object ["message" .= responseMessage r]
