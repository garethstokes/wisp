module Domain.Chat
  ( ChatContext(..)
  , ChatMessage(..)
  , ChatRequest(..)
  , ChatResponse(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), withObject, (.:), (.:?))
import Data.Text (Text)
import Domain.Activity (Activity)
import Domain.Person (Person)

-- Context assembled for the LLM
data ChatContext = ChatContext
  { contextCalendarEvents :: [Activity]
  , contextRecentActivities :: [Activity]
  , contextPendingEmails :: [Activity]
  , contextQuarantined :: [Activity]
  , contextSurfaced :: [Activity]
  , contextNeedsReview :: [Activity]
  , contextMentionedPeople :: [Person]
  } deriving (Show)

-- Single message in conversation
data ChatMessage = ChatMessage
  { messageRole    :: Text        -- "user" | "assistant" | "tool"
  , messageContent :: Text
  , messageAgent   :: Maybe Text  -- Which agent responded (for assistant messages)
  , messageToolCall :: Maybe Value
  } deriving (Show, Eq)

instance FromJSON ChatMessage where
  parseJSON = withObject "ChatMessage" $ \v -> ChatMessage
    <$> v .: "role"
    <*> v .: "content"
    <*> v .:? "agent"
    <*> v .:? "tool_call"

instance ToJSON ChatMessage where
  toJSON m = object $
    [ "role" .= messageRole m
    , "content" .= messageContent m
    ]
    ++ maybe [] (\a -> ["agent" .= a]) (messageAgent m)
    ++ maybe [] (\tc -> ["tool_call" .= tc]) (messageToolCall m)

-- Incoming chat request (updated)
data ChatRequest = ChatRequest
  { chatAgent    :: Text
  , chatMessages :: [ChatMessage]
  , chatTimezone :: Maybe Text  -- IANA timezone for local time conversion
  } deriving (Show)

instance FromJSON ChatRequest where
  parseJSON = withObject "ChatRequest" $ \v -> ChatRequest
    <$> v .: "agent"
    <*> v .: "messages"
    <*> v .:? "timezone"

-- Chat response (updated)
data ChatResponse = ChatResponse
  { responseMessage  :: Text
  , responseToolCall :: Maybe Value
  } deriving (Show, Eq)

instance ToJSON ChatResponse where
  toJSON r = object $
    [ "message" .= responseMessage r
    ] ++ maybe [] (\tc -> ["tool_call" .= tc]) (responseToolCall r)

instance FromJSON ChatResponse where
  parseJSON = withObject "ChatResponse" $ \v -> ChatResponse
    <$> v .: "message"
    <*> v .:? "tool_call"
