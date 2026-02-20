module Domain.ChatEvent
  ( ChatEvent(..)
  , ToolCallInfo(..)
  , ToolResultInfo(..)
  , chatEventToSSE
  ) where

import Data.Aeson (ToJSON(..), object, (.=), encode)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value)

data ToolCallInfo = ToolCallInfo
  { toolCallName :: Text
  , toolCallArgs :: Value
  } deriving (Show, Eq)

instance ToJSON ToolCallInfo where
  toJSON t = object ["tool" .= toolCallName t, "args" .= toolCallArgs t]

data ToolResultInfo = ToolResultInfo
  { toolResultName :: Text
  , toolResultValue :: Value
  , toolResultDurationMs :: Int
  } deriving (Show, Eq)

instance ToJSON ToolResultInfo where
  toJSON t = object
    [ "tool" .= toolResultName t
    , "result" .= toolResultValue t
    , "duration_ms" .= toolResultDurationMs t
    ]

data ChatEvent
  = ChunkEvent Text
  | ToolCallStartEvent ToolCallInfo
  | ToolCallResultEvent ToolResultInfo
  | DoneEvent Text Int  -- session_id, token_count
  | ErrorEvent Text Text  -- message, code
  deriving (Show, Eq)

instance ToJSON ChatEvent where
  toJSON (ChunkEvent txt) = object ["text" .= txt]
  toJSON (ToolCallStartEvent info) = toJSON info
  toJSON (ToolCallResultEvent info) = toJSON info
  toJSON (DoneEvent sid tokens) = object ["session_id" .= sid, "token_count" .= tokens]
  toJSON (ErrorEvent msg code) = object ["message" .= msg, "code" .= code]

-- | Format a ChatEvent as SSE
chatEventToSSE :: ChatEvent -> BL.ByteString
chatEventToSSE evt =
  let (eventName, payload) = case evt of
        ChunkEvent _ -> ("chunk", encode evt)
        ToolCallStartEvent _ -> ("tool_call_start", encode evt)
        ToolCallResultEvent _ -> ("tool_call_result", encode evt)
        DoneEvent _ _ -> ("done", encode evt)
        ErrorEvent _ _ -> ("error", encode evt)
  in BL.fromStrict $ TE.encodeUtf8 $
       "event: " <> eventName <> "\n" <>
       "data: " <> TE.decodeUtf8 (BL.toStrict payload) <> "\n\n"
