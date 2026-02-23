module Domain.RunSpec where

import Test.Hspec
import Data.Aeson (encode, decode, object, (.=))
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Domain.Run

-- Helper for consistent timestamps in tests
testTime :: UTCTime
testTime = posixSecondsToUTCTime 1700000000

spec :: Spec
spec = describe "Run" $ do
  describe "RunStatus" $ do
    it "serializes Running to JSON" $ do
      encode Running `shouldBe` "\"running\""

    it "serializes Waiting to JSON" $ do
      encode Waiting `shouldBe` "\"waiting\""

    it "serializes Completed to JSON" $ do
      encode Completed `shouldBe` "\"completed\""

    it "serializes Failed to JSON" $ do
      encode Failed `shouldBe` "\"failed\""

    it "round-trips through JSON" $ do
      decode (encode Running) `shouldBe` Just Running
      decode (encode Waiting) `shouldBe` Just Waiting
      decode (encode Completed) `shouldBe` Just Completed
      decode (encode Failed) `shouldBe` Just Failed

  describe "RunId" $ do
    it "serializes to JSON" $ do
      encode (RunId "abc123") `shouldBe` "\"abc123\""

    it "round-trips through JSON" $ do
      decode (encode (RunId "test")) `shouldBe` Just (RunId "test")

  describe "RunEvent" $ do
    it "serializes InputEvent to JSON with type field" $ do
      let event = InputEvent
            { eventId = "evt1"
            , eventParentEventId = Nothing
            , eventTimestamp = testTime
            , eventTool = "message_from_user"
            , eventData = object ["content" .= ("hello" :: String)]
            }
      let json = LBS.unpack (encode event)
      json `shouldContain` "\"type\":\"input\""
      json `shouldContain` "\"tool\":\"message_from_user\""

    it "serializes ToolRequested to JSON" $ do
      let event = ToolRequested
            { eventId = "evt2"
            , eventParentEventId = Just "evt1"
            , eventTimestamp = testTime
            , eventToolName = "query_activities"
            , eventToolArgs = object ["status" .= ("quarantined" :: String)]
            }
      let json = LBS.unpack (encode event)
      json `shouldContain` "\"type\":\"tool_requested\""
      json `shouldContain` "\"parent_event_id\":\"evt1\""

    it "serializes ToolSucceeded to JSON" $ do
      let event = ToolSucceeded
            { eventId = "evt3"
            , eventParentEventId = Just "evt2"
            , eventTimestamp = testTime
            , eventToolName = "query_activities"
            , eventResult = object ["count" .= (5 :: Int)]
            }
      let json = LBS.unpack (encode event)
      json `shouldContain` "\"type\":\"tool_succeeded\""

    it "serializes ToolFailed to JSON" $ do
      let event = ToolFailed
            { eventId = "evt4"
            , eventParentEventId = Just "evt2"
            , eventTimestamp = testTime
            , eventToolName = "query_activities"
            , eventError = "Database connection failed"
            }
      let json = LBS.unpack (encode event)
      json `shouldContain` "\"type\":\"tool_failed\""
      json `shouldContain` "\"error\":\"Database connection failed\""

    it "serializes ContextAssembled to JSON" $ do
      let event = ContextAssembled
            { eventId = "evt5"
            , eventParentEventId = Nothing
            , eventTimestamp = testTime
            , eventContext = object ["items" .= (3 :: Int)]
            }
      let json = LBS.unpack (encode event)
      json `shouldContain` "\"type\":\"context_assembled\""
      json `shouldContain` "\"context\":"

    it "serializes LlmCalled to JSON" $ do
      let event = LlmCalled
            { eventId = "evt6"
            , eventParentEventId = Nothing
            , eventTimestamp = testTime
            , eventModel = "claude-3-5-sonnet"
            , eventSystemPrompt = "You are a helpful assistant"
            , eventUserPrompt = "Hello"
            , eventRawResponse = "Hi there!"
            , eventInputTokens = Just 100
            , eventOutputTokens = Just 50
            }
      let json = LBS.unpack (encode event)
      json `shouldContain` "\"type\":\"llm_called\""
      json `shouldContain` "\"model\":\"claude-3-5-sonnet\""

  describe "eventTypeToText" $ do
    it "returns correct type strings" $ do
      eventTypeToText (InputEvent "x" Nothing testTime "t" (object [])) `shouldBe` "input"
      eventTypeToText (ContextAssembled "x" Nothing testTime (object [])) `shouldBe` "context_assembled"
      eventTypeToText (LlmCalled "x" Nothing testTime "m" "s" "u" "r" Nothing Nothing) `shouldBe` "llm_called"
      eventTypeToText (ToolRequested "x" Nothing testTime "t" (object [])) `shouldBe` "tool_requested"
      eventTypeToText (ToolSucceeded "x" Nothing testTime "t" (object [])) `shouldBe` "tool_succeeded"
      eventTypeToText (ToolFailed "x" Nothing testTime "t" "e") `shouldBe` "tool_failed"

  describe "Run" $ do
    it "serializes to JSON with all fields" $ do
      let run = Run
            { runId = RunId "run123"
            , runParentRunId = Nothing
            , runAgent = "wisp"
            , runSessionId = Just "default"
            , runCreatedAt = testTime
            , runUpdatedAt = testTime
            , runStatus = Completed
            , runEvents = []
            }
      let json = LBS.unpack (encode run)
      json `shouldContain` "\"id\":\"run123\""
      json `shouldContain` "\"agent\":\"wisp\""
      json `shouldContain` "\"status\":\"completed\""
