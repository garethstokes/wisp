module Domain.SessionSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, toJSON)
import Data.ByteString.Lazy.Char8 (unpack)
import Domain.Session
import Domain.Chat (ChatMessage(..))

spec :: Spec
spec = describe "Session" $ do
  describe "JSON serialization" $ do
    it "serializes Session to JSON" $ do
      let session = Session
            { sessionId = SessionId "sess-123"
            , sessionAgentId = "wisp"
            , sessionMessages = []
            , sessionCreatedAt = read "2026-02-18 10:00:00 UTC"
            , sessionEndedAt = Nothing
            , sessionSummarized = False
            }
      unpack (encode session) `shouldContain` "\"agent_id\":\"wisp\""
