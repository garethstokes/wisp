module Infra.Db.SessionSpec (spec) where

import Test.Hspec
import Data.Maybe (isJust)
import Control.Monad.IO.Class (liftIO)
import Domain.Session (Session(..), SessionId(..))
import Domain.Chat (ChatMessage(..))
import Infra.Db.Session
import TestEnv (withTestEnv, runTestApp)

spec :: Spec
spec = describe "Infra.Db.Session" $ around withTestEnv $ do
  describe "createSession" $ do
    it "creates a new session for an agent" $ \env -> runTestApp env $ do
      session <- createSession "wisp/concierge"
      liftIO $ sessionAgentId session `shouldBe` "wisp/concierge"
      liftIO $ sessionMessages session `shouldBe` []
      liftIO $ sessionSummarized session `shouldBe` False

  describe "appendMessage" $ do
    it "adds a message to a session" $ \env -> runTestApp env $ do
      session <- createSession "wisp/concierge"
      let msg = ChatMessage { messageRole = "user", messageContent = "Hello", messageAgent = Nothing, messageToolCall = Nothing }
      updated <- appendMessage (sessionId session) msg
      liftIO $ length (sessionMessages updated) `shouldBe` 1

  describe "getActiveSession" $ do
    it "returns active session for agent" $ \env -> runTestApp env $ do
      _ <- createSession "wisp/concierge"
      mSession <- getActiveSession "wisp/concierge"
      liftIO $ mSession `shouldSatisfy` isJust

  describe "endSession" $ do
    it "marks session as ended" $ \env -> runTestApp env $ do
      session <- createSession "wisp/concierge"
      endSession (sessionId session)
      mSession <- getSession (sessionId session)
      liftIO $ case mSession of
        Just s -> sessionEndedAt s `shouldSatisfy` isJust
        Nothing -> expectationFailure "Session not found"
