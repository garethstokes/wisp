module Infra.Google.GmailSpec where

import Test.Hspec
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Infra.Google.Gmail

spec :: Spec
spec = describe "Gmail API" $ do
  describe "GmailMessage parsing" $ do
    it "parses a minimal message response" $ do
      let json = LBS.pack "{\"id\":\"msg123\",\"threadId\":\"t456\"}"
      case decode json :: Maybe GmailMessage of
        Nothing -> expectationFailure "Failed to parse minimal message"
        Just msg -> do
          gmailId msg `shouldBe` "msg123"
          gmailThreadId msg `shouldBe` "t456"

    it "parses a message with optional fields" $ do
      let json = LBS.pack $ concat
            [ "{\"id\":\"msg123\",\"threadId\":\"t456\""
            , ",\"labelIds\":[\"INBOX\",\"UNREAD\"]"
            , ",\"snippet\":\"Hello world\""
            , ",\"internalDate\":\"1234567890000\""
            , "}"
            ]
      case decode json :: Maybe GmailMessage of
        Nothing -> expectationFailure "Failed to parse message with optional fields"
        Just msg -> do
          gmailId msg `shouldBe` "msg123"
          gmailLabelIds msg `shouldBe` Just ["INBOX", "UNREAD"]
          gmailSnippet msg `shouldBe` Just "Hello world"
          gmailInternalDate msg `shouldBe` Just "1234567890000"

  describe "GmailMessageList parsing" $ do
    it "parses an empty message list" $ do
      let json = LBS.pack "{}"
      case decode json :: Maybe GmailMessageList of
        Nothing -> expectationFailure "Failed to parse empty list"
        Just lst -> do
          messages lst `shouldBe` Nothing
          nextPageToken lst `shouldBe` Nothing

    it "parses a message list with messages" $ do
      let json = LBS.pack $ concat
            [ "{\"messages\":[{\"id\":\"m1\",\"threadId\":\"t1\"}]"
            , ",\"nextPageToken\":\"token123\""
            , ",\"resultSizeEstimate\":100"
            , "}"
            ]
      case decode json :: Maybe GmailMessageList of
        Nothing -> expectationFailure "Failed to parse message list"
        Just lst -> do
          case messages lst of
            Nothing -> expectationFailure "Expected messages"
            Just msgs -> do
              length msgs `shouldBe` 1
              refId (head msgs) `shouldBe` "m1"
          nextPageToken lst `shouldBe` Just "token123"
          resultSizeEstimate lst `shouldBe` Just 100

  describe "GmailMessageRef parsing" $ do
    it "parses a message reference" $ do
      let json = LBS.pack "{\"id\":\"ref1\",\"threadId\":\"thread1\"}"
      case decode json :: Maybe GmailMessageRef of
        Nothing -> expectationFailure "Failed to parse message ref"
        Just ref -> do
          refId ref `shouldBe` "ref1"
          refThreadId ref `shouldBe` "thread1"

  describe "GmailPayload parsing" $ do
    it "parses a payload with headers" $ do
      let json = LBS.pack $ concat
            [ "{\"headers\":[{\"name\":\"Subject\",\"value\":\"Test\"}]"
            , ",\"mimeType\":\"text/plain\""
            , ",\"body\":{\"size\":100,\"data\":\"SGVsbG8=\"}"
            , "}"
            ]
      case decode json :: Maybe GmailPayload of
        Nothing -> expectationFailure "Failed to parse payload"
        Just payload -> do
          payloadMimeType payload `shouldBe` Just "text/plain"
          case payloadHeaders payload of
            Nothing -> expectationFailure "Expected headers"
            Just headers -> do
              length headers `shouldBe` 1
              headerName (head headers) `shouldBe` "Subject"
              headerValue (head headers) `shouldBe` "Test"
          case payloadBody payload of
            Nothing -> expectationFailure "Expected body"
            Just body -> do
              bodySize body `shouldBe` 100
              bodyData body `shouldBe` Just "SGVsbG8="

  describe "GmailHistoryList parsing" $ do
    it "parses an empty history list" $ do
      let json = LBS.pack "{\"historyId\":\"12345\"}"
      case decode json :: Maybe GmailHistoryList of
        Nothing -> expectationFailure "Failed to parse empty history"
        Just hist -> do
          history hist `shouldBe` Nothing
          historyId hist `shouldBe` Just "12345"

    it "parses a history list with records" $ do
      let json = LBS.pack $ concat
            [ "{\"history\":[{\"id\":\"h1\",\"messagesAdded\":[{\"message\":{\"id\":\"m1\",\"threadId\":\"t1\"}}]}]"
            , ",\"historyId\":\"12345\""
            , "}"
            ]
      case decode json :: Maybe GmailHistoryList of
        Nothing -> expectationFailure "Failed to parse history list"
        Just histList -> do
          historyId histList `shouldBe` Just "12345"
          case history histList of
            Nothing -> expectationFailure "Expected history records"
            Just records -> do
              length records `shouldBe` 1
              historyRecordId (head records) `shouldBe` "h1"
              case historyMessagesAdded (head records) of
                Nothing -> expectationFailure "Expected messagesAdded"
                Just added -> do
                  length added `shouldBe` 1
                  refId (historyMessage (head added)) `shouldBe` "m1"
