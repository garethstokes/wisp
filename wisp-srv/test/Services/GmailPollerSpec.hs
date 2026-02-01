module Services.GmailPollerSpec where

import Test.Hspec
import Services.GmailPoller (extractEmailInfo)
import Infra.Google.Gmail (GmailMessage(..), GmailPayload(..), GmailHeader(..))

spec :: Spec
spec = describe "GmailPoller" $ do
  describe "extractEmailInfo" $ do
    it "extracts subject and sender from headers" $ do
      let headers = Just
            [ GmailHeader "Subject" "Test Subject"
            , GmailHeader "From" "sender@example.com"
            ]
      let payload = Just $ GmailPayload headers Nothing Nothing
      let msg = GmailMessage "id1" "t1" Nothing Nothing payload Nothing Nothing
      let (subject, sender) = extractEmailInfo msg
      subject `shouldBe` Just "Test Subject"
      sender `shouldBe` Just "sender@example.com"

    it "returns Nothing when headers missing" $ do
      let msg = GmailMessage "id1" "t1" Nothing Nothing Nothing Nothing Nothing
      let (subject, sender) = extractEmailInfo msg
      subject `shouldBe` Nothing
      sender `shouldBe` Nothing

    it "returns Nothing when payload has no headers" $ do
      let payload = Just $ GmailPayload Nothing Nothing Nothing
      let msg = GmailMessage "id1" "t1" Nothing Nothing payload Nothing Nothing
      let (subject, sender) = extractEmailInfo msg
      subject `shouldBe` Nothing
      sender `shouldBe` Nothing

    it "returns partial result when only subject present" $ do
      let headers = Just [ GmailHeader "Subject" "Only Subject" ]
      let payload = Just $ GmailPayload headers Nothing Nothing
      let msg = GmailMessage "id1" "t1" Nothing Nothing payload Nothing Nothing
      let (subject, sender) = extractEmailInfo msg
      subject `shouldBe` Just "Only Subject"
      sender `shouldBe` Nothing

    it "returns partial result when only sender present" $ do
      let headers = Just [ GmailHeader "From" "only@sender.com" ]
      let payload = Just $ GmailPayload headers Nothing Nothing
      let msg = GmailMessage "id1" "t1" Nothing Nothing payload Nothing Nothing
      let (subject, sender) = extractEmailInfo msg
      subject `shouldBe` Nothing
      sender `shouldBe` Just "only@sender.com"
