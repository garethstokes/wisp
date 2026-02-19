-- test/Domain/DocumentSpec.hs
module Domain.DocumentSpec where

import Test.Hspec
import Domain.Document
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as LBS

spec :: Spec
spec = describe "Document" $ do
  describe "DocumentType" $ do
    it "serializes ProjectDoc to 'project'" $ do
      LBS.unpack (encode ProjectDoc) `shouldBe` "\"project\""

    it "serializes NoteDoc to 'note'" $ do
      LBS.unpack (encode NoteDoc) `shouldBe` "\"note\""

    it "serializes PreferenceDoc to 'preference'" $ do
      LBS.unpack (encode PreferenceDoc) `shouldBe` "\"preference\""

    it "deserializes 'project' to ProjectDoc" $ do
      decode "\"project\"" `shouldBe` Just ProjectDoc

  describe "ProjectType" $ do
    it "serializes Work to 'work'" $ do
      LBS.unpack (encode Work) `shouldBe` "\"work\""

    it "deserializes 'health' to Health" $ do
      decode "\"health\"" `shouldBe` Just Health

  describe "ProjectData" $ do
    it "serializes with name and type" $ do
      let proj = ProjectData "Gym" Health
          json = LBS.unpack (encode proj)
      json `shouldContain` "\"name\""
      json `shouldContain` "\"Gym\""
      json `shouldContain` "\"type\""
      json `shouldContain` "\"health\""

    it "deserializes from JSON" $ do
      let json = "{\"name\":\"Gym\",\"type\":\"health\"}"
          Just proj = decode json :: Maybe ProjectData
      projectName proj `shouldBe` "Gym"
      projectType proj `shouldBe` Health

  describe "NoteData" $ do
    it "serializes with title and content" $ do
      let note = NoteData "Alice is my sister" (Just "Works at Google")
          json = LBS.unpack (encode note)
      json `shouldContain` "\"title\""
      json `shouldContain` "\"Alice is my sister\""

    it "handles null content" $ do
      let note = NoteData "Simple note" Nothing
          json = encode note
          Just decoded = decode json :: Maybe NoteData
      noteContent decoded `shouldBe` Nothing

  describe "PreferenceData" $ do
    it "serializes with key, value, context" $ do
      let pref = PreferenceData "meeting_time" "10am-12pm" (Just "scheduling")
          json = LBS.unpack (encode pref)
      json `shouldContain` "\"key\""
      json `shouldContain` "\"meeting_time\""

  describe "LogSource" $ do
    it "serializes LogUser to 'user'" $ do
      LBS.unpack (encode LogUser) `shouldBe` "\"user\""

    it "deserializes 'agent' to LogAgent" $ do
      decode "\"agent\"" `shouldBe` Just LogAgent
