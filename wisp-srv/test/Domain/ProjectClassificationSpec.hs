module Domain.ProjectClassificationSpec where

import Test.Hspec
import Data.Aeson (decode)
import Domain.ProjectClassification

spec :: Spec
spec = describe "ProjectClassification" $ do
  describe "JSON parsing" $ do
    it "parses project assignment with confidence" $ do
      let json = "{\"name\":\"wisp\",\"confidence\":0.9}"
          Just pc = decode json :: Maybe ProjectAssignment
      paName pc `shouldBe` "wisp"
      paConfidence pc `shouldBe` 0.9

    it "parses list of project assignments" $ do
      let json = "[{\"name\":\"wisp\",\"confidence\":0.9},{\"name\":\"superit\",\"confidence\":0.6}]"
          Just pcs = decode json :: Maybe [ProjectAssignment]
      length pcs `shouldBe` 2
