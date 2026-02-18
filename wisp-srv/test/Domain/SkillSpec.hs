module Domain.SkillSpec (spec) where

import Test.Hspec
import Domain.Skill

spec :: Spec
spec = describe "Skill" $ do
  it "parses skill name from tag" $ do
    parseSkillTag "skill:concierge" `shouldBe` Just "concierge"
    parseSkillTag "skill:scheduler" `shouldBe` Just "scheduler"
    parseSkillTag "other" `shouldBe` Nothing

  it "creates skill tag from name" $ do
    skillTag "concierge" `shouldBe` "skill:concierge"

  it "creates skill prompt tags" $ do
    skillPromptTags "concierge" `shouldBe` ["skill:concierge", "prompt"]
