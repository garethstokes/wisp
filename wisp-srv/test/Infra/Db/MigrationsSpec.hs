module Infra.Db.MigrationsSpec where

import Test.Hspec
import Infra.Db.Migrations (parseMigrationNumber)

spec :: Spec
spec = describe "Migrations" $ do
  describe "parseMigrationNumber" $ do
    it "extracts number from migration filename" $ do
      parseMigrationNumber "001_entities.sql" `shouldBe` Just 1
      parseMigrationNumber "002_jobs.sql" `shouldBe` Just 2
      parseMigrationNumber "010_foo.sql" `shouldBe` Just 10

    it "returns Nothing for invalid filenames" $ do
      parseMigrationNumber "entities.sql" `shouldBe` Nothing
      parseMigrationNumber "abc_entities.sql" `shouldBe` Nothing
