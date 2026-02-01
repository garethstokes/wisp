module Domain.IdSpec where

import Test.Hspec
import Domain.Id
import Data.Aeson (encode, decode)
import qualified Data.Text as T

spec :: Spec
spec = describe "EntityId" $ do
  it "generates 12-character IDs" $ do
    eid <- newEntityId
    T.length (unEntityId eid) `shouldBe` 12

  it "round-trips through JSON" $ do
    eid <- newEntityId
    decode (encode eid) `shouldBe` Just eid
