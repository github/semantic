module Command.Spec where

import Command
import Prologue hiding (readFile)
import Source
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "readFile" $ do
    it "returns a blob for extant files" $ do
      blob <- runCommand (readFile "semantic-diff.cabal")
      fmap path blob `shouldBe` Just "semantic-diff.cabal"
