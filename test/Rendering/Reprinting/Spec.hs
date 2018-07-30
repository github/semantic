module Rendering.Reprinting.Spec where

import SpecHelpers

import qualified Data.Language as Language
import Rendering.Reprinter
import Semantic.IO
import Data.Blob

spec :: Spec
spec = describe "reprinting" $

  it "should pass over a pristine tree" $ do
    let path = "test/fixtures/javascript/reprinting/map.json"
    src  <- blobSource <$> readBlobFromPath (File path Language.JSON)
    tree <- parseFile jsonParser "test/fixtures/javascript/reprinting/map.json"
    let tagged = mark Pristine tree
    let toks = reprint src tagged
    toks `shouldBe` [Chunk src]
