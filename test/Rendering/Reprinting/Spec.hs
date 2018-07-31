{-# LANGUAGE OverloadedLists #-}

module Rendering.Reprinting.Spec where

import SpecHelpers

import qualified Data.Language as Language
import Rendering.Reprinter
import Semantic.IO
import Data.Blob

setup = do
  let path = "test/fixtures/javascript/reprinting/map.json"
  src  <- blobSource <$> readBlobFromPath (File path Language.JSON)
  tree <- parseFile jsonParser "test/fixtures/javascript/reprinting/map.json"
  pure (src, tree)

spec :: Spec
spec = describe "reprinting" $ do

  it "should pass over a pristine tree" $ do
    (src, tree) <- setup
    let tagged = mark Pristine tree
    let toks = reprint src tagged
    toks `shouldBe` [Chunk src]

  it "should emit control tokens but only 1 chunk for a wholly-modified tree" $ do
    (src, tree) <- fmap (fmap (mark Modified))setup
    let toks = reprint src tree
    forM_ @[] [List, Associative] $ \t -> do
      toks `shouldSatisfy` (elem (TControl (Enter t)))
      toks `shouldSatisfy` (elem (TControl (Exit t)))
