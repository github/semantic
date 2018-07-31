{-# LANGUAGE OverloadedLists #-}

module Reprinting.Spec where

import SpecHelpers

import qualified Data.Language as Language
import Reprinting.Algebra
import Reprinting.Pipeline
import Semantic.IO
import Data.Blob

spec :: Spec
spec = describe "reprinting" $ do
  (src, tree) <- runIO $ do
    let path = "test/fixtures/javascript/reprinting/map.json"
    src  <- blobSource <$> readBlobFromPath (File path Language.JSON)
    tree <- parseFile jsonParser "test/fixtures/javascript/reprinting/map.json"
    pure (src, tree)

  describe "tokenization" $ do

    it "should pass over a pristine tree" $ do
      let tagged = mark Pristine tree
      let toks = reprint src tagged
      toks `shouldBe` [Chunk src]

    it "should emit control tokens but only 1 chunk for a wholly-modified tree" $ do
      let toks = reprint src (mark Modified tree)
      forM_ @[] [List, Associative] $ \t -> do
        toks `shouldSatisfy` (elem (TControl (Enter t)))
        toks `shouldSatisfy` (elem (TControl (Exit t)))

  describe "pipeline" $ do
    it "should roundtrip exactly over a pristine tree" $ do
      let tagged = mark Pristine tree
      let printed = runReprinter (Proxy @'Language.JSON) src tagged
      printed `shouldBe` Right src

    it "should roundtrip exactly over a wholly-modified tree" $ do
      let tagged = mark Modified tree
      let printed = runReprinter (Proxy @'Language.JSON) src tagged
      printed `shouldBe` Right src
