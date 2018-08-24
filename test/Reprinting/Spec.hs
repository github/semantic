{-# LANGUAGE OverloadedLists, TypeOperators #-}

module Reprinting.Spec where

import SpecHelpers hiding (project, inject)

import Data.Functor.Foldable (embed, cata)
import qualified Data.Language as Language
import qualified Data.Syntax.Literal as Literal
import Data.Algebra
import Reprinting.Tokenize
import Reprinting.Pipeline
import Data.Sum
import Data.Foldable
import Semantic.IO
import Semantic.Util.Rewriting hiding (parseFile)
import Data.Blob
import Language.JSON.PrettyPrint
import Language.Ruby.PrettyPrint

spec :: Spec
spec = describe "reprinting" $ do
  context "JSON" $ do
    let path = "test/fixtures/javascript/reprinting/map.json"
    (src, tree) <- runIO $ do
      src  <- blobSource <$> readBlobFromPath (File path Language.JSON)
      tree <- parseFile jsonParser path
      pure (src, tree)

    describe "tokenization" $ do

      it "should pass over a pristine tree" $ do
        let tagged = mark Unmodified tree
        let toks = tokenizing src tagged
        toks `shouldBe` [Chunk src]

      it "should emit control tokens but only 1 chunk for a wholly-modified tree" $ do
        let toks = tokenizing src (mark Refactored tree)
        for_ @[] [TList, THash] $ \t -> do
          toks `shouldSatisfy` elem (TControl (Enter t))
          toks `shouldSatisfy` elem (TControl (Exit t))

    describe "pipeline" $ do

      it "should roundtrip exactly over a pristine tree" $ do
        let tagged = mark Unmodified tree
        let printed = runReprinter src defaultJSONPipeline tagged
        printed `shouldBe` Right src

      it "should roundtrip exactly over a wholly-modified tree" $ do
        let tagged = mark Refactored tree
        let printed = runReprinter src defaultJSONPipeline tagged
        printed `shouldBe` Right src

      it "should be able to parse the output of a refactor" $ do
        let tagged = increaseNumbers (mark Refactored tree)
        let (Right printed) = runReprinter src defaultJSONPipeline tagged
        tree' <- runTask (parse jsonParser (Blob printed path Language.JSON))
        length tree' `shouldSatisfy` (/= 0)

  context "Ruby" $ do
    let dir = "test/fixtures/ruby/reprinting"
    let path = dir </> "function.rb"
    let expectedPath = dir </> "function.out.rb"
    (src, tree, expected) <- runIO $ do
      expected  <- blobSource <$> readBlobFromPath (File expectedPath Language.Ruby)
      src  <- blobSource <$> readBlobFromPath (File path Language.Ruby)
      tree <- parseFile miniRubyParser path
      pure (src, tree, expected)

    describe "pipeline" $ do

      it "should roundtrip over a wholly-modified tree" $ do
        let tagged = mark Refactored tree
        let (Right printed) = runReprinter src printingRuby tagged
        printed `shouldBe` expected
        tree' <- runTask (parse miniRubyParser (Blob printed expectedPath Language.Ruby))
        length tree' `shouldSatisfy` (/= 0)
