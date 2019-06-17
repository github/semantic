{-# LANGUAGE GADTs, OverloadedLists, TypeOperators #-}

module Reprinting.Spec (spec) where

import SpecHelpers

import           Data.Foldable
import qualified Data.Machine as Machine

import           Control.Rewriting
import qualified Data.Language as Language
import           Data.Reprinting.Scope
import           Data.Reprinting.Token
import           Data.Sum
import qualified Data.Syntax.Literal as Literal
import           Language.JSON.PrettyPrint
import           Reprinting.Pipeline
import           Reprinting.Tokenize

increaseNumbers :: (Literal.Float :< fs, Apply Functor fs) => Rule (Term (Sum fs) History)
increaseNumbers = do
  (Literal.Float c) <- target >>= guardTerm
  create (Literal.Float (c <> "0"))

spec :: Spec
spec = describe "reprinting" $ do
  context "JSON" $ do
    let path = "test/fixtures/javascript/reprinting/map.json"
    (src, tree) <- runIO $ do
      src  <- blobSource <$> readBlobFromFile' (File path Language.JSON)
      tree <- parseFile jsonParser path
      pure (src, tree)

    describe "tokenization" $ do

      it "should pass over a pristine tree" $ do
        let tagged = mark Unmodified tree
        let toks = Machine.run $ tokenizing src tagged
        toks `shouldSatisfy` not . null
        head toks `shouldSatisfy` isControl
        last toks `shouldSatisfy` isChunk

      it "should emit control tokens but only 1 chunk for a wholly-modified tree" $ do
        let toks = Machine.run $ tokenizing src (mark Refactored tree)
        for_ @[] [List, Hash] $ \t -> do
          toks `shouldSatisfy` elem (Control (Enter t))
          toks `shouldSatisfy` elem (Control (Exit t))

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
        let maybeTagged = rewrite (mark Unmodified tree) (topDownAny increaseNumbers)
        tagged <- maybe (fail "rewrite failed") pure maybeTagged

        let eitherPrinted = runReprinter src defaultJSONPipeline tagged
        printed <- either (fail "reprinter failed") pure eitherPrinted

        tree' <- runTaskOrDie (parse jsonParser (makeBlob printed path Language.JSON mempty))
        length tree' `shouldSatisfy` (/= 0)
