{-# LANGUAGE GADTs, OverloadedLists, TypeOperators #-}

module Reprinting.Spec where

import SpecHelpers hiding (inject, project)

import           Data.Foldable
import           Data.Functor.Foldable (cata, embed)
import qualified Data.Machine as Machine

import           Control.Rewriting hiding (context)
import           Data.Algebra
import           Data.Blob
import qualified Data.Language as Language
import           Data.Reprinting.Scope
import           Data.Reprinting.Token
import           Data.Sum
import qualified Data.Syntax.Literal as Literal
import           Language.JSON.PrettyPrint
import           Language.Python.PrettyPrint
import           Language.Ruby.PrettyPrint
import           Reprinting.Pipeline
import           Reprinting.Tokenize
import           Semantic.IO

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
        let (Just tagged) = rewrite (mark Unmodified tree) (topDownAny increaseNumbers)
        let (Right printed) = runReprinter src defaultJSONPipeline tagged
        tree' <- runTaskOrDie (parse jsonParser (Blob printed path Language.JSON))
        length tree' `shouldSatisfy` (/= 0)
