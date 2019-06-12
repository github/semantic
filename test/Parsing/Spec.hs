module Parsing.Spec (spec) where

import Data.AST
import Data.Blob
import Data.ByteString.Char8 (pack)
import Data.Duration
import Data.Language
import Data.Maybe
import Data.Source
import Parsing.TreeSitter
import SpecHelpers
import TreeSitter.JSON (tree_sitter_json, Grammar)

spec :: Spec
spec = parallel $ do
  describe "parseToAST" $ do
    let source = toJSONSource $ take 10000 [1..]
    let largeBlob = sourceBlob "large.json" JSON source

    it "returns a result when the timeout does not expire" $ do
      let timeout = fromMicroseconds 0 -- Zero microseconds indicates no timeout
      let parseTask = parseToAST timeout tree_sitter_json largeBlob :: TaskEff (Maybe (AST [] Grammar))
      result <- runTaskOrDie parseTask
      (isJust result) `shouldBe` True

    it "returns nothing when the timeout expires" $ do
      let timeout = fromMicroseconds 1000
      let parseTask = parseToAST timeout tree_sitter_json largeBlob :: TaskEff (Maybe (AST [] Grammar))
      result <- runTaskOrDie parseTask
      (isNothing result) `shouldBe` True

toJSONSource :: Show a => a -> Source
toJSONSource = fromUTF8 . pack . show
