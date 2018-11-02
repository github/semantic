{-# LANGUAGE TypeOperators, TypeFamilies #-}

module Matching.Python.Spec (spec) where

import Control.Arrow
import           Control.Matching
import           Data.Abstract.Module
import           Data.List
import           Data.Sum
import qualified Data.Syntax.Declaration as Decl
import qualified Data.Syntax.Literal as Lit
import qualified Data.Syntax.Statement as Stmt
import           Data.Text (Text)
import           SpecHelpers

-- This gets the Text contents of all integers
docstringMatcher :: ( Decl.Function :< fs
                    , [] :< fs
                    , Lit.TextElement :< fs
                    , term ~ Term (Sum fs) ann
                    ) => Matcher term (TermF Decl.Function ann term)
docstringMatcher =
  narrowF <* (Decl.functionBody
              >>: narrow @[]
              >>> mhead
              >>> narrow @Lit.TextElement
              >>> ensure Lit.isTripleQuoted)

spec :: Spec
spec = describe "matching/python" $ do
  it "matches top-level docstrings" $ do
    parsed <- parseFile pythonParser "test/fixtures/python/matching/docstrings.py"
    let matched = matchRecursively @[] docstringMatcher parsed
    length matched `shouldBe` 2

  it "matches docstrings recursively" $ do
    parsed <- parseFile pythonParser "test/fixtures/python/matching/docstrings_nested.py"
    let matched = matchRecursively @[] docstringMatcher parsed
    length matched `shouldBe` 3
