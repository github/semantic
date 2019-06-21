{-# LANGUAGE TypeFamilies, TypeOperators #-}

module Rewriting.Python.Spec (spec) where

import           Control.Arrow
import           Control.Rewriting
import           Data.Sum
import qualified Data.Syntax.Declaration as Decl
import qualified Data.Syntax.Literal as Lit
import           SpecHelpers

-- This gets the Text contents of all integers
docstringMatcher :: ( Decl.Function :< fs
                    , [] :< fs
                    , Lit.TextElement :< fs
                    , term ~ Term (Sum fs) ann
                    ) => Rewrite term (TermF Decl.Function ann term)
docstringMatcher =
  narrowF <* (enter Decl.functionBody
              >>> narrow @[]
              >>> mhead
              >>> narrow @Lit.TextElement
              >>> ensure Lit.isTripleQuoted)

spec :: Spec
spec = describe "matching/python" $ do
  it "matches top-level docstrings" $ do
    parsed <- parseFileQuiet pythonParser "test/fixtures/python/matching/docstrings.py"
    let matched = recursively @[] docstringMatcher parsed
    length matched `shouldBe` 2

  it "matches docstrings recursively" $ do
    parsed <- parseFileQuiet pythonParser "test/fixtures/python/matching/docstrings_nested.py"
    let matched = recursively @[] docstringMatcher parsed
    length matched `shouldBe` 3
