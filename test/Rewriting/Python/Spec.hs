{-# LANGUAGE TypeFamilies, TypeOperators, TemplateHaskell #-}

module Rewriting.Python.Spec (spec) where

import           Control.Arrow
import           Control.Rewriting
import           Data.Abstract.Module
import           Data.List
import           Data.Sum
import qualified Data.Syntax.Declaration as Decl
import qualified Data.Syntax.Literal as Lit
import qualified Data.Syntax.Statement as Stmt
import           Data.Text (Text)
import           SpecHelpers

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

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

case_matches_top_level_docstrings,
  case_matches_docstrings_recursively
  :: Assertion

case_matches_top_level_docstrings = do
  parsed <- parseFile pythonParser "test/fixtures/python/matching/docstrings.py"
  let matched = recursively @[] docstringMatcher parsed
  length matched @?= 2

case_matches_docstrings_recursively = do
  parsed <- parseFile pythonParser "test/fixtures/python/matching/docstrings_nested.py"
  let matched = recursively @[] docstringMatcher parsed
  length matched @?= 3

spec :: TestTree
spec = $(testGroupGenerator)
