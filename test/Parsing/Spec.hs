{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Werror #-}

module Parsing.Spec (spec) where

import Control.Effect
import Data.AST
import Data.Blob
import Data.ByteString.Char8 (pack)
import Data.Duration
import Data.Language
import Data.Maybe
import Data.Source
import Parsing.TreeSitter
import Semantic.Config
import SpecHelpers
import System.Timeout
import TreeSitter.JSON (tree_sitter_json, Grammar)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

toJSONSource :: Show a => a -> Source
toJSONSource = fromUTF8 . pack . show
source = toJSONSource $ take 10000 [1..]
largeBlob = sourceBlob "large.json" JSON source

case_parseToAST_returns_result_when_timeout_zer = do
  let timeout = fromMicroseconds 0 -- Zero microseconds indicates no timeout
  let parseTask = parseToAST timeout tree_sitter_json largeBlob :: TaskEff (Maybe (AST [] Grammar))
  result <- runTaskOrDie parseTask
  isJust result @? "Should always get a result when the timeout is zero"

case_parseToAST_returns_nothing_when_timeout_expires = do
  let timeout = fromMicroseconds 1000
  let parseTask = parseToAST timeout tree_sitter_json largeBlob :: TaskEff (Maybe (AST [] Grammar))
  result <- runTaskOrDie parseTask
  isNothing result @? "Should never get a result for a big file/small timeout"

spec :: TestTree
spec = $(testGroupGenerator)
