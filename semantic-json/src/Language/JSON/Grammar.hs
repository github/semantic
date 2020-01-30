{-# LANGUAGE TemplateHaskell #-}
module Language.JSON.Grammar
( tree_sitter_json
, Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.JSON (tree_sitter_json)
import AST.Grammar.TH

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/tree-sitter-json/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_json
