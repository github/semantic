{-# LANGUAGE TemplateHaskell #-}
module Language.PHP.Grammar
( tree_sitter_php
, Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.PHP (tree_sitter_php)
import AST.Grammar.TH
import TreeSitter.Language (addDependentFileRelative)

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/tree-sitter-php/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_php
