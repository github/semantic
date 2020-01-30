{-# LANGUAGE TemplateHaskell #-}
module Language.Python.Grammar
( tree_sitter_python
, Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.Python (tree_sitter_python)
import AST.Grammar.TH
import TreeSitter.Language (addDependentFileRelative)

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/tree-sitter-python/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_python
