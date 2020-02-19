{-# LANGUAGE TemplateHaskell #-}
module Language.Python.Grammar
( tree_sitter_python
, Grammar(..)
) where

import AST.Grammar.TH
import Language.Haskell.TH
import TreeSitter.Python (tree_sitter_python)

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_python
