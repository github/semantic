{-# LANGUAGE TemplateHaskell #-}
module Language.C.Grammar
( tree_sitter_c
, Grammar(..)
) where

import AST.Grammar.TH
import Language.Haskell.TH
import TreeSitter.C (tree_sitter_c)

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_c
