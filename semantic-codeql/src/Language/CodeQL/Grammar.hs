{-# LANGUAGE TemplateHaskell #-}
module Language.CodeQL.Grammar
( tree_sitter_ql
, Grammar(..)
) where

import AST.Grammar.TH
import Language.Haskell.TH
import TreeSitter.QL (tree_sitter_ql)

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_ql
