{-# LANGUAGE TemplateHaskell #-}
module Language.Go.Grammar
( tree_sitter_go
, Grammar(..)
) where

import AST.Grammar.TH
import Language.Haskell.TH
import TreeSitter.Go (tree_sitter_go)

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_go
