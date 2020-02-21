{-# LANGUAGE TemplateHaskell #-}
module Language.PHP.Grammar
( tree_sitter_php
, Grammar(..)
) where

import AST.Grammar.TH
import Language.Haskell.TH
import TreeSitter.PHP (tree_sitter_php)

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_php
