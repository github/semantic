{-# LANGUAGE TemplateHaskell #-}
module Language.Ruby.Grammar
( tree_sitter_ruby
, Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.Ruby (tree_sitter_ruby)
import AST.Grammar.TH

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/tree-sitter-ruby/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_ruby
