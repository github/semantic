{-# LANGUAGE TemplateHaskell #-}
module Language.Ruby.Grammar
( tree_sitter_ruby
, Grammar(..)
) where

import AST.Grammar.TH
import Language.Haskell.TH
import TreeSitter.Ruby (tree_sitter_ruby)

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_ruby
