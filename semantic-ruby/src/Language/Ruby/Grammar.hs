{-# LANGUAGE TemplateHaskell #-}
module Language.Ruby.Grammar
( tree_sitter_ruby
, Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.Ruby.Internal
import TreeSitter.Language

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/tree-sitter-ruby/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_ruby
