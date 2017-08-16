{-# LANGUAGE TemplateHaskell #-}
module Language.Ruby.Grammar where

import Language.Haskell.TH
import TreeSitter.Language
import TreeSitter.Ruby

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/haskell-tree-sitter/languages/ruby/vendor/tree-sitter-ruby/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_ruby
