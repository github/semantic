{-# LANGUAGE TemplateHaskell #-}
module Language.Ruby.Grammar where

import Language.Haskell.TH
import Text.Parser.TreeSitter.Language
import Text.Parser.TreeSitter.Ruby

-- Re-generate template haskell code when these files change:
addDependentFileRelative "../../../languages/ruby/vendor/tree-sitter-ruby/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_ruby
