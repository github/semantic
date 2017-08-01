{-# LANGUAGE TemplateHaskell #-}
module Language.Ruby.Grammar where

import Language.Haskell.TH
import Text.Parser.TreeSitter.Language
import Text.Parser.TreeSitter.Ruby

-- v3 - Bump to get file to change to force template haskell to regenerate.
-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_ruby
