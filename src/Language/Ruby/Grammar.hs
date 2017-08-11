{-# LANGUAGE TemplateHaskell #-}
module Language.Ruby.Grammar where

import Language.Haskell.TH
import TreeSitter.Language
import TreeSitter.Ruby

-- v2 - Bump to get file to change to force template haskell to regenerate.
-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_ruby
