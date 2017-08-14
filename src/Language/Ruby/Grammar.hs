{-# LANGUAGE TemplateHaskell #-}
module Language.Ruby.Grammar where

import Language.Haskell.TH
import Text.Parser.TreeSitter.Language
import Text.Parser.TreeSitter.Ruby

-- Bump to get file to change to force template haskell to regenerate.
-- tree-sitter-ruby@6681b4c8d255ca0aec09c29777271736186b6c7d
-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_ruby
