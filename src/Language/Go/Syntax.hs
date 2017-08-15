{-# LANGUAGE TemplateHaskell #-}
module Language.Go.Syntax where

import Language.Haskell.TH
import TreeSitter.Go
import TreeSitter.Language

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_go
