{-# LANGUAGE TemplateHaskell #-}
module Language.Go.Grammar where

import Language.Haskell.TH
import TreeSitter.Language
import TreeSitter.Go

-- | Statically-known rules corresponding to symbols in the grammar.
-- v1 - bump this to regenerate
mkSymbolDatatype (mkName "Grammar") tree_sitter_go
