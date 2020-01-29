{-# LANGUAGE TemplateHaskell #-}
module Language.Go.Grammar
( tree_sitter_go
, Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.Go.Internal
import TreeSitter.Language

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/tree-sitter-go/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_go
