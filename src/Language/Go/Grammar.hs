{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.Go.Grammar where

import Language.Haskell.TH
import TreeSitter.Go
import TreeSitter.Language

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/haskell-tree-sitter/languages/go/vendor/tree-sitter-go/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
-- v1 - bump this to regenerate
mkSymbolDatatype (mkName "Grammar") tree_sitter_go
