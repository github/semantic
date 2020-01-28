{-# LANGUAGE TemplateHaskell #-}
module Language.Python.Grammar
( tree_sitter_python
, Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.Python.Internal
import TreeSitter.Language

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/tree-sitter-python/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_python
