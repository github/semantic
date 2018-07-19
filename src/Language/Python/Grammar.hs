{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.Python.Grammar where

import Language.Haskell.TH
import TreeSitter.Language
import TreeSitter.Python

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/haskell-tree-sitter/languages/python/vendor/tree-sitter-python/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_python
