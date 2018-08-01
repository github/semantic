{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.JSON.Grammar where

import Language.Haskell.TH
import TreeSitter.Language
import TreeSitter.JSON

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/haskell-tree-sitter/languages/json/vendor/tree-sitter-json/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
-- v2 - bump this to regenerate
mkSymbolDatatype (mkName "Grammar") tree_sitter_json
