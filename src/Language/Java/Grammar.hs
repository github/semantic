{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.Java.Grammar where

import Language.Haskell.TH
import TreeSitter.Language
import TreeSitter.Java

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/haskell-tree-sitter/languages/java/vendor/tree-sitter-java/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_java
