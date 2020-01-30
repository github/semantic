{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Grammar
( tree_sitter_java
, Grammar(..)
) where

import Language.Haskell.TH
-- import TreeSitter.Java.Internal
import TreeSitter.Java (tree_sitter_java)
import TreeSitter.Language

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/tree-sitter-java/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_java
