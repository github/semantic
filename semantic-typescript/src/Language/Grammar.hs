{-# LANGUAGE TemplateHaskell #-}
module Language.TypeScript.Grammar
( tree_sitter_typescript
, Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.TypeScript.Internal
import TreeSitter.Language

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/tree-sitter-typescript/typescript/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_typescript
