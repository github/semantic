{-# LANGUAGE TemplateHaskell #-}
module Language.TypeScript.Grammar where

import Language.Haskell.TH
import TreeSitter.Language
import TreeSitter.TypeScript

-- | Statically-known rules corresponding to symbols in the grammar.
-- v2 - bump this to regenerate
mkSymbolDatatype (mkName "Grammar") tree_sitter_typescript
