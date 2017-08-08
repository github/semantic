{-# LANGUAGE TemplateHaskell #-}
module Language.TypeScript.Grammar where

import Language.Haskell.TH
import Text.Parser.TreeSitter.Language
import Text.Parser.TreeSitter.TypeScript

-- | Statically-known rules corresponding to symbols in the grammar.
-- v2 - bump this to regenerate
mkSymbolDatatype (mkName "Grammar") tree_sitter_typescript
