{-# LANGUAGE TemplateHaskell #-}
module Language.TypeScript.Syntax where

import Language.Haskell.TH
import Text.Parser.TreeSitter.Language
import Text.Parser.TreeSitter.TypeScript

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_typescript
