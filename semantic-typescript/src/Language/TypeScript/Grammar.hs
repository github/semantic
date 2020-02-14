{-# LANGUAGE TemplateHaskell #-}
module Language.TypeScript.Grammar
( tree_sitter_typescript
, Grammar(..)
) where

import AST.Grammar.TH
import Language.Haskell.TH
import TreeSitter.TypeScript (tree_sitter_typescript)

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_typescript
