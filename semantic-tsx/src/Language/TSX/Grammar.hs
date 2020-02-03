{-# LANGUAGE TemplateHaskell #-}
module Language.TSX.Grammar
( tree_sitter_tsx
, Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.TSX (tree_sitter_tsx)
import AST.Grammar.TH
import TreeSitter.Language (addDependentFileRelative)

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/tree-sitter-typescript/tsx/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData  (mkName "Grammar") tree_sitter_tsx
