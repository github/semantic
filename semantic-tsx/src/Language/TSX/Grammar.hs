{-# LANGUAGE TemplateHaskell #-}
module Language.TSX.Grammar
( tree_sitter_tsx
, Grammar(..)
) where

import AST.Grammar.TH
import Language.Haskell.TH
import TreeSitter.TSX (tree_sitter_tsx)

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData  (mkName "Grammar") tree_sitter_tsx
