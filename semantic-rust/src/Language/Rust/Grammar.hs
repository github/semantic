{-# LANGUAGE TemplateHaskell #-}
module Language.Rust.Grammar
( tree_sitter_rust
, Grammar(..)
) where

import AST.Grammar.TH
import Language.Haskell.TH
import TreeSitter.Rust (tree_sitter_rust)

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_rust
