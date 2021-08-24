{-# LANGUAGE TemplateHaskell #-}
module Language.OCaml.Grammar
( tree_sitter_ocaml
, Grammar(..)
) where

import AST.Grammar.TH
import Language.Haskell.TH
import TreeSitter.OCaml (tree_sitter_ocaml)

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_ocaml
