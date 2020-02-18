{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Grammar
( tree_sitter_java
, Grammar(..)
) where

import AST.Grammar.TH
import Language.Haskell.TH
import TreeSitter.Java (tree_sitter_java)

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_java
