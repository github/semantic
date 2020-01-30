{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Grammar
( tree_sitter_java
, Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.Java (tree_sitter_java)
import AST.Grammar.TH

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../../vendor/tree-sitter-java/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_java
