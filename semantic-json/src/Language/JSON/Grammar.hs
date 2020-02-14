{-# LANGUAGE TemplateHaskell #-}
module Language.JSON.Grammar
( tree_sitter_json
, Grammar(..)
) where

import AST.Grammar.TH
import Language.Haskell.TH
import TreeSitter.JSON (tree_sitter_json)
import TreeSitter.Language (addDependentFileRelative)

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_json
