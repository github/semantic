{-# LANGUAGE TemplateHaskell #-}
module Language.JSON.Grammar where

import Language.Haskell.TH
import TreeSitter.Language
import TreeSitter.JSON

-- | Statically-known rules corresponding to symbols in the grammar.
-- v2 - bump this to regenerate
mkSymbolDatatype (mkName "Grammar") tree_sitter_json
