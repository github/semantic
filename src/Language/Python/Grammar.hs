{-# LANGUAGE TemplateHaskell #-}
module Language.Python.Grammar where

import Language.Haskell.TH
import TreeSitter.Language
import TreeSitter.Python

-- | Statically-known rules corresponding to symbols in the grammar.
-- v4 - bump this to regenerate
mkSymbolDatatype (mkName "Grammar") tree_sitter_python
