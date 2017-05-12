{-# LANGUAGE TemplateHaskell #-}
module Language.Python.Syntax where

import Language.Haskell.TH
import Text.Parser.TreeSitter.Python
import Text.Parser.TreeSitter.Language

-- | Statically-known rules corresponding to symbols in the grammar.
tmkSymbolDatatypet (mkName "Grammar") tree_sitter_python
