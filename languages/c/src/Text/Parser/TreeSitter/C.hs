module Text.Parser.TreeSitter.C where

import Text.Parser.TreeSitter
import Foreign.Ptr

foreign import ccall unsafe "vendor/tree-sitter-c/src/parser.c tree_sitter_c" tree_sitter_c :: Ptr Language
