module TreeSitter.JSON where

import Foreign.Ptr
import TreeSitter

foreign import ccall unsafe "vendor/tree-sitter-json/src/parser.c tree_sitter_json" tree_sitter_json :: Ptr Language
