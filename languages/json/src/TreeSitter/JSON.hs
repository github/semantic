module TreeSitter.JSON
( tree_sitter_json
) where

import Foreign.Ptr
import TreeSitter.Language

foreign import ccall unsafe "vendor/tree-sitter-json/src/parser.c tree_sitter_json" tree_sitter_json :: Ptr Language
