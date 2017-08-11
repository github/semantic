module TreeSitter.Python
( tree_sitter_python
) where

import Foreign.Ptr
import TreeSitter.Language

foreign import ccall unsafe "vendor/tree-sitter-python/src/parser.c tree_sitter_python" tree_sitter_python :: Ptr Language
