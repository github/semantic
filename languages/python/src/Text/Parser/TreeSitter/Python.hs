module Text.Parser.TreeSitter.Python
( tree_sitter_python
) where

import Foreign.Ptr
import Text.Parser.TreeSitter

foreign import ccall unsafe "vendor/tree-sitter-python/src/parser.c tree_sitter_python" tree_sitter_python :: Ptr Language
