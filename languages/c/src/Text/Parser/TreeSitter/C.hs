module Text.Parser.TreeSitter.C where

import Text.Parser.TreeSitter
import Foreign.Ptr

foreign import ccall "vendor/tree-sitter-c/src/parser.c ts_language_c" ts_language_c :: Ptr Language
