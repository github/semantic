module Text.Parser.TreeSitter.JavaScript where

import Text.Parser.TreeSitter
import Foreign.Ptr

foreign import ccall "vendor/tree-sitter-javascript/src/parser.c ts_language_javascript" ts_language_javascript :: Ptr Language
