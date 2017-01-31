module Text.Parser.TreeSitter.Go where

import Text.Parser.TreeSitter
import Foreign.Ptr

foreign import ccall "vendor/tree-sitter-go/src/parser.c ts_language_go" ts_language_go :: Ptr Language
