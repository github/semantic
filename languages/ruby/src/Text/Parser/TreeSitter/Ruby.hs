module Text.Parser.TreeSitter.Ruby where

import Text.Parser.TreeSitter
import Foreign.Ptr

foreign import ccall "vendor/tree-sitter-ruby/src/parser.c ts_language_ruby" ts_language_ruby :: Ptr Language
