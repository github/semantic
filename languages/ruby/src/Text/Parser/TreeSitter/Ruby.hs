module Text.Parser.TreeSitter.Ruby where

import Text.Parser.TreeSitter
import Foreign.Ptr

foreign import ccall unsafe "vendor/tree-sitter-ruby/src/parser.c tree_sitter_ruby" tree_sitter_ruby :: Ptr Language
