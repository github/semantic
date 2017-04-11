module Text.Parser.TreeSitter.Ruby where

import Foreign.Ptr
import Text.Parser.TreeSitter

foreign import ccall unsafe "vendor/tree-sitter-ruby/src/parser.c tree_sitter_ruby" tree_sitter_ruby :: Ptr Language
