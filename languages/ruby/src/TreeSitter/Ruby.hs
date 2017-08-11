module TreeSitter.Ruby
( tree_sitter_ruby
) where

import Foreign.Ptr
import TreeSitter.Language

foreign import ccall unsafe "vendor/tree-sitter-ruby/src/parser.c tree_sitter_ruby" tree_sitter_ruby :: Ptr Language
