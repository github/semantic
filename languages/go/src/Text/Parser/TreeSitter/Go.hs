module Text.Parser.TreeSitter.Go where

import Text.Parser.TreeSitter
import Foreign.Ptr

foreign import ccall unsafe "vendor/tree-sitter-go/src/parser.c tree_sitter_go" tree_sitter_go :: Ptr Language
