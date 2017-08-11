module TreeSitter.Go where

import Foreign.Ptr
import TreeSitter.Language

foreign import ccall unsafe "vendor/tree-sitter-go/src/parser.c tree_sitter_go" tree_sitter_go :: Ptr Language
