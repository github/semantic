module Text.Parser.TreeSitter.JavaScript where

import Text.Parser.TreeSitter
import Foreign.Ptr

foreign import ccall unsafe "vendor/tree-sitter-javascript/src/parser.c tree_sitter_javascript" tree_sitter_javascript :: Ptr Language
