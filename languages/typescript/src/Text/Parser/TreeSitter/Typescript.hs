module Text.Parser.TreeSitter.Typescript where

import Text.Parser.TreeSitter
import Foreign.Ptr

foreign import ccall "vendor/tree-sitter-typescript/src/parser.c tree_sitter_typescript" tree_sitter_typescript :: Ptr Language
