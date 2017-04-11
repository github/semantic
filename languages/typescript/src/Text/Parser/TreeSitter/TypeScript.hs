module Text.Parser.TreeSitter.TypeScript where

import Foreign.Ptr
import Text.Parser.TreeSitter

foreign import ccall unsafe "vendor/tree-sitter-typescript/src/parser.c tree_sitter_typescript" tree_sitter_typescript :: Ptr Language
