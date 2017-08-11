module TreeSitter.TypeScript where

import Foreign.Ptr
import TreeSitter.Language

foreign import ccall unsafe "vendor/tree-sitter-typescript/src/parser.c tree_sitter_typescript" tree_sitter_typescript :: Ptr Language
