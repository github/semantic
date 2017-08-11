module TreeSitter.TypeScript
( tree_sitter_typescript
) where

import Foreign.Ptr
import TreeSitter.Language

foreign import ccall unsafe "vendor/tree-sitter-typescript/src/parser.c tree_sitter_typescript" tree_sitter_typescript :: Ptr Language
