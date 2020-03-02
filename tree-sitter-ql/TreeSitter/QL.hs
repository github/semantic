module TreeSitter.QL
( tree_sitter_ql
, getNodeTypesPath
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_ql

foreign import ccall unsafe "vendor/tree-sitter-ql/src/parser.c tree_sitter_ql" tree_sitter_ql :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-ql/src/node-types.json"
