module Text.Parser.TreeSitter.Document where

import Foreign
import Foreign.C
import Text.Parser.TreeSitter.Language

newtype Document = Document ()
  deriving (Show, Eq)

foreign import ccall "vendor/tree-sitter/include/tree_sitter/runtime.h ts_document_new" ts_document_new :: IO (Ptr Document)
foreign import ccall "vendor/tree-sitter/include/tree_sitter/runtime.h ts_document_set_input_string" ts_document_set_input_string :: Ptr Document -> CString -> IO ()
foreign import ccall "vendor/tree-sitter/include/tree_sitter/runtime.h ts_document_parse" ts_document_parse :: Ptr Document -> IO ()
foreign import ccall "vendor/tree-sitter/include/tree_sitter/runtime.h ts_document_free" ts_document_free :: Ptr Document -> IO ()
foreign import ccall "vendor/tree-sitter/include/tree_sitter/runtime.h ts_document_set_language" ts_document_set_language :: Ptr Document -> Ptr Language -> IO ()
