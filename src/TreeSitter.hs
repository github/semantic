module TreeSitter where

import Diff
import Range
import Parser
import qualified Data.Set as Set
import Foreign
import Foreign.C
import Foreign.C.Types

data TSLanguage = TsLanguage deriving (Show, Eq)
foreign import ccall "prototype/doubt-difftool/doubt-difftool-Bridging-Header.h ts_language_c" ts_language_c :: Ptr TSLanguage
foreign import ccall "prototype/doubt-difftool/doubt-difftool-Bridging-Header.h ts_language_javascript" ts_language_javascript :: Ptr TSLanguage

data TSDocument = TsDocument deriving (Show, Eq)
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_make" ts_document_make :: IO (Ptr TSDocument)
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_set_language" ts_document_set_language :: Ptr TSDocument -> Ptr TSLanguage -> IO ()
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_set_input_string" ts_document_set_input_string :: Ptr TSDocument -> CString -> IO ()
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_parse" ts_document_parse :: Ptr TSDocument -> IO ()
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_free" ts_document_free :: Ptr TSDocument -> IO ()

data TSNode = TsNode { _data :: Ptr (), offset0 :: CSize, offset1 :: CSize, offset2 :: CSize }
  deriving (Show, Eq)

instance Storable TSNode where
  alignment _ = 32
  sizeOf _ = 32
  peek _ = error "Haskell code should never read TSNode values directly."
  poke _ _ = error "Haskell code should never write TSNode values directly."

foreign import ccall "app/bridge.h ts_document_root_node_p" ts_document_root_node_p :: Ptr TSDocument -> Ptr TSNode -> IO ()
foreign import ccall "app/bridge.h ts_node_p_name" ts_node_p_name :: Ptr TSNode -> Ptr TSDocument -> IO CString
foreign import ccall "app/bridge.h ts_node_p_named_child_count" ts_node_p_named_child_count :: Ptr TSNode -> IO CSize
foreign import ccall "app/bridge.h ts_node_p_named_child" ts_node_p_named_child :: Ptr TSNode -> CSize -> Ptr TSNode -> IO CSize
foreign import ccall "app/bridge.h ts_node_p_start_char" ts_node_p_start_char :: Ptr TSNode -> CSize
foreign import ccall "app/bridge.h ts_node_p_end_char" ts_node_p_end_char :: Ptr TSNode -> CSize

data Language = Language { getTsLanguage :: Ptr TSLanguage, getConstructor :: Constructor }

languageForType :: String -> Maybe Language
languageForType mediaType = case mediaType of
    ".h" -> c
    ".c" -> c
    ".js" -> Just . Language ts_language_javascript $ constructorForProductions
      (Set.fromList [ "object" ])
      (Set.fromList [ "pair", "rel_op", "math_op", "bool_op", "bitwise_op", "type_op", "math_assignment", "assignment", "subscript_access", "member_access", "new_expression", "function_call", "function", "ternary" ])
    _ -> Nothing
  where c = Just . Language ts_language_c $ constructorForProductions mempty (Set.fromList [ "assignment_expression", "logical_expression", "pointer_expression", "field_expression", "relational_expression", "designator", "call_expression", "math_expression" ])

parseTreeSitterFile :: Language -> Parser
parseTreeSitterFile (Language language constructor) contents = do
  document <- ts_document_make
  ts_document_set_language document language
  withCString contents (\source -> do
    ts_document_set_input_string document source
    ts_document_parse document
    term <- documentToTerm constructor document contents
    ts_document_free document
    return term)

documentToTerm :: Constructor -> Ptr TSDocument -> Parser
documentToTerm constructor document contents = alloca $ \root -> do
  ts_document_root_node_p document root
  (_, term) <- toTerm root
  return term
  where toTerm node = do
          name <- ts_node_p_name node document
          name <- peekCString name
          count <- ts_node_p_named_child_count node
          children <- if count == 0
            then return []
            else mapM (alloca . getChild node toTerm) [0..pred count]
          return (name, constructor contents (Info (range node) (Set.singleton name)) children)
        getChild node transform n out = do
          _ <- ts_node_p_named_child node n out
          transform out
        range node = Range { start = fromIntegral $! ts_node_p_start_char node, end = fromIntegral $! ts_node_p_end_char node }
