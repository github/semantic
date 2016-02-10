module TreeSitter where

import Category
import Range
import Parser
import Source
import qualified Data.Set as Set
import Foreign
import Foreign.C
import Foreign.C.Types
import qualified Data.Text as T
import Foreign.CStorable
import qualified GHC.Generics as Generics

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
  deriving (Show, Eq, Generics.Generic)

instance CStorable TSNode
instance Storable TSNode where
  alignment = cAlignment
  sizeOf = cSizeOf
  peek = cPeek
  poke = cPoke

foreign import ccall "app/bridge.h ts_document_root_node_p" ts_document_root_node_p :: Ptr TSDocument -> Ptr TSNode -> IO ()
foreign import ccall "app/bridge.h ts_node_p_name" ts_node_p_name :: Ptr TSNode -> Ptr TSDocument -> IO CString
foreign import ccall "app/bridge.h ts_node_p_named_child_count" ts_node_p_named_child_count :: Ptr TSNode -> IO CSize
foreign import ccall "app/bridge.h ts_node_p_named_child" ts_node_p_named_child :: Ptr TSNode -> CSize -> Ptr TSNode -> IO CSize
foreign import ccall "app/bridge.h ts_node_p_start_char" ts_node_p_start_char :: Ptr TSNode -> CSize
foreign import ccall "app/bridge.h ts_node_p_end_char" ts_node_p_end_char :: Ptr TSNode -> CSize

-- | A language in the eyes of semantic-diff.
data Language = Language { getTsLanguage :: Ptr TSLanguage, getConstructor :: Constructor }

-- | Returns a Language based on the file extension (including the ".").
languageForType :: T.Text -> Maybe Language
languageForType mediaType = case mediaType of
    ".h" -> Just . Language ts_language_c $ termConstructor categoryForNodeName
    ".c" -> Just . Language ts_language_c $ termConstructor categoryForNodeName
    ".js" -> Just . Language ts_language_javascript $ termConstructor categoryForNodeName
    _ -> Nothing

-- | Returns a parser for the given language.
treeSitterParser :: Language -> Parser
treeSitterParser (Language language constructor) contents = do
  document <- ts_document_make
  ts_document_set_language document language
  withCString (toList contents) (\source -> do
    ts_document_set_input_string document source
    ts_document_parse document
    term <- documentToTerm constructor document contents
    ts_document_free document
    return term)

-- | Given a node name from TreeSitter, return the correct category.
categoryForNodeName :: String -> Set.Set Category
categoryForNodeName name = case name of
  "function_call" -> Set.singleton FunctionCall
  _ -> Set.singleton (Other name)

-- | Given a constructor and a tree sitter document, return a parser.
documentToTerm :: Constructor -> Ptr TSDocument -> Parser
documentToTerm constructor document contents = alloca $ \ root -> do
  ts_document_root_node_p document root
  (_, term) <- toTerm root
  return term
  where toTerm node = do
          name <- ts_node_p_name node document
          name <- peekCString name
          count <- ts_node_p_named_child_count node
          children <- mapM (alloca . getChild node) $ take (fromIntegral count) [0..]
          -- Note: The strict application here is semantically important. Without it, we may not evaluate the range until after we’ve exited the scope that `node` was allocated within, meaning `alloca` will free it & other stack data may overwrite it.
          range <- return $! Range { start = fromIntegral $ ts_node_p_start_char node, end = fromIntegral $ ts_node_p_end_char node }

          return (name, constructor contents range name children)
        getChild node n out = do
          _ <- ts_node_p_named_child node n out
          toTerm out
