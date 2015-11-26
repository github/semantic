module Main where

import Diff
import Patch
import Term
import Syntax
import Control.Comonad.Cofree
import Control.Monad.Free hiding (unfoldM)
import Data.Maybe
import Data.Set
import System.Environment

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr.Unsafe

data TSLanguage = TsLanguage deriving (Show, Eq)
foreign import ccall "prototype/doubt-difftool/doubt-difftool-Bridging-Header.h ts_language_c" ts_language_c :: IO (Ptr TSLanguage)

data TSDocument = TsDocument deriving (Show, Eq)
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_make" ts_document_make :: IO (Ptr TSDocument)
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_set_language" ts_document_set_language :: Ptr TSDocument -> Ptr TSLanguage -> IO ()
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_set_input_string" ts_document_set_input_string :: Ptr TSDocument -> CString -> IO ()
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_parse" ts_document_parse :: Ptr TSDocument -> IO ()
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_free" ts_document_free :: Ptr TSDocument -> IO ()

data TSLength = TsLength { bytes :: CSize, chars :: CSize }
  deriving (Show, Eq)

data TSNode = TsNode { _data :: Ptr (), offset :: TSLength }
  deriving (Show, Eq)

instance Storable TSNode where
  alignment n = 24
  sizeOf n = 24
  peek p = error "Haskell code should never read TSNode values directly."
  poke p n = error "Haskell code should never write TSNode values directly."

foreign import ccall "app/bridge.h ts_document_root_node_p" ts_document_root_node_p :: Ptr TSDocument -> Ptr TSNode -> IO ()
foreign import ccall "app/bridge.h ts_node_p_name" ts_node_p_name :: Ptr TSNode -> Ptr TSDocument -> IO CString
foreign import ccall "app/bridge.h ts_node_p_named_child_count" ts_node_p_named_child_count :: Ptr TSNode -> IO CSize
foreign import ccall "app/bridge.h ts_node_p_named_child" ts_node_p_named_child :: Ptr TSNode -> CSize -> Ptr TSNode -> IO CSize
foreign import ccall "app/bridge.h ts_node_p_pos_chars" ts_node_p_pos_chars :: Ptr TSNode -> IO CSize
foreign import ccall "app/bridge.h ts_node_p_size_chars" ts_node_p_size_chars :: Ptr TSNode -> IO CSize

main :: IO ()
main = do
  args <- getArgs
  let (a, b) = files args in do
    a' <- parseTreeSitterFile a
    b' <- parseTreeSitterFile b
    return (a', b')
  return ()

parseTreeSitterFile :: FilePath -> IO ()
parseTreeSitterFile file = do
  document <- ts_document_make
  language <- ts_language_c
  ts_document_set_language document language
  contents <- readFile file
  withCString contents (\source -> do
    ts_document_set_input_string document source
    ts_document_parse document
    term <- documentToTerm document contents
    ts_document_free document)
  putStrLn $ "hooray"

documentToTerm :: Ptr TSDocument -> String -> IO (Term String Info)
documentToTerm document contents = alloca $ \root -> do
  ts_document_root_node_p document root
  toTerm root where
    toTerm :: Ptr TSNode -> IO (Term String Info)
    toTerm node = do
      name <- ts_node_p_name node document
      name <- peekCString name
      children <- withNamedChildren node toTerm
      range <- range node
      annotation <- return . Info range $ singleton name
      return $ annotation :< case children of
        [] -> Leaf $ substring range contents
        _ | member name fixedProductions -> Fixed children
        _ | otherwise -> Indexed children

keyedProductions = fromList [ "object" ]
fixedProductions = fromList [ "pair", "rel_op", "math_op", "bool_op", "bitwise_op", "type_op", "math_assignment", "assignment", "subscript_access", "member_access", "new_expression", "function_call", "function", "ternary" ]

withNamedChildren :: Ptr TSNode -> (Ptr TSNode -> IO a) -> IO [a]
withNamedChildren node f = do
  count <- ts_node_p_named_child_count node
  if count == 0
    then return []
    else mapM (alloca . getChild f) [0..pred count] where
      getChild f n out = do
        ts_node_p_named_child node n out
        out <- f out
        return out

range :: Ptr TSNode -> IO Range
range node = do
  pos <- ts_node_p_pos_chars node
  size <- ts_node_p_size_chars node
  return Range { start = fromEnum $ toInteger pos, end = (fromEnum $ toInteger pos) + (fromEnum $ toInteger size) }

files (a : as) = (a, file as) where
  file (a : as) = a
files [] = error "expected two files to diff"

substring :: Range -> String -> String
substring range = take (end range) . drop (start range)
