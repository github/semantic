module Main where

import Diff
import Patch
import Term
import Syntax
import Control.Comonad.Cofree
import Control.Monad ((>=>))
import Control.Monad.Free hiding (unfoldM)
import Data.Map
import Data.Maybe
import Data.Set
import Language.Haskell.Parser
import Language.Haskell.Syntax
import System.Environment

import GHC.Generics
import GHC.Prim
import Foreign
import Foreign.C
import Foreign.CStorable
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr.Unsafe

data TSLanguage = TsLanguage deriving (Show, Eq, Generic, CStorable)
foreign import ccall "prototype/doubt-difftool/doubt-difftool-Bridging-Header.h ts_language_c" ts_language_c :: IO (Ptr TSLanguage)

data TSDocument = TsDocument deriving (Show, Eq, Generic, CStorable)
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_make" ts_document_make :: IO (Ptr TSDocument)
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_set_language" ts_document_set_language :: Ptr TSDocument -> Ptr TSLanguage -> IO ()
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_set_input_string" ts_document_set_input_string :: Ptr TSDocument -> CString -> IO ()
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_parse" ts_document_parse :: Ptr TSDocument -> IO ()
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_free" ts_document_free :: Ptr TSDocument -> IO ()

data TSLength = TsLength !CSize !CSize
  deriving (Show, Eq, Generic, CStorable)

instance Storable TSLength where
  alignment l = cAlignment l
  sizeOf l = cSizeOf l
  peek p = cPeek p
  poke p l = cPoke p l

data TSNode = TsNode !(Ptr ()) !TSLength
  deriving (Show, Eq, Generic, CStorable)

instance Storable TSNode where
  alignment n = cAlignment n
  sizeOf n = cSizeOf n
  peek p = cPeek p
  poke p n = cPoke p n

foreign import ccall "app/bridge.h ts_document_root_node_p" ts_document_root_node_p :: Ptr TSDocument -> Ptr TSNode -> IO ()
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_node_p_name" ts_node_p_name :: Ptr TSNode -> Ptr TSDocument -> IO CString
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_node_p_named_child_count" ts_node_p_named_child_count :: Ptr TSNode -> IO CSize
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_node_p_named_child" ts_node_p_named_child :: Ptr TSNode -> CSize -> Ptr TSNode -> IO CSize

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
  source <- newCString contents
  ts_document_set_input_string document source
  ts_document_parse document
  withNode (\root -> do
    ts_document_root_node_p document root
    unfoldM (toTerm document) (root, "program"))
  ts_document_free document
  free source
  putStrLn $ "cSizeOf " ++ show (cSizeOf document)

toTerm :: Ptr TSDocument -> (Ptr TSNode, String) -> IO (Info, Syntax String (Ptr TSNode, String))
toTerm document (node, category) = do
  name <- ts_node_p_name node document
  count <- ts_node_p_named_child_count node
  return (Info (Range { start = 0, end = 0 }) $ Data.Set.fromList [ category ], Leaf "") where
    keyedProductions = Data.Set.fromList [ "object" ]
    fixedProductions = Data.Set.fromList [ "pair", "rel_op", "math_op", "bool_op", "bitwise_op", "type_op", "math_assignment", "assignment", "subscript_access", "member_access", "new_expression", "function_call", "function", "ternary" ]

withNode :: (Ptr TSNode -> IO a) -> IO a
withNode writer = do
  node <- (mallocForeignPtr :: IO (ForeignPtr TSNode))
  withForeignPtr node writer

parseModuleFile :: FilePath -> IO (ParseResult HsModule)
parseModuleFile file = do
  contents <- readFile file
  return $ parseModule contents

files (a : as) = (a, file as) where
  file (a : as) = a
files [] = error "expected two files to diff"
