module Main where

import Diff
import Patch
import Term
import Syntax
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Map
import Data.Maybe
import Data.Set
import Language.Haskell.Parser
import Language.Haskell.Syntax
import System.Environment

import GHC.Generics
import Foreign
import Foreign.CStorable
import Foreign.C.Types
import Foreign.C.String

data TSLanguage = TsLanguage deriving (Show, Eq, Generic, CStorable)
foreign import ccall "prototype/doubt-difftool/doubt-difftool-Bridging-Header.h ts_language_c" ts_language_c :: IO (Foreign.Ptr TSLanguage)

data TSDocument = TsDocument deriving (Show, Eq, Generic, CStorable)
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_make" ts_document_make :: IO (Foreign.Ptr TSDocument)
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_set_language" ts_document_set_language :: Foreign.Ptr TSDocument -> Foreign.Ptr TSLanguage -> IO ()
foreign import ccall "prototype/External/tree-sitter/include/tree_sitter/runtime.h ts_document_set_input_string" ts_document_set_input_string :: Foreign.Ptr TSDocument -> CString -> IO ()

main :: IO ()
main = do
--   args <- getArgs
--   return f
  -- let (a, b) = files args in do
  --   -- a' <- parseModuleFile a
  --   -- b' <- parseModuleFile b
  --   return f
  -- return ()
  document <- ts_document_make
  language <- ts_language_c
  ts_document_set_language document language
  putStrLn $ "cSizeOf " ++ show (cSizeOf document)

parseModuleFile :: FilePath -> IO (ParseResult HsModule)
parseModuleFile file = do
  contents <- readFile file
  return $ parseModule contents

files (a : as) = (a, file as) where
  file (a : as) = a
files [] = error "expected two files to diff"
