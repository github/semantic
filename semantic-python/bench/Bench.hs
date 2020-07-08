{-# LANGUAGE TypeApplications #-}
module Main
( main
) where

import           Control.Monad
import qualified Data.ByteString as B
import           Gauge
import           System.Exit (die)
import           System.Environment (getArgs)
import qualified Language.Python.AST as Py
import           Language.Python.Grammar
import           AST.Unmarshal

main :: IO ()
main = do
  getArgs >>= defaultMain . map (bench <*> nfIO . (() <$) . parseFile)

parseFile :: FilePath -> IO (Py.Module ())
parseFile = either die pure <=< parseByteString @Py.Module @() tree_sitter_python <=< B.readFile
