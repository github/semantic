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


main :: IO ()
main = do
  args <- getArgs
  let (a, b) = files args in do
    a' <- parseModuleFile a
    b' <- parseModuleFile b
    return (a', b')
  return ()

parseModuleFile :: FilePath -> IO (ParseResult HsModule)
parseModuleFile file = do
  contents <- readFile file
  return $ parseModule contents

files (a : as) = (a, file as) where
  file (a : as) = a
