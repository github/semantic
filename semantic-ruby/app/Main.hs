{-# LANGUAGE TypeApplications #-}
module Main
( main
) where

import           Control.Monad
import qualified Data.ByteString as B
import           Data.Foldable (traverse_)
import           System.Exit (die)
import           System.Environment (getArgs)
import           TreeSitter.Ruby
import qualified TreeSitter.Ruby.AST as Rb
import           TreeSitter.Unmarshal

main :: IO ()
main = getArgs >>= traverse_ (print <=< parseFile)

parseFile :: FilePath -> IO (Rb.Program ())
parseFile = either die pure <=< parseByteString @Rb.Program @() tree_sitter_ruby <=< B.readFile
