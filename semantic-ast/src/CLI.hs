{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
module CLI (main) where

import System.Environment
import TreeSitter.Unmarshal
import TreeSitter.Python.AST
import TreeSitter.Python
import Source.Range
import Source.Span
import Data.ByteString (readFile, ByteString)
import System.IO (FilePath)

main :: IO ()
main = do
  args <- head <$> getArgs
  bytestring <- Data.ByteString.readFile args
