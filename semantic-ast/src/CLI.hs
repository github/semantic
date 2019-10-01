{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
module CLI (main) where

import System.Environment
import TreeSitter.Unmarshal
import TreeSitter.Python.AST
import TreeSitter.Python
import Source.Range
import Source.Span
import Data.ByteString (ByteString)


newtype SignalException = SignalException Signal
  deriving (Show, Typeable)
instance Exception SignalException


main :: IO ()
main = do
  args <- getArgs
  parseByteString getArgs
