module Parser where

import Prologue
import Source

-- | A function that takes a source blob and returns an annotated AST.
-- | The return is in the IO monad because some of the parsers are written in C
-- | and aren't pure.
type Parser f a = SourceBlob -> IO (Cofree f a)
