{-# LANGUAGE DataKinds #-}
module Data.AST where

import Data.Range
import Data.Record
import Data.Span
import Data.Term

-- | An AST node labelled with symbols and source location.
type AST syntax grammar = Term syntax (Node grammar)

data Node grammar = Node
  { nodeSymbol :: !grammar
  , nodeByteRange :: {-# UNPACK #-} !Range
  , nodeSpan :: {-# UNPACK #-} !Span
  }
  deriving (Eq, Show)

-- | A location specified as possibly-empty intervals of bytes and line/column positions.
type Location = '[Range, Span]

nodeLocation :: Node grammar -> Record Location
nodeLocation Node{..} = nodeByteRange :. nodeSpan :. Nil
