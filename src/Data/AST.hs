{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.AST
  ( Node (..)
  , nodeSpan
  , nodeByteRange
  , AST
  ) where

import Data.Term
import Source.Loc as Loc

-- | An AST node labelled with symbols and source location.
type AST grammar = Term [] (Node grammar)

data Node grammar = Node
  { nodeSymbol    :: !grammar
  , nodeLocation  :: {-# UNPACK #-} !Loc
  }
  deriving (Eq, Ord, Show)

nodeSpan :: Node grammar -> Span
nodeSpan = Loc.span . nodeLocation

nodeByteRange :: Node grammar -> Range
nodeByteRange = byteRange . nodeLocation
