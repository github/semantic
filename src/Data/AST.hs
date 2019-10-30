{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.AST
  ( Node (..)
  , nodeSpan
  , nodeByteRange
  , AST
  ) where

import Data.Term
import Data.Aeson
import Data.Text (pack)
import Data.JSON.Fields
import Source.Loc as Loc

-- | An AST node labelled with symbols and source location.
type AST syntax grammar = Term syntax (Node grammar)

data Node grammar = Node
  { nodeSymbol    :: !grammar
  , nodeLocation  :: {-# UNPACK #-} !Loc
  }
  deriving (Eq, Ord, Show)


instance Show grammar => ToJSONFields (Node grammar) where
  toJSONFields Node{..} =
    [ "symbol" .= pack (show nodeSymbol)
    , "span"   .= Loc.span nodeLocation
    ]

nodeSpan :: Node grammar -> Span
nodeSpan = Loc.span . nodeLocation

nodeByteRange :: Node grammar -> Range
nodeByteRange = byteRange . nodeLocation
