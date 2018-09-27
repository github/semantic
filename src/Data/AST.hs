{-# LANGUAGE DataKinds #-}
module Data.AST
  ( Node (..)
  , nodeSpan
  , nodeByteRange
  , AST
  ) where

import Data.Location
import Data.Term
import Data.Aeson
import Data.Text (pack)
import Data.JSON.Fields

-- | An AST node labelled with symbols and source location.
type AST syntax grammar = Term syntax (Node grammar)

data Node grammar = Node
  { nodeSymbol    :: !grammar
  , nodeLocation  :: {-# UNPACK #-} !Location
  }
  deriving (Eq, Ord, Show)


instance Show grammar => ToJSONFields (Node grammar) where
  toJSONFields Node{..} =
    [ "symbol" .= pack (show nodeSymbol)
    , "span"   .= locationSpan nodeLocation
    ]

nodeSpan :: Node grammar -> Span
nodeSpan = locationSpan . nodeLocation

nodeByteRange :: Node grammar -> Range
nodeByteRange = locationByteRange . nodeLocation
