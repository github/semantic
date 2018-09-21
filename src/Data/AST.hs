{-# LANGUAGE DataKinds #-}
module Data.AST
  ( Node (..)
  , nodeSpan
  , nodeByteRange
  , AST
  -- , Location
  -- , nodeLocation
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
  -- , nodeByteRange :: {-# UNPACK #-} !Range
  -- , nodeSpan      :: {-# UNPACK #-} !Span
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

-- nodeLocation :: Node grammar -> Record Location
-- nodeLocation Node{..} = nodeByteRange :. nodeSpan :. Nil
