{-# LANGUAGE OverloadedStrings #-}

module Stack.Edge
  ( Edge (..)
  ) where

import Data.Text (Text)
import Stack.Node (Node)

-- | This is suitable for conversion from (label, node, node) tuples.
data Edge = Edge
  { sourceNode :: Node
  , sinkNode   :: Node
  , label      :: Text
  } deriving (Eq, Show, Ord)
