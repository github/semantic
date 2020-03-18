{-# LANGUAGE OverloadedStrings #-}

module Stack.Edge
  ( Edge (..)
  , parseEdges
  , formatEdge
  ) where

import           Control.Comonad
import           Data.Text (Text)
import qualified Data.Text as T
import           Stack.Node (Node)

-- | This is suitable for conversion from (label, node, node) tuples.
data Edge = Edge
  { sourceNode :: Node
  , sinkNode   :: Node
  , label      :: Text
  } deriving (Eq, Show)

parseEdges :: Text -> [Edge]
parseEdges = const []

formatEdge :: Edge -> Text
formatEdge (Edge src sink lab) =
  get src <> ":" <> get sink <> ":" <> lab
    where get = T.pack . show . extract

