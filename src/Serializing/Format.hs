{-# LANGUAGE GADTs #-}
module Serializing.Format
( Format(..)
, Builder
) where

import Algebra.Graph.Class
import Data.ByteString.Builder

data Format input where
  DOT :: (Ord vertex, ToGraph graph, ToVertex graph ~ vertex) => Style vertex Builder -> Format graph
