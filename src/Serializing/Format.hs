{-# LANGUAGE GADTs #-}
module Serializing.Format
( Format(..)
, Builder
, runSerialize
) where

import Algebra.Graph.Class
import Data.ByteString.Builder
import Serializing.DOT

data Format input where
  DOT  :: (Ord vertex, ToGraph graph, ToVertex graph ~ vertex) => Style vertex Builder -> Format graph
  Show :: Show input                                                                   => Format input

runSerialize :: Format input -> input -> Builder
runSerialize (DOT style) graph = serializeDOT style graph
runSerialize Show        input = stringUtf8 (show input)
