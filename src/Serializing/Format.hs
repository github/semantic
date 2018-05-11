{-# LANGUAGE GADTs #-}
module Serializing.Format
( Format(..)
, Builder
, runSerialize
) where

import Algebra.Graph.Class
import Data.ByteString.Builder
import Prologue
import Serializing.DOT
import Serializing.SExpression

data Format input where
  DOT         :: (Ord vertex, ToGraph graph, ToVertex graph ~ vertex) => Style vertex Builder -> Format graph
  SExpression :: (Recursive input, ToSExpression (Base input))                                => Format input
  Show        :: Show input                                                                   => Format input

runSerialize :: Format input -> input -> Builder
runSerialize (DOT style) graph = serializeDOT style graph
runSerialize SExpression input = serializeSExpression input
runSerialize Show        input = stringUtf8 (show input)

-- TODO: it would be kinda neat if we could use pretty-show/hscolour for Show output
