{-# LANGUAGE GADTs #-}
module Serializing.Format
( Format(..)
, Builder
, runSerialize
, SomeFormat(..)
, Options(..)
) where

import Algebra.Graph.Class
import Data.Aeson (ToJSON(..), fromEncoding)
import Data.ByteString.Builder
import Prologue
import Serializing.DOT
import Serializing.SExpression

data Format input where
  DOT         :: (Ord vertex, ToGraph graph, ToVertex graph ~ vertex) => Style vertex Builder -> Format graph
  JSON        :: ToJSON input                                                                 => Format input
  SExpression :: (Recursive input, ToSExpression (Base input))        => Options              -> Format input
  Show        :: Show input                                                                   => Format input

runSerialize :: Bool -> Format input -> input -> Builder
runSerialize _ (DOT style)        = serializeDOT style
runSerialize _ JSON               = (<> "\n") . fromEncoding . toEncoding
runSerialize _ (SExpression opts) = serializeSExpression opts
runSerialize _ Show               = stringUtf8 . show

-- TODO: it would be kinda neat if we could use pretty-show/hscolour for Show output


-- | Abstract over a 'Format'’s input type.
data SomeFormat where
  SomeFormat :: Format input -> SomeFormat
