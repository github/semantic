{-# LANGUAGE GADTs #-}
module Serializing.Format
( Format(..)
, FormatStyle(..)
, Builder
, runSerialize
, Options(..)
) where

import Algebra.Graph.Class
import Data.Aeson (ToJSON(..), fromEncoding)
import Data.ByteString.Builder
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise
import Prologue
import Serializing.DOT
import Serializing.SExpression
import Text.Show.Pretty

data Format input where
  DOT         :: (Ord vertex, ToGraph graph, ToVertex graph ~ vertex) => Style vertex Builder -> Format graph
  JSON        :: ToJSON input                                                                 => Format input
  SExpression :: (Recursive input, ToSExpression (Base input))        => Options              -> Format input
  Show        :: Show input                                                                   => Format input

data FormatStyle = Colourful | Plain

runSerialize :: FormatStyle -> Format input -> input -> Builder
runSerialize _         (DOT style)        = serializeDOT style
runSerialize _         JSON               = (<> "\n") . fromEncoding . toEncoding
runSerialize _         (SExpression opts) = serializeSExpression opts
runSerialize Colourful Show               = (<> "\n") . stringUtf8 . hscolour TTY defaultColourPrefs False False "" False . ppShow
runSerialize Plain     Show               = (<> "\n") . stringUtf8 . show
