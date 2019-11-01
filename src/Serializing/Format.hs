{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings #-}
module Serializing.Format
( Format(..)
, FormatStyle(..)
, Builder
, runSerialize
, Options(..)
) where

import Algebra.Graph.Export.Dot
import Algebra.Graph.ToGraph
import Data.Aeson (ToJSON (..), fromEncoding)
import Data.ByteString.Builder
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise
import Prologue
import Data.ProtoLens.Encoding as Proto
import Data.ProtoLens.Message (Message)
import Serializing.SExpression
import Text.Show.Pretty

data Format input where
  DOT         :: (Ord vertex, ToGraph graph, ToVertex graph ~ vertex) => Style vertex Builder -> Format graph
  JSON        :: ToJSON input                                         =>                         Format input
  SExpression :: (Recursive input, ToSExpression (Base input))        => Options              -> Format input
  Show        :: Show input                                           =>                         Format input
  Proto       :: Message input                                        =>                         Format input

data FormatStyle = Colourful | Plain

runSerialize :: FormatStyle -> Format input -> input -> Builder
runSerialize _         (DOT style)        = export style
runSerialize _         JSON               = (<> "\n") . fromEncoding . toEncoding
runSerialize _         (SExpression opts) = serializeSExpression opts
runSerialize Colourful Show               = (<> "\n") . stringUtf8 . hscolour TTY defaultColourPrefs False False "" False . ppShow
runSerialize Plain     Show               = (<> "\n") . stringUtf8 . show
runSerialize _         Proto              = Proto.buildMessage
