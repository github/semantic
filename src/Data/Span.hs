{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | Source position and span information
--
--   Mostly taken from purescript's SourcePos definition.
module Data.Span
( Span(..)
, Pos(..)
, spanFromSrcLoc
, emptySpan
) where

import Data.Aeson ((.=), (.:))
import Proto3.Suite
import Proto3.Wire.Decode as Decode
import Proto3.Wire.Encode as Encode
import qualified Data.Aeson as A
import Data.JSON.Fields
import GHC.Stack
import Prologue

-- | Source position information
data Pos = Pos
  { posLine   :: !Int
  , posColumn :: !Int
  }
  deriving (Show, Read, Eq, Ord, Generic, Hashable)

instance Named Pos where nameOf _ = "Position"
instance Message Pos where
  encodeMessage num Pos{..} = Encode.embedded num (encodePrimitive 1 posLine <> encodePrimitive 2 posColumn)
  -- TODO: FIXME
  decodeMessage = undefined -- fromMaybe def <$> Decode.embedded (decodeMessage (fieldNumber 1))
  dotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim Int64) (Single "line") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim Int64) (Single "column") [] Nothing
    ]

instance HasDefault Pos where
  def = Pos 1 1

instance A.ToJSON Pos where
  toJSON Pos{..} =
    A.toJSON [posLine, posColumn]

instance A.FromJSON Pos where
  parseJSON arr = do
    [line, col] <- A.parseJSON arr
    pure $ Pos line col

instance Lower Pos where
  lowerBound = Pos 1 1

data Span = Span
  { spanStart :: Pos
  , spanEnd   :: Pos
  }
  deriving (Show, Read, Eq, Ord, Generic, Hashable, Named)


instance Message Span where
  encodeMessage num Span{..} = Encode.embedded num (encodeMessage 1 spanStart <> encodeMessage 2 spanEnd)
  decodeMessage = undefined
  dotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim . Named $ Single (nameOf (Proxy @Pos))) (Single "start") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim . Named $ Single (nameOf (Proxy @Pos))) (Single "end") [] Nothing
    ]

spanFromSrcLoc :: SrcLoc -> Span
spanFromSrcLoc = Span . (Pos . srcLocStartLine <*> srcLocStartCol) <*> (Pos . srcLocEndLine <*> srcLocEndCol)

emptySpan :: Span
emptySpan = Span (Pos 1 1) (Pos 1 1)

instance Semigroup Span where
  Span start1 end1 <> Span start2 end2 = Span (min start1 start2) (max end1 end2)

instance A.ToJSON Span where
  toJSON Span{..} =
    A.object [ "start" .= spanStart
             , "end" .= spanEnd
             ]

instance A.FromJSON Span where
  parseJSON = A.withObject "Span" $ \o ->
    Span <$>
      o .: "start" <*>
      o .: "end"

instance ToJSONFields Span where
  toJSONFields sourceSpan = [ "sourceSpan" .= sourceSpan ]

instance Lower Span where
  lowerBound = Span lowerBound lowerBound
