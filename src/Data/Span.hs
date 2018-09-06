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

-- | Source position information (1 indexed)
data Pos = Pos
  { posLine   :: !Int
  , posColumn :: !Int
  }
  deriving (Show, Read, Eq, Ord, Generic, Hashable)

-- | A Span of position information
data Span = Span
  { spanStart :: Pos
  , spanEnd   :: Pos
  }
  deriving (Show, Read, Eq, Ord, Generic, Hashable, Named)


-- Instances

instance Named Pos where nameOf _ = "Position"
instance Message Pos where
  encodeMessage _ Pos{..} = encodeMessageField 1 posLine <> encodeMessageField 2 posColumn
  decodeMessage _ = Pos <$> Decode.at decodeMessageField 1 <*> Decode.at decodeMessageField 2
  dotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim Int64) (Single "line") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim Int64) (Single "column") [] Nothing
    ]

instance A.ToJSON Pos where
  toJSON Pos{..} =
    A.toJSON [posLine, posColumn]

instance A.FromJSON Pos where
  parseJSON arr = do
    [line, col] <- A.parseJSON arr
    pure $ Pos line col

instance Lower Pos where
  lowerBound = Pos 1 1

instance HasDefault Pos where
  def = lowerBound @Pos


instance Message Span where
  encodeMessage _ Span{..} = Encode.embedded 1 (encodeMessage 1 spanStart) <> Encode.embedded 2 (encodeMessage 1 spanEnd)
  decodeMessage _ = Span <$> embeddedAt (decodeMessage 1) 1 <*> embeddedAt (decodeMessage 1) 2
    where embeddedAt parser = Decode.at (Decode.embedded'' parser)
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
  lowerBound = emptySpan

instance HasDefault Span where
  def = emptySpan
