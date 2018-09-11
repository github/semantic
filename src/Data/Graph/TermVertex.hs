{-# LANGUAGE DeriveAnyClass, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Graph.TermVertex
( TermVertex(..)
, TermAnnotation(..)
) where

import Prologue

import           Data.Aeson
import           Data.Graph
import           Data.JSON.Fields
import           Data.Range
import           Data.Span
import           GHC.Exts (fromList)
import           Proto3.Suite
import qualified Proto3.Suite as PB
import           Proto3.Wire.Decode as Decode
import           Proto3.Wire.Encode as Encode

-- Terms

data TermVertex
  = TermVertex
  { vertexId :: Int
  , vertexTermName :: String
  , vertexAnnotation :: TermAnnotation
  } deriving (Eq, Ord, Show, Generic, Named)

data TermAnnotation
  = TermAnnotation
  { range :: Range
  , span :: Span
  } deriving (Eq, Ord, Show, Generic, Named)


-- Instances

instance Named (Graph TermVertex) where nameOf _ = "TermGraph"
instance Message (Graph TermVertex) where
  encodeMessage _ graph =  encodeMessageField 1 (NestedVec (fromList (vertexList graph)))
                        <> encodeMessageField 2 (NestedVec (fromList (edgeList graph)))
  decodeMessage = error "decodeMessage not implemented for (Graph TermVertex)"
  dotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Repeated . Named $ Single "TermVertex") (Single "vertices") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Repeated . Named $ Single "TermEdge")  (Single "edges") [] Nothing
    ]

instance Named (Edge TermVertex) where nameOf _ = "TermEdge"
instance Message (Edge TermVertex) where
  encodeMessage _ (Edge (from, to)) = encodePrimitive 1 (uniqueTag from) <> encodePrimitive 2 (uniqueTag to)
  decodeMessage = error "decodeMessage not implemented for (Edge TermVertex)"
  dotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim PB.Int64) (Single "from") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim PB.Int64) (Single "to")   [] Nothing
    ]

instance Message TermVertex where
  encodeMessage _ TermVertex{..}
    =  encodeMessageField 1 vertexId
    <> encodeMessageField 2 vertexTermName
    <> Encode.embedded 3 (encodeMessage 0 vertexAnnotation)
  decodeMessage _
    =   TermVertex
    <$> Decode.at decodeMessageField 1
    <*> Decode.at decodeMessageField 2
    <*> embeddedAt (decodeMessage 0) 3
    where embeddedAt parser = Decode.at (Decode.embedded'' parser)
  dotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim PB.Int64) (Single "id") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim PB.String) (Single "name") [] Nothing
    , DotProtoMessageField $ DotProtoField 3 (Prim . Named $ Single "TermAnnotation") (Single "annotation") [] Nothing
    ]

instance VertexTag TermVertex where uniqueTag = vertexId

instance ToJSON TermVertex where
  toJSON TermVertex{..} = object $
    [ "id"   .= vertexId
    , "term" .= vertexTermName
    ] <> toJSONFields vertexAnnotation
  toEncoding TermVertex{..} = pairs . fold $
      "id"   .= vertexId
    : "term" .= vertexTermName
    : toJSONFields vertexAnnotation

instance Message TermAnnotation where
  encodeMessage _ TermAnnotation{..} = Encode.embedded 1 (encodeMessage 0 range) <> Encode.embedded 2 (encodeMessage 0 span)
  decodeMessage _ = TermAnnotation <$> embeddedAt (decodeMessage 0) 1 <*> embeddedAt (decodeMessage 0) 2
    where embeddedAt parser = Decode.at (Decode.embedded'' parser)
  dotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim . Named $ Single (nameOf (Proxy @Range))) (Single "range") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim . Named $ Single (nameOf (Proxy @Span))) (Single "span") [] Nothing
    ]

instance HasDefault TermAnnotation where
  def = TermAnnotation def def

instance ToJSON TermAnnotation where
  toJSON TermAnnotation{..} = object $ toJSONFields range <> toJSONFields span

instance ToJSONFields TermAnnotation where
  toJSONFields TermAnnotation{..} = toJSONFields range <> toJSONFields span
