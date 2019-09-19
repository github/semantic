{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Span
( Span(..)
, Pos(..)
) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as A
import           GHC.Generics (Generic)
import           TreeSitter.Node
import           TreeSitter.Unmarshal

data Span = Span
  { spanStart :: {-# UNPACK #-} !Pos
  , spanEnd   :: {-# UNPACK #-} !Pos
  }
  deriving (Eq, Ord, Generic, Show)

instance A.ToJSON Span where
  toJSON s = A.object
    [ "start" .= spanStart s
    , "end"   .= spanEnd   s
    ]

instance Unmarshal Span where
  unmarshalNodes _ = do
    node <- peekNode
    case node of
      Just node -> do
        let spanStart = pointToPos (nodeStartPoint node)
            spanEnd = pointToPos (nodeEndPoint node)
        pure (Span spanStart spanEnd)
      Nothing -> fail "expected a node but didn't get one"
    where pointToPos (TSPoint line column) = Pos (fromIntegral line) (fromIntegral column)


data Pos = Pos
  { posLine   :: {-# UNPACK #-} !Int
  , posColumn :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Generic, Show)

instance A.ToJSON Pos where
  toJSON p = A.toJSON [posLine p, posColumn p]
