{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Span
( Span(..)
, Pos(..)
) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as A
import           GHC.Generics (Generic)

data Span = Span
  { spanStart :: {-# UNPACK #-} !Pos
  , spanEnd   :: {-# UNPACK #-} !Pos
  }
  deriving (Eq, Ord, Generic)

instance Show Span where
  showsPrec _ s = shows (spanStart s) . showString ".." . shows (spanEnd s)

instance A.ToJSON Span where
  toJSON s = A.object
    [ "start" .= spanStart s
    , "end"   .= spanEnd   s
    ]

data Pos = Pos
  { posLine   :: {-# UNPACK #-} !Int
  , posColumn :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Generic)

instance Show Pos where
  showsPrec _ p = showChar '[' . shows (posLine p) . showString ", " . shows (posColumn p) . showChar ']'

instance A.ToJSON Pos where
  toJSON p = A.toJSON [posLine p, posColumn p]
