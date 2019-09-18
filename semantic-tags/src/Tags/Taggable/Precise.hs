{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Tags.Taggable.Precise
( Python(..)
) where

import           Data.Aeson as A
import           Data.Text (Text)
import           GHC.Generics (Generic)
import qualified TreeSitter.Python.AST as Python

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

data Tag = Tag
  { name :: Text
  , kind :: Text
  , span :: Span
  , context :: [Text]
  , line :: Maybe Text
  , docs :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Tag


newtype Python a = Python { getPython :: Python.Module a }
