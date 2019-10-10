{-# LANGUAGE DeriveTraversable #-}
module Data.File
( File(..)
, fromBody
) where

import Data.Loc
import Data.Maybe (fromJust)
import GHC.Stack
import Source.Span

data File a = File
  { filePath :: !Path
  , fileSpan :: {-# UNPACK #-} !Span
  , fileBody :: !a
  }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

fromBody :: HasCallStack => a -> File a
fromBody body = File path span body where
  (path, span) = fromJust (stackLoc callStack)
