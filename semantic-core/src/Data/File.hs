{-# LANGUAGE DeriveTraversable #-}
module Data.File
( File(..)
, fromBody
) where

import Data.Loc
import Data.Maybe (fromJust)
import GHC.Stack

data File a = File
  { fileLoc  :: !Loc
  , fileBody :: !a
  }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

fromBody :: HasCallStack => a -> File a
fromBody body = File (fromJust (stackLoc callStack)) body
