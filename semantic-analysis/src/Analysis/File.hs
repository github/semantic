{-# LANGUAGE DeriveTraversable #-}
module Analysis.File
( File(..)
, fromBody
) where

import Data.Maybe (fromJust, listToMaybe)
import GHC.Stack
import Source.Span
import qualified System.Path as Path

data File a = File
  { filePath :: !Path.AbsRelFile
  , fileSpan :: {-# UNPACK #-} !Span
  , fileBody :: !a
  }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

fromBody :: HasCallStack => a -> File a
fromBody body = File (Path.absRel (srcLocFile srcLoc)) (spanFromSrcLoc srcLoc) body where
  srcLoc = snd (fromJust (listToMaybe (getCallStack callStack)))
