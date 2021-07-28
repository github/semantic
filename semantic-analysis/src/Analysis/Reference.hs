module Analysis.Reference
( -- * Reference
  Reference(..)
) where

import Source.Span
import System.Path as Path

-- Reference

data Reference = Reference
  { refPath :: Path.AbsRelFile
  , refSpan :: Span
  }
  deriving (Eq, Ord, Show)
-- FIXME: add this to some sort of static context carried in analyses
