module Analysis.Reference
( -- * Reference
  Reference(..)
  -- * Constructors
, fromPath
) where

import Source.Span

-- Reference

data Reference = Reference
  { refPath :: FilePath
  , refSpan :: Span
  }
  deriving (Eq, Ord, Show)


-- Constructors

fromPath :: FilePath -> Reference
fromPath p = Reference p (point (Pos 0 0))
