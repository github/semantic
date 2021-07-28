module Analysis.Reference
( -- * Reference
  Reference(..)
  -- * Constructors
, fromPath
) where

import Source.Span
import System.Path as Path
import System.Path.PartClass as Path.PartClass

-- Reference

data Reference = Reference
  { refPath :: Path.AbsRelFile
  , refSpan :: Span
  }
  deriving (Eq, Ord, Show)
-- FIXME: add this to some sort of static context carried in analyses


-- Constructors

fromPath :: Path.PartClass.AbsRel ar => Path.File ar -> Reference
fromPath p = Reference (Path.toAbsRel p) (point (Pos 0 0))
