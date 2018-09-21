module Data.SplitDiff
  ( SplitPatch (..)
  , getRange
  ) where

import Control.Monad.Free
import Data.Location
import Data.Term

-- | A patch to only one side of a diff.
data SplitPatch a
  = SplitInsert { splitTerm :: a }
  | SplitDelete { splitTerm :: a }
  | SplitReplace { splitTerm :: a }
  deriving (Foldable, Eq, Functor, Show, Traversable)

-- | Get the range of a SplitDiff.
getRange :: SplitDiff f Location -> Range
getRange diff = locationByteRange $ case diff of
  Free annotated -> termFAnnotation annotated
  Pure patch -> termAnnotation (splitTerm patch)

-- | A diff with only one side’s annotations.
type SplitDiff syntax ann = Free (TermF syntax ann) (SplitPatch (Term syntax ann))
