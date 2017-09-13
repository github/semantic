module SplitDiff where

import Control.Monad.Free
import Data.Record
import Info
import Term

-- | A patch to only one side of a diff.
data SplitPatch a
  = SplitInsert { splitTerm :: a }
  | SplitDelete { splitTerm :: a }
  | SplitReplace { splitTerm :: a }
  deriving (Show, Eq, Functor)

-- | Get the range of a SplitDiff.
getRange :: Functor f => HasField fields Range => SplitDiff f (Record fields) -> Range
getRange diff = byteRange $ case diff of
  Free annotated -> termAnnotation annotated
  Pure patch -> extract (splitTerm patch)

-- | A diff with only one side’s annotations.
type SplitDiff f annotation = Free (TermF f annotation) (SplitPatch (Term f annotation))
