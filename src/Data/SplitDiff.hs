module Data.SplitDiff where

import Control.Monad.Free
import Data.Record
import Data.Term
import Info

-- | A patch to only one side of a diff.
data SplitPatch a
  = SplitInsert { splitTerm :: a }
  | SplitDelete { splitTerm :: a }
  | SplitReplace { splitTerm :: a }
  deriving (Foldable, Eq, Functor, Show, Traversable)

-- | Get the range of a SplitDiff.
getRange :: Functor f => HasField fields Range => SplitDiff f (Record fields) -> Range
getRange diff = byteRange $ case diff of
  Free annotated -> termFAnnotation annotated
  Pure patch -> termAnnotation (splitTerm patch)

-- | A diff with only one side’s annotations.
type SplitDiff syntax ann = Free (TermF syntax ann) (SplitPatch (Term syntax ann))

unSplit :: Functor syntax => SplitDiff syntax ann -> Term syntax ann
unSplit = iter Term . fmap splitTerm
