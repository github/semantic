module SplitDiff where

import Control.Monad.Free (Free(..))
import Data.Copointed (copoint)
import Diff (Annotated(..))
import Term (Term)

-- | A patch to only one side of a diff.
data SplitPatch a = SplitInsert a | SplitDelete a | SplitReplace a
  deriving (Show, Eq, Functor)

-- | Get the term from a split patch.
getSplitTerm :: SplitPatch a -> a
getSplitTerm (SplitInsert a) = a
getSplitTerm (SplitDelete a) = a
getSplitTerm (SplitReplace a) = a

-- | A diff with only one side’s annotations.
type SplitDiff leaf annotation = Free (Annotated leaf annotation) (SplitPatch (Term leaf annotation))

-- | Get the Info from a split diff
getSplitAnnotation :: SplitDiff leaf annotation -> annotation
getSplitAnnotation (Pure patch) = copoint (getSplitTerm patch)
getSplitAnnotation (Free annotated) = annotation annotated
