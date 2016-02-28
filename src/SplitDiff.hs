module SplitDiff where

import Diff (Annotated)
import Control.Monad.Free (Free)
import Term (Term)

-- | A patch to only one side of a diff.
data SplitPatch a = SplitInsert a | SplitDelete a | SplitReplace a
  deriving (Show, Eq)

-- | A diff with only one side’s annotations.
type SplitDiff leaf annotation = Free (Annotated leaf annotation) (SplitPatch (Term leaf annotation))
