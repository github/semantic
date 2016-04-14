module SplitDiff where

import Info
import Control.Comonad.Cofree ()
import Control.Monad.Free (Free(..))
import Data.Copointed
import Diff (Annotated(..))
import Range
import Term (Term)

-- | A patch to only one side of a diff.
data SplitPatch a = SplitInsert a | SplitDelete a | SplitReplace a
  deriving (Show, Eq, Functor)

-- | Get the term from a split patch.
getSplitTerm :: SplitPatch a -> a
getSplitTerm (SplitInsert a) = a
getSplitTerm (SplitDelete a) = a
getSplitTerm (SplitReplace a) = a

-- | Get the range of a SplitDiff.
getRange :: SplitDiff leaf Info -> Range
getRange diff = characterRange $ case diff of
  Free annotated -> annotation annotated
  Pure patch -> copoint (getSplitTerm patch)

-- | A diff with only one side’s annotations.
type SplitDiff leaf annotation = Free (Annotated leaf annotation) (SplitPatch (Term leaf annotation))
