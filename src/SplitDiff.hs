module SplitDiff where

import Data.Record
import Info
import Prologue
import Syntax
import Term (Term, TermF)

-- | A patch to only one side of a diff.
data SplitPatch a = SplitInsert a | SplitDelete a | SplitReplace a
  deriving (Show, Eq, Functor)

-- | Get the term from a split patch.
getSplitTerm :: SplitPatch a -> a
getSplitTerm (SplitInsert a) = a
getSplitTerm (SplitDelete a) = a
getSplitTerm (SplitReplace a) = a

-- | Get the range of a SplitDiff.
getRange :: Functor f => HasField fields Range => SplitDiff f (Record fields) -> Range
getRange diff = byteRange $ case runFree diff of
  Free annotated -> headF annotated
  Pure patch -> extract (getSplitTerm patch)

-- | A diff with only one side’s annotations.
type SplitDiff f annotation = Free (TermF f annotation) (SplitPatch (Term f annotation))
type SplitSyntaxDiff leaf fields = SplitDiff Syntax (Record fields)
