module SplitDiff where

import Syntax
import Control.Monad.Trans.Free (Free)
import Control.Comonad.Trans.Cofree
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
type SplitDiff leaf annotation = Free (CofreeF (Syntax leaf) annotation) (SplitPatch (Term leaf annotation))
