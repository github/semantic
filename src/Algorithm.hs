module Algorithm where

import Diff
import Prologue
import Term

-- | A single step in a diffing algorithm.
data AlgorithmF
  a -- ^ The type of leaves in the syntax tree, typically String, but possibly some datatype representing different leaves more precisely.
  annotation -- ^ The type of annotations.
  f -- ^ The type representing another level of the diffing algorithm. Often Algorithm.
  -- | Recursively diff two terms and pass the result to the continuation.
  = Recursive (Term a annotation) (Term a annotation) (Diff a annotation -> f)
  -- | Diff two arrays and pass the result to the continuation.
  | ByIndex [Term a annotation] [Term a annotation] ([Diff a annotation] -> f)
  | ByRandomWalkSimilarity [Term a annotation] [Term a annotation] ([Diff a annotation] -> f)
  deriving Functor

-- | A lazily-produced AST for diffing.
type Algorithm a annotation = Free (AlgorithmF a annotation)

recursively :: Term leaf annotation -> Term leaf annotation -> Algorithm leaf annotation (Diff leaf annotation)
recursively a b = wrap (Recursive a b pure)
