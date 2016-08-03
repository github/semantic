module Algorithm where

import Control.Monad.Free.Church
import Diff
import Prologue
import Term

-- | A single step in a diffing algorithm.
data AlgorithmF
  term -- ^ The type of terms.
  diff -- ^ The type of diffs.
  f -- ^ The type representing another level of the diffing algorithm. Often Algorithm.
  -- | Recursively diff two terms and pass the result to the continuation.
  = Recursive term term (diff -> f)
  -- | Diff two arrays and pass the result to the continuation.
  | ByIndex [term] [term] ([diff] -> f)
  | ByRandomWalkSimilarity [term] [term] ([diff] -> f)
  deriving Functor

-- | A lazily-produced AST for diffing.
type Algorithm term diff = F (AlgorithmF term diff)

recursively :: Term leaf annotation -> Term leaf annotation -> Algorithm (Term leaf annotation) (Diff leaf annotation) (Diff leaf annotation)
recursively a b = wrap (Recursive a b pure)

byIndex :: [Term leaf annotation] -> [Term leaf annotation] -> Algorithm (Term leaf annotation) (Diff leaf annotation) [Diff leaf annotation]
byIndex a b = wrap (ByIndex a b pure)

bySimilarity :: [Term leaf annotation] -> [Term leaf annotation] -> Algorithm (Term leaf annotation) (Diff leaf annotation) [Diff leaf annotation]
bySimilarity a b = wrap (ByRandomWalkSimilarity a b pure)
