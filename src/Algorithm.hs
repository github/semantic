module Algorithm where

import Control.Monad.Free.Church
import Prologue

-- | A single step in a diffing algorithm.
data AlgorithmF
  term -- ^ The type of terms.
  diff -- ^ The type of diffs.
  f -- ^ The type representing another level of the diffing algorithm. Often Algorithm.
  -- | Recursively diff two terms and pass the result to the continuation.
  = Recursive term term (diff -> f)
  -- | Diff two arrays and pass the result to the continuation.
  | ByIndex [term] [term] ([diff] -> f)
  | BySimilarity [term] [term] ([diff] -> f)
  deriving Functor

-- | A lazily-produced AST for diffing.
type Algorithm term diff = F (AlgorithmF term diff)

recursively :: term -> term -> Algorithm term diff diff
recursively a b = wrap (Recursive a b pure)

byIndex :: [term] -> [term] -> Algorithm term diff [diff]
byIndex a b = wrap (ByIndex a b pure)

bySimilarity :: [term] -> [term] -> Algorithm term diff [diff]
bySimilarity a b = wrap (BySimilarity a b pure)
