{-# LANGUAGE GADTs, RankNTypes #-}
module Algorithm where

import Control.Applicative.Free
import Prologue hiding (Pure)

-- | A single step in a diffing algorithm, parameterized by the types of terms, diffs, and the result of the applicable algorithm.
data AlgorithmF term diff result where
  -- | Diff two terms recursively in O(n) time, resulting in a single diff node.
  Linear :: term -> term -> AlgorithmF term diff diff
  -- | Diff two lists of terms by each element’s position in O(n³) time, resulting in a list of diffs.
  SES :: [term] -> [term] -> AlgorithmF term diff [diff]
  -- | Diff two lists of terms by each element’s similarity in O(n³ log n), resulting in a list of diffs.
  RWS :: [term] -> [term] -> AlgorithmF term diff [diff]

-- | The free applicative for 'AlgorithmF'. This enables us to construct diff values using <$> and <*> notation.
type Algorithm term diff = Ap (AlgorithmF term diff)

-- | Tear down an Ap by iteration.
iterAp :: Functor g => (g a -> a) -> Ap g a -> a
iterAp algebra = go
  where go (Pure a) = a
        go (Ap underlying apply) = algebra (go . (apply <*>) . pure <$> underlying)

-- | Tear down an Ap by iteration, given a continuation.
iterAp' :: (forall x. g x -> (x -> a) -> a) -> Ap g a -> a
iterAp' algebra = go
  where go (Pure a) = a
        go (Ap underlying apply) = algebra underlying (go . (apply <*>) . pure)


-- DSL

-- | Constructs a 'Recursive' diff of two terms.
linearly :: term -> term -> Algorithm term diff diff
linearly a b = liftAp (Linear a b)

-- | Constructs a 'ByIndex' diff of two lists of terms.
bySES :: [term] -> [term] -> Algorithm term diff [diff]
bySES a b = liftAp (SES a b)

-- | Constructs a 'BySimilarity' diff of two lists of terms.
byRWS :: [term] -> [term] -> Algorithm term diff [diff]
byRWS a b = liftAp (RWS a b)
