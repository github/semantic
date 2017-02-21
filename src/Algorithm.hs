{-# LANGUAGE GADTs #-}
module Algorithm where

import Control.Applicative.Free
import Prologue hiding (Pure)

-- | A single step in a diffing algorithm, parameterized by the types of terms, diffs, and the result of the applicable algorithm.
data AlgorithmF term diff result where
  -- | Recursively diff two terms and pass the result to the continuation.
  Recursive :: term -> term -> AlgorithmF term diff diff
  -- | Diff two lists by each element’s position, and pass the resulting list of diffs to the continuation.
  ByIndex :: [term] -> [term] -> AlgorithmF term diff [diff]
  -- | Diff two lists by each element’s similarity and pass the resulting list of diffs to the continuation.
  BySimilarity :: [term] -> [term] -> AlgorithmF term diff [diff]

-- | The free applicative for 'AlgorithmF'. This enables us to construct diff values using <$> and <*> notation.
type Algorithm term diff = Ap (AlgorithmF term diff)

-- | Tear down an Ap by iteration.
iterAp :: Functor g => (g a -> a) -> Ap g a -> a
iterAp algebra = go
  where go (Pure a) = a
        go (Ap underlying apply) = algebra (go . (apply <*>) . pure <$> underlying)


-- DSL

-- | Constructs a 'Recursive' diff of two terms.
recursively :: term -> term -> Algorithm term diff diff
recursively a b = liftAp (Recursive a b)

-- | Constructs a 'ByIndex' diff of two lists of terms.
byIndex :: [term] -> [term] -> Algorithm term diff [diff]
byIndex a b = liftAp (ByIndex a b)

-- | Constructs a 'BySimilarity' diff of two lists of terms.
bySimilarity :: [term] -> [term] -> Algorithm term diff [diff]
bySimilarity a b = liftAp (BySimilarity a b)
