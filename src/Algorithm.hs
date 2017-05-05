{-# LANGUAGE GADTs, RankNTypes #-}
module Algorithm where

import Control.Monad.Free.Freer
import Data.These
import Prologue hiding (liftF)

-- | A single step in a diffing algorithm, parameterized by the types of terms, diffs, and the result of the applicable algorithm.
data AlgorithmF term diff result where
  -- | Diff two terms with the choice of algorithm left to the interpreter’s discretion.
  Diff :: term -> term -> AlgorithmF term diff diff
  -- | Diff two terms recursively in O(n) time, resulting in a single diff node.
  Linear :: term -> term -> AlgorithmF term diff diff
  -- | Diff two lists of terms by each element’s similarity in O(n³ log n), resulting in a list of diffs.
  RWS :: [term] -> [term] -> AlgorithmF term diff [diff]
  -- | Delete a term..
  Delete :: term -> AlgorithmF term diff diff
  -- | Insert a term.
  Insert :: term -> AlgorithmF term diff diff
  -- | Replace one term with another.
  Replace :: term -> term -> AlgorithmF term diff diff

-- | The free applicative for 'AlgorithmF'. This enables us to construct diff values using <$> and <*> notation.
type Algorithm term diff = Freer (AlgorithmF term diff)


-- DSL

-- | Diff two terms without specifying the algorithm to be used.
diff :: term -> term -> Algorithm term diff diff
diff = (liftF .) . Diff

-- | Diff a These of terms without specifying the algorithm to be used.
diffThese :: These term term -> Algorithm term diff diff
diffThese = these byDeleting byInserting diff

-- | Diff a pair of optional terms without specifying the algorithm to be used.
diffMaybe :: Maybe term -> Maybe term -> Algorithm term diff (Maybe diff)
diffMaybe a b = case (a, b) of
  (Just a, Just b) -> Just <$> diff a b
  (Just a, _) -> Just <$> byDeleting a
  (_, Just b) -> Just <$> byInserting b
  _ -> pure Nothing

-- | Diff two terms linearly.
linearly :: term -> term -> Algorithm term diff diff
linearly a b = liftF (Linear a b)

-- | Diff two terms using RWS.
byRWS :: [term] -> [term] -> Algorithm term diff [diff]
byRWS a b = liftF (RWS a b)

-- | Delete a term.
byDeleting :: term -> Algorithm term diff diff
byDeleting = liftF . Delete

-- | Insert a term.
byInserting :: term -> Algorithm term diff diff
byInserting = liftF . Insert

-- | Replace one term with another.
byReplacing :: term -> term -> Algorithm term diff diff
byReplacing = (liftF .) . Replace
