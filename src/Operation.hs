module Operation where

import Diff
import Data.OrderedMap
import qualified Data.Text as T
import Term

-- | A single step in a diffing algorithm.
data Operation
  a -- ^ The type of leaves in the syntax tree, typically String, but possibly some datatype representing different leaves more precisely.
  annotation -- ^ The type of annotations.
  f -- ^ The type representing another level of the diffing algorithm. Often Algorithm.
  =
  -- | Recursively diff two terms and pass the result to the continuation.
  Recursive (Term a annotation) (Term a annotation) (Diff a annotation -> f)
  -- | Diff two dictionaries and pass the result to the continuation.
  | ByKey (OrderedMap T.Text (Term a annotation)) (OrderedMap T.Text (Term a annotation)) (OrderedMap T.Text (Diff a annotation) -> f)
  -- | Diff two arrays and pass the result to the continuation.
  | ByIndex [Term a annotation] [Term a annotation] ([Diff a annotation] -> f)
  deriving Functor
