module Row where

import Data.Functor.Both as Both
import Line
import Prelude hiding (fst, snd)

-- | A row in a split diff, composed of a before line and an after line.
type Row a = Both (Line a)

-- | Merge open lines and prepend closed lines (as determined by a pair of functions) onto a list of rows.
adjoinRowsBy :: Both (a -> Bool) -> Row a -> [Row a] -> [Row a]
adjoinRowsBy _ row [] = [ row ]
adjoinRowsBy f row (nextRow : rows) = zipWithDefaults both mempty (coalesceLinesBy <$> f <*> row <*> nextRow) ++ rows
