module Row where

import Data.Functor.Both as Both
import Line
import Prelude hiding (fst, snd)

-- | A row in a split diff, composed of a before line and an after line.
type Row a = Both (Line a)
