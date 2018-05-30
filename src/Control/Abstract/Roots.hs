module Control.Abstract.Roots
( Live
, askRoots
, extraRoots
) where

import Control.Abstract.Evaluator
import Data.Abstract.Live
import Prologue

-- | Retrieve the local 'Live' set.
askRoots :: Member (Reader (Live location)) effects => Evaluator location value effects (Live location)
askRoots = ask

-- | Run a computation with the given 'Live' set added to the local root set.
extraRoots :: (Member (Reader (Live location)) effects, Ord location) => Live location -> Evaluator location value effects a -> Evaluator location value effects a
extraRoots roots = local (<> roots)
