module Control.Abstract.Roots
( Live
, askRoots
, extraRoots
) where

import Control.Abstract.Evaluator
import Data.Abstract.Live
import Prologue

-- | Retrieve the local 'Live' set.
askRoots :: Member (Reader (Live address)) effects => Evaluator address value effects (Live address)
askRoots = ask

-- | Run a computation with the given 'Live' set added to the local root set.
extraRoots :: (Member (Reader (Live address)) effects, Ord address) => Live address -> Evaluator address value effects a -> Evaluator address value effects a
extraRoots roots = local (<> roots)
