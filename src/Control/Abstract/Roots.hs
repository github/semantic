{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
module Control.Abstract.Roots
( ValueRoots(..)
, Live
, askRoots
, extraRoots
) where

import Control.Abstract.Evaluator
import Data.Abstract.Live

-- | Value types, e.g. closures, which can root a set of addresses.
class ValueRoots address value where
  -- | Compute the set of addresses rooted by a given value.
  valueRoots :: value -> Live address

-- | Retrieve the local 'Live' set.
askRoots :: (Member (Reader (Live address)) sig, Carrier sig m) => Evaluator term address value m (Live address)
askRoots = ask

-- | Run a computation with the given 'Live' set added to the local root set.
extraRoots :: (Member (Reader (Live address)) sig, Carrier sig m, Ord address) => Live address -> Evaluator term address value m a -> Evaluator term address value m a
extraRoots roots = local (<> roots)
