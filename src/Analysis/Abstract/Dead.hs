{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Analysis.Abstract.Dead
( Dead(..)
, revivingTerms
, killingModules
, providingDeadSet
) where

import Control.Abstract
import Control.Carrier.State.Strict
import Data.Abstract.Module
import Data.Functor.Foldable
import Data.Semigroup.Reducer as Reducer
import Data.Semilattice.Lower
import Data.Set (Set, delete)

-- | A set of “dead” (unreachable) terms.
newtype Dead term = Dead { unDead :: Set term }
  deriving (Eq, Foldable, Lower, Monoid, Ord, Semigroup, Show)

deriving instance Ord term => Reducer term (Dead term)

-- | Update the current 'Dead' set.
killAll :: (Has (State (Dead term)) sig m) => Dead term -> Evaluator term address value m ()
killAll = put

-- | Revive a single term, removing it from the current 'Dead' set.
revive :: (Has (State (Dead term)) sig m, Ord term) => term -> Evaluator term address value m ()
revive t = modify (Dead . delete t . unDead)

-- | Compute the set of all subterms recursively.
subterms :: (Ord term, Recursive term, Foldable (Base term)) => term -> Dead term
subterms term = term `cons` para (foldMap (uncurry cons)) term


revivingTerms :: ( Has (State (Dead term)) sig m
                 , Ord term
                 )
              => Open (term -> Evaluator term address value m a)
revivingTerms recur term = revive term *> recur term

killingModules :: ( Foldable (Base term)
                  , Has (State (Dead term)) sig m
                  , Ord term
                  , Recursive term
                  )
               => Open (Module term -> Evaluator term address value m a)
killingModules recur m = killAll (subterms (moduleBody m)) *> recur m

providingDeadSet :: Evaluator term address value (StateC (Dead term) (Evaluator term address value m)) a
                 -> Evaluator term address value m (Dead term, a)
providingDeadSet = runState lowerBound . runEvaluator
