{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving #-}
module Analysis.Abstract.Dead
( Dead(..)
, revivingTerms
, killingModules
, providingDeadSet
) where

import Control.Abstract
import Data.Abstract.Module
import Data.Semigroup.Reducer as Reducer
import Data.Set (delete)
import Prologue

-- | A set of “dead” (unreachable) terms.
newtype Dead term = Dead { unDead :: Set term }
  deriving (Eq, Foldable, Lower, Monoid, Ord, Semigroup, Show)

deriving instance Ord term => Reducer term (Dead term)

-- | Update the current 'Dead' set.
killAll :: (Member (State (Dead term)) sig, Carrier sig m) => Dead term -> Evaluator term address value m ()
killAll = put

-- | Revive a single term, removing it from the current 'Dead' set.
revive :: (Member (State (Dead term)) sig, Carrier sig m, Ord term) => term -> Evaluator term address value m ()
revive t = modify (Dead . delete t . unDead)

-- | Compute the set of all subterms recursively.
subterms :: (Ord term, Recursive term, Foldable (Base term)) => term -> Dead term
subterms term = term `cons` para (foldMap (uncurry cons)) term


revivingTerms :: ( Member (State (Dead term)) sig
                , Ord term
                , Carrier sig m
                )
              => Open (term -> Evaluator term address value m a)
revivingTerms recur term = revive term *> recur term

killingModules :: ( Foldable (Base term)
                 , Member (State (Dead term)) sig
                 , Ord term
                 , Recursive term
                 , Carrier sig m
                 )
               => Open (Module term -> Evaluator term address value m a)
killingModules recur m = killAll (subterms (moduleBody m)) *> recur m

providingDeadSet :: Evaluator term address value (StateC (Dead term) (Evaluator term address value m)) a
                 -> Evaluator term address value m (Dead term, a)
providingDeadSet = runState lowerBound . runEvaluator
