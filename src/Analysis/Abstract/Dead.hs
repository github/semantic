{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}
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
killAll :: Member (State (Dead term)) effects => Dead term -> Evaluator term address value effects ()
killAll = put

-- | Revive a single term, removing it from the current 'Dead' set.
revive :: (Member (State (Dead term)) effects, Ord term) => term -> Evaluator term address value effects ()
revive t = modify' (Dead . delete t . unDead)

-- | Compute the set of all subterms recursively.
subterms :: (Ord term, Recursive term, Foldable (Base term)) => term -> Dead term
subterms term = term `cons` para (foldMap (uncurry cons)) term


revivingTerms :: ( Corecursive term
                 , Member (State (Dead term)) effects
                 , Ord term
                 )
              => SubtermAlgebra (Base term) term (Evaluator term address value effects a)
              -> SubtermAlgebra (Base term) term (Evaluator term address value effects a)
revivingTerms recur term = revive (embedSubterm term) *> recur term

killingModules :: ( Foldable (Base term)
                  , Member (State (Dead term)) effects
                  , Ord term
                  , Recursive term
                  )
               => Open (Module term -> Evaluator term address value effects a)
killingModules recur m = killAll (subterms (moduleBody m)) *> recur m

providingDeadSet :: Effects effects => Evaluator term address value (State (Dead term) ': effects) a -> Evaluator term address value effects (Dead term, a)
providingDeadSet = runState lowerBound
