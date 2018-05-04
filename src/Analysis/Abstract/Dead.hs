{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s Evaluator constraint
module Analysis.Abstract.Dead
( DeadCode
) where

import Control.Abstract.Analysis
import Data.Abstract.Module
import Data.Semigroup.Reducer as Reducer
import Data.Set (delete)
import Prologue

-- | An analysis tracking dead (unreachable) code.
newtype DeadCode m (effects :: [* -> *]) a = DeadCode { runDeadCode :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance Evaluator location term value m => Evaluator location term value (DeadCode m)

-- | A set of “dead” (unreachable) terms.
newtype Dead term = Dead { unDead :: Set term }
  deriving (Eq, Foldable, Semigroup, Monoid, Ord, Show)

deriving instance Ord term => Reducer term (Dead term)

-- | Update the current 'Dead' set.
killAll :: (Effectful m, Member (State (Dead term)) effects) => Dead term -> DeadCode m effects ()
killAll = raise . put

-- | Revive a single term, removing it from the current 'Dead' set.
revive :: (Effectful m, Member (State (Dead term)) effects) => Ord term => term -> DeadCode m effects ()
revive t = raise (modify (Dead . delete t . unDead))

-- | Compute the set of all subterms recursively.
subterms :: (Ord term, Recursive term, Foldable (Base term)) => term -> Dead term
subterms term = term `cons` para (foldMap (uncurry cons)) term


instance ( Corecursive term
         , Effectful m
         , Foldable (Base term)
         , Member (State (Dead term)) outer
         , AnalyzeTerm location term value inner outer m
         , Ord term
         , Recursive term
         )
      => AnalyzeTerm location term value inner outer (DeadCode m) where
  analyzeTerm recur term = do
    revive (embedSubterm term)
    DeadCode (analyzeTerm (runDeadCode . recur) term)

instance ( Corecursive term
         , Effectful m
         , Foldable (Base term)
         , Member (State (Dead term)) outer
         , AnalyzeModule location term value inner outer m
         , Ord term
         , Recursive term
         )
      => AnalyzeModule location term value inner outer (DeadCode m) where
  analyzeModule recur m = do
    killAll (subterms (subterm (moduleBody m)))
    DeadCode (analyzeModule (runDeadCode . recur) m)

instance ( Evaluator location term value m
         , Interpreter m effects
         , Ord term
         )
      => Interpreter (DeadCode m) (State (Dead term) ': effects) where
  type Result (DeadCode m) (State (Dead term) ': effects) result = Result m effects (result, Dead term)
  interpret = interpret . runDeadCode . raiseHandler (`runState` mempty)
