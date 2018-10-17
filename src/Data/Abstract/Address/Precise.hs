{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Precise
( Precise(..)
, runAllocator
, handleAllocator
, runDeref
, handleDeref
) where

import Control.Abstract
import qualified Data.Set as Set
import Prologue

-- | 'Precise' models precise store semantics where only the 'Latest' value is taken. Everything gets it's own address (always makes a new allocation) which makes for a larger store.
newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, NFData)

instance Show Precise where
  showsPrec d = showsUnaryWith showsPrec "Precise" d . unPrecise


runAllocator :: ( Member Fresh effects
                , PureEffects effects
                )
             => Evaluator term Precise value (Allocator Precise ': effects) a
             -> Evaluator term Precise value effects a
runAllocator = interpret handleAllocator

handleAllocator :: Member Fresh effects => Allocator Precise (Eff (Allocator Precise ': effects)) a -> Evaluator term Precise value effects a
handleAllocator (Alloc _) = Precise <$> fresh

runDeref :: PureEffects effects
         => Evaluator term Precise value (Deref value ': effects) a
         -> Evaluator term Precise value effects a
runDeref = interpret handleDeref

handleDeref :: Deref value (Eff (Deref value ': effects)) a -> Evaluator term Precise value effects a
handleDeref (DerefCell        cell) = pure (fst <$> Set.minView cell)
handleDeref (AssignCell value _)    = pure (Set.singleton value)
