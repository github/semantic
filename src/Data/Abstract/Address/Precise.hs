{-# LANGUAGE GADTs, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Precise
( Precise(..)
) where

import Control.Abstract
import qualified Data.Set as Set
import Prologue

-- | 'Precise' models precise store semantics where only the 'Latest' value is taken. Everything gets it's own address (always makes a new allocation) which makes for a larger store.
newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord)

instance Show Precise where
  showsPrec d = showsUnaryWith showsPrec "Precise" d . unPrecise


instance Member Fresh effects => Allocatable Precise effects where
  allocCell _ = Precise <$> fresh

instance Derefable Precise effects where
  derefCell _ = pure . fmap fst . Set.minView

  assignCell _ value _ = pure (Set.singleton value)


runAllocator :: ( Member Fresh effects
                , PureEffects effects
                )
             => Evaluator Precise value (Allocator Precise ': effects) a
             -> Evaluator Precise value effects a
runAllocator = interpret handleAllocator

handleAllocator :: Member Fresh effects => Allocator Precise (Eff (Allocator Precise ': effects)) a -> Evaluator Precise value effects a
handleAllocator (Alloc _) = Precise <$> fresh

runDeref :: PureEffects effects
         => Evaluator Precise value (Deref Precise value ': effects) a
         -> Evaluator Precise value effects a
runDeref = interpret handleDeref

handleDeref :: Deref Precise value (Eff (Deref Precise value ': effects)) a -> Evaluator Precise value effects a
handleDeref (DerefCell  _       cell) = pure (fst <$> Set.minView cell)
handleDeref (AssignCell _ value _)    = pure (Set.singleton value)
