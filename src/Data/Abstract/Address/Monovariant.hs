{-# LANGUAGE GADTs, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Monovariant
( Monovariant(..)
, runAllocator
, handleAllocator
, runDeref
, handleDeref
) where

import Control.Abstract
import Data.Abstract.Name
import qualified Data.Set as Set
import Prologue

-- | 'Monovariant' models using one address for a particular name. It trackes the set of values that a particular address takes and uses it's name to lookup in the store and only allocation if new.
newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord)

instance Show Monovariant where
  showsPrec d = showsUnaryWith showsPrec "Monovariant" d . unMonovariant


runAllocator :: PureEffects effects
             => Evaluator term Monovariant value (Allocator Monovariant ': effects) a
             -> Evaluator term Monovariant value effects a
runAllocator = interpret handleAllocator

handleAllocator :: Allocator Monovariant (Eff (Allocator Monovariant ': effects)) a -> Evaluator term Monovariant value effects a
handleAllocator (Alloc name) = pure (Monovariant name)

runDeref :: ( Member NonDet effects
            , Ord value
            , PureEffects effects
            )
         => Evaluator term Monovariant value (Deref value ': effects) a
         -> Evaluator term Monovariant value effects a
runDeref = interpret handleDeref

handleDeref :: ( Member NonDet effects
               , Ord value
               )
            => Deref value (Eff (Deref value ': effects)) a
            -> Evaluator term Monovariant value effects a
handleDeref (DerefCell        cell) = traverse (foldMapA pure) (nonEmpty (toList cell))
handleDeref (AssignCell value cell) = pure (Set.insert value cell)
