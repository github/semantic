{-# LANGUAGE GADTs, LambdaCase, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Monovariant
( Monovariant(..)
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


instance Allocatable Monovariant effects where
  allocCell = pure . Monovariant

instance Member NonDet effects => Derefable Monovariant effects where
  derefCell _ = traverse (foldMapA pure) . nonEmpty . toList

  assignCell _ value values = pure (Set.insert value values)


runAllocator :: PureEffects effects
             => Evaluator Monovariant value (Allocator Monovariant ': effects) a
             -> Evaluator Monovariant value effects a
runAllocator = interpret $ \ (Alloc name) -> pure (Monovariant name)

runDeref :: ( Member NonDet effects
            , Ord value
            , PureEffects effects
            )
         => Evaluator Monovariant value (Deref Monovariant value ': effects) a
         -> Evaluator Monovariant value effects a
runDeref = interpret $ \case
  DerefCell  _       cell -> traverse (foldMapA pure) (nonEmpty (toList cell))
  AssignCell _ value cell -> pure (Set.insert value cell)
