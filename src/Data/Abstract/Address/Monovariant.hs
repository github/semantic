{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Monovariant
( Monovariant(..)
) where

import Control.Abstract
import Control.Effect.Carrier
import Data.Abstract.Name
import qualified Data.Set as Set
import Prologue

-- | 'Monovariant' models using one address for a particular name. It tracks the set of values that a particular address takes and uses it's name to lookup in the store and only allocation if new.
newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord)

instance Show Monovariant where
  showsPrec d = showsUnaryWith showsPrec "Monovariant" d . unMonovariant


instance Carrier sig m => Carrier (Allocator Monovariant :+: sig) (AllocatorC Monovariant m) where
  eff (L (Alloc name k)) = k (Monovariant name)
  eff (R other) = AllocatorC . eff . handleCoercible $ other

instance (Ord value, Carrier sig m, Alternative m, Monad m) => Carrier (Deref value :+: sig) (DerefC Monovariant value m) where
  eff (L (DerefCell cell k)) = traverse (foldMapA pure) (nonEmpty (toList cell)) >>= k
  eff (L (AssignCell value cell k)) = k (Set.insert value cell)
  eff (R other) = DerefC . eff . handleCoercible $ other
