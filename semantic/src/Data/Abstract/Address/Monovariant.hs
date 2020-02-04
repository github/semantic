{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.Address.Monovariant
( Monovariant(..)
) where

import           Analysis.Name
import           Control.Abstract
import           Control.Algebra
import           Data.Foldable
import           Data.Functor.Classes
import           Data.List.NonEmpty (nonEmpty)
import qualified Data.Set as Set

-- | 'Monovariant' models using one address for a particular name. It tracks the set of values that a particular address takes and uses it's name to lookup in the store and only allocation if new.
newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord)

instance Show Monovariant where
  showsPrec d = showsUnaryWith showsPrec "Monovariant" d . unMonovariant


instance Algebra sig m => Algebra (Allocator Monovariant :+: sig) (AllocatorC Monovariant m) where
  alg (L (Alloc name k)) = k (Monovariant name)
  alg (R other)          = AllocatorC . alg . handleCoercible $ other

instance (Ord value, Algebra sig m, Alternative m, Monad m) => Algebra (Deref value :+: sig) (DerefC Monovariant value m) where
  alg (L (DerefCell cell k))        = traverse (foldMapA pure) (nonEmpty (toList cell)) >>= k
  alg (L (AssignCell value cell k)) = k (Set.insert value cell)
  alg (R other)                     = DerefC . alg . handleCoercible $ other
