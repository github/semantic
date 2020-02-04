{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.Address.Precise
( Precise(..)
) where

import           Control.Abstract
import           Control.Algebra
import           Data.Functor.Classes
import qualified Data.Set as Set

-- | 'Precise' models precise store semantics where only the 'Latest' value is taken. Everything gets it's own address (always makes a new allocation) which makes for a larger store.
newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord)

instance Show Precise where
  showsPrec d = showsUnaryWith showsPrec "Precise" d . unPrecise


instance Has Fresh sig m => Algebra (Allocator Precise :+: sig) (AllocatorC Precise m) where
  alg (R other)       = AllocatorC . alg . handleCoercible $ other
  alg (L (Alloc _ k)) = Precise <$> fresh >>= k


instance Algebra sig m => Algebra (Deref value :+: sig) (DerefC Precise value m) where
  alg (R other) = DerefC . alg . handleCoercible $ other
  alg (L op) = case op of
    DerefCell        cell k -> k (fst <$> Set.minView cell)
    AssignCell value _    k -> k (Set.singleton value)
