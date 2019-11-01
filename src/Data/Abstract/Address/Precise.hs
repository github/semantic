{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Address.Precise
( Precise(..)
) where

import Control.Abstract
import Control.Abstract.ScopeGraph (AllocatorC(..))
import Control.Effect.Carrier
import qualified Data.Set as Set
import Prologue

-- | 'Precise' models precise store semantics where only the 'Latest' value is taken. Everything gets it's own address (always makes a new allocation) which makes for a larger store.
newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord)

instance Show Precise where
  showsPrec d = showsUnaryWith showsPrec "Precise" d . unPrecise


instance (Member Fresh sig, Carrier sig m) => Carrier (Allocator Precise :+: sig) (AllocatorC Precise m) where
  eff (R other) = AllocatorC . eff . handleCoercible $ other
  eff (L (Alloc _ k)) = Precise <$> fresh >>= k


instance Carrier sig m => Carrier (Deref value :+: sig) (DerefC Precise value m) where
  eff (R other) = DerefC . eff . handleCoercible $ other
  eff (L op) = case op of
    DerefCell        cell k -> k (fst <$> Set.minView cell)
    AssignCell value _    k -> k (Set.singleton value)
