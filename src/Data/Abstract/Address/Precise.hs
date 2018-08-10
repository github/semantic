{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.Address.Precise
( Precise(..)
) where

import Control.Abstract.Addressable
import Control.Monad.Effect.Fresh
import qualified Data.Set as Set
import Prologue

-- | 'Precise' models precise store semantics where only the 'Latest' value is taken. Everything gets it's own address (always makes a new allocation) which makes for a larger store.
newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord)

instance Show Precise where
  showsPrec d = showsUnaryWith showsPrec "Precise" d . unPrecise


instance Member Fresh effects => Allocatable Precise effects where
  allocCell _ = Precise <$> fresh

  assignCell _ value _ = pure (Set.singleton value)

instance Derefable Precise effects where
  derefCell _ = pure . fmap fst . Set.minView
