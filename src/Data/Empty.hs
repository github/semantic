{-# LANGUAGE UndecidableInstances #-}

module Data.Empty ( Empty (..) ) where

-- | A typeclass for values that have a sensible notion of an empty value.
-- This is used in Control.Effect to provide a useful default for running
-- a State computation without providing it an explicit starting value.
-- This is very useful if a type has no coherent Monoid instance but
-- needs a value analogous to 'mempty'. It is not recommended to use this
-- for other purposes, as there are no laws by which 'empty' is required
-- to abide.
class Empty a where
  empty :: a

-- | Every Monoid has an Empty instance.
instance {-# OVERLAPS #-} Monoid a => Empty a where
  empty = mempty
