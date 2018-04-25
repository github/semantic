{-# LANGUAGE DefaultSignatures, UndecidableInstances #-}
module Data.Semilattice.Lower
( Lower (..)
) where

class Lower s where
  -- | The greatest lower bound of @s@.
  --
  --   Laws:
  --
  --   If @s@ is 'Bounded', we require 'lower' and 'minBound' to agree:
  --
  --   > lower = minBound
  --
  --   If @s@ is a 'Join' semilattice, 'lower' must be the identity of '(\/)':
  --
  --   > lower \/ a = a
  --
  --   If @s@ is 'Ord'ered, 'lower' must be at least as small as every terminating value:
  --
  --   > compare lower a /= GT
  lower :: s
  default lower :: Bounded s => s
  lower = minBound

--- | Every Monoid has a Lower instance.
instance {-# OVERLAPS #-} Monoid a => Lower a where
  lower = mempty
