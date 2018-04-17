{-# LANGUAGE UndecidableInstances #-}

module Data.Empty where

class Empty a where
  empty :: a

instance {-# OVERLAPS #-} Monoid a => Empty a where
  empty = mempty
