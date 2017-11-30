{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Abstract.Address where

import Data.Functor.Classes

newtype Address l a = Address { unAddress :: l }
  deriving (Eq, Ord, Show)

instance Foldable (Address l) where
  foldMap _ = mempty

instance Functor (Address l) where
  fmap _ = Address . unAddress

instance Traversable (Address l) where
  traverse _ = fmap Address . pure . unAddress


instance Eq2 Address where
  liftEq2 eqL _ (Address a) (Address b) = eqL a b

instance Eq l => Eq1 (Address l) where
  liftEq = liftEq2 (==)

instance Ord2 Address where
  liftCompare2 compareL _ (Address a) (Address b) = compareL a b

instance Ord l => Ord1 (Address l) where
  liftCompare = liftCompare2 compare

instance Show2 Address where
  liftShowsPrec2 spL _ _ _ d = showsUnaryWith spL "Address" d . unAddress

instance Show l => Show1 (Address l) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
