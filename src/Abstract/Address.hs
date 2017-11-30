{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TypeFamilyDependencies #-}
module Abstract.Address where

import Abstract.FreeVariables
import Data.Functor.Classes
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Functor.Classes.Show.Generic
import Data.Pointed
import Data.Semigroup
import GHC.Generics

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


newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)


newtype I a = I { unI :: a }
  deriving (Eq, Generic1, Ord, Show)

instance Semigroup (I a) where
  (<>) = const

instance Foldable I where
  foldMap f = f . unI

instance Functor I where
  fmap f = I . f . unI

instance Traversable I where
  traverse f = fmap I . f . unI

instance Pointed I where
  point = I

instance Eq1 I where liftEq = genericLiftEq
instance Ord1 I where liftCompare = genericLiftCompare
instance Show1 I where liftShowsPrec = genericLiftShowsPrec
