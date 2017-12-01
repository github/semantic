{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TypeFamilyDependencies #-}
module Data.Abstract.Address where

import Data.Abstract.FreeVariables
import Data.Functor.Classes
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Functor.Classes.Show.Generic
import Data.Pointed
import Data.Semigroup
import Data.Set
import GHC.Generics

-- | An abstract address with a location of @l@ pointing to a variable of type @a@.
newtype Address l a = Address { unAddress :: l }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


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


-- | 'Precise' models precise store semantics where only the 'Latest' value is taken.
newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)


type family Cell l = res | res -> l where
  Cell Precise = Latest
  Cell Monovariant = Set


newtype Latest a = Latest { unLatest :: a }
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

instance Semigroup (Latest a) where
  (<>) = flip const

instance Pointed Latest where
  point = Latest

instance Eq1 Latest where liftEq = genericLiftEq
instance Ord1 Latest where liftCompare = genericLiftCompare
instance Show1 Latest where liftShowsPrec = genericLiftShowsPrec
