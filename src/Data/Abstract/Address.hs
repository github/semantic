{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TypeFamilyDependencies #-}
module Data.Abstract.Address where

import Data.Abstract.FreeVariables
import Data.Functor.Classes.Generic
import Data.Pointed
import Data.Semigroup
import Data.Set
import GHC.Generics

-- | An abstract address with a location of @l@ pointing to a variable of type @a@.
newtype Address l a = Address { unAddress :: l }
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

instance Eq l => Eq1 (Address l) where liftEq = genericLiftEq
instance Ord l => Ord1 (Address l) where liftCompare = genericLiftCompare
instance Show l => Show1 (Address l) where liftShowsPrec = genericLiftShowsPrec


-- | 'Precise' models precise store semantics where only the 'Latest' value is taken. Everything gets it's own address (always makes a new allocation) which makes for a larger store.
newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

-- | 'Monovariant' models using one address for a particular name. It trackes the set of values that a particular address takes and uses it's name to lookup in the store and only allocation if new.
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
