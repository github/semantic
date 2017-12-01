{-# LANGUAGE ConstraintKinds, DataKinds, FunctionalDependencies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Data.Abstract.Value where

import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Live
import qualified Data.Abstract.Type as Type
import Data.ByteString (ByteString)
import Data.Functor.Classes.Generic
import Data.Semigroup
import qualified Data.Set as Set
import Data.Union
import GHC.Generics
import Prelude hiding (Integer, String)
import qualified Prelude

type ValueConstructors location
  = '[Closure location
    , Unit
    , Boolean
    , Integer
    , String
    ]

type Value location = Union (ValueConstructors location)

data Closure location term = Closure [Name] term (Environment location (Value location term))
  deriving (Eq, Generic1, Ord, Show)

instance (Eq location) => Eq1 (Closure location) where liftEq = genericLiftEq
instance (Ord location) => Ord1 (Closure location) where liftCompare = genericLiftCompare
instance (Show location) => Show1 (Closure location) where liftShowsPrec = genericLiftShowsPrec

data Unit term = Unit
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Unit where liftEq = genericLiftEq
instance Ord1 Unit where liftCompare = genericLiftCompare
instance Show1 Unit where liftShowsPrec = genericLiftShowsPrec

newtype Boolean term = Boolean Prelude.Bool
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Boolean where liftEq = genericLiftEq
instance Ord1 Boolean where liftCompare = genericLiftCompare
instance Show1 Boolean where liftShowsPrec = genericLiftShowsPrec

newtype Integer term = Integer Prelude.Integer
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Integer where liftEq = genericLiftEq
instance Ord1 Integer where liftCompare = genericLiftCompare
instance Show1 Integer where liftShowsPrec = genericLiftShowsPrec

newtype String term = String ByteString
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 String where liftEq = genericLiftEq
instance Ord1 String where liftCompare = genericLiftCompare
instance Show1 String where liftShowsPrec = genericLiftShowsPrec


type family LocationFor value :: * where
  LocationFor (Value location term) = location
  LocationFor Type.Type = Monovariant


-- Instances

class ValueRoots l v | v -> l where
  valueRoots :: v -> Live l v

class AbstractValue v where
  unit :: v
  integer :: Prelude.Integer -> v
  boolean :: Bool -> v
  string :: ByteString -> v

instance (FreeVariables term, Ord location) => ValueRoots location (Value location term) where
  valueRoots v
    | Just (Closure names body env) <- prj v = envRoots env (foldr Set.delete (freeVariables body) names)
    | otherwise                              = mempty

instance AbstractValue (Value location term) where
  unit = inj Unit
  integer = inj . Integer
  boolean = inj . Boolean
  string = inj . String

instance ValueRoots Monovariant Type.Type where
  valueRoots _ = mempty

instance AbstractValue Type.Type where
  unit = Type.Unit
  integer _ = Type.Int
  boolean _ = Type.Bool
  string _ = Type.String
