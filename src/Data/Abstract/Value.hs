{-# LANGUAGE ConstraintKinds, DataKinds, FunctionalDependencies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Data.Abstract.Value where

import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Store
import Data.Abstract.FreeVariables
import Data.Abstract.Live
import qualified Data.Abstract.Type as Type
import qualified Data.Set as Set
import Data.Scientific (Scientific)
import Prologue
import Prelude hiding (Float, Integer, String, fail)
import qualified Prelude

type ValueConstructors location term
  = '[Closure location term
    , Interface location
    , Unit
    , Boolean
    , Float
    , Integer
    , String
    , Tuple
    ]

-- | Open union of primitive values that terms can be evaluated to.
--   Fix by another name.
newtype Value location term = Value { deValue :: Union (ValueConstructors location term) (Value location term) }
  deriving (Eq, Show, Ord)


injValue :: (f :< ValueConstructors location term) => f (Value location term) -> Value location term
injValue = Value . inj

prjValue :: (f :< ValueConstructors location term) => Value location term -> Maybe (f (Value location term))
prjValue = prj . deValue

-- TODO: Parameterize Value by the set of constructors s.t. each language can have a distinct value union.

-- | A function value consisting of a list of parameters, the body of the function, and an environment of bindings captured by the body.
data Closure location term value = Closure [Name] term (Environment location value)
  deriving (Eq, Generic1, Ord, Show)

instance (Eq location, Eq term) => Eq1 (Closure location term) where liftEq = genericLiftEq
instance (Ord location, Ord term) => Ord1 (Closure location term) where liftCompare = genericLiftCompare
instance (Show location, Show term) => Show1 (Closure location term) where liftShowsPrec = genericLiftShowsPrec

-- | A program value consisting of the value of the program and it's enviornment of bindings.
data Interface location value = Interface value (Environment location value)
  deriving (Eq, Generic1, Ord, Show)

instance (Eq location) => Eq1 (Interface location) where liftEq = genericLiftEq
instance (Ord location) => Ord1 (Interface location) where liftCompare = genericLiftCompare
instance (Show location) => Show1 (Interface location) where liftShowsPrec = genericLiftShowsPrec

-- | The unit value. Typically used to represent the result of imperative statements.
data Unit value = Unit
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Unit where liftEq = genericLiftEq
instance Ord1 Unit where liftCompare = genericLiftCompare
instance Show1 Unit where liftShowsPrec = genericLiftShowsPrec

-- | Boolean values.
newtype Boolean value = Boolean Prelude.Bool
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Boolean where liftEq = genericLiftEq
instance Ord1 Boolean where liftCompare = genericLiftCompare
instance Show1 Boolean where liftShowsPrec = genericLiftShowsPrec

-- | Arbitrary-width integral values.
newtype Integer value = Integer Prelude.Integer
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Integer where liftEq = genericLiftEq
instance Ord1 Integer where liftCompare = genericLiftCompare
instance Show1 Integer where liftShowsPrec = genericLiftShowsPrec

-- | String values.
newtype String value = String ByteString
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 String where liftEq = genericLiftEq
instance Ord1 String where liftCompare = genericLiftCompare
instance Show1 String where liftShowsPrec = genericLiftShowsPrec

-- | Float values.
newtype Float value = Float Scientific
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Float where liftEq = genericLiftEq
instance Ord1 Float where liftCompare = genericLiftCompare
instance Show1 Float where liftShowsPrec = genericLiftShowsPrec

-- Zero or more values.
-- TODO: Investigate whether we should use Vector for this.
-- TODO: Should we have a Some type over a nonemmpty list? Or does this merit one?

newtype Tuple value = Tuple [value]
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Tuple where liftEq = genericLiftEq
instance Ord1 Tuple where liftCompare = genericLiftCompare
instance Show1 Tuple where liftShowsPrec = genericLiftShowsPrec

-- | The environment for an abstract value type.
type EnvironmentFor v = Environment (LocationFor v) v

-- | The store for an abstract value type.
type StoreFor v = Store (LocationFor v) v

-- | The location type (the body of 'Address'es) which should be used for an abstract value type.
type family LocationFor value :: * where
  LocationFor (Value location term) = location
  LocationFor Type.Type = Monovariant

-- | Value types, e.g. closures, which can root a set of addresses.
class ValueRoots l v | v -> l where
  -- | Compute the set of addresses rooted by a given value.
  valueRoots :: v -> Live l v

instance (FreeVariables term, Ord location) => ValueRoots location (Value location term) where
  valueRoots v
    | Just (Closure names body env) <- prjValue v = envRoots env (foldr Set.delete (freeVariables (body :: term)) names)
    | otherwise                                   = mempty

instance ValueRoots Monovariant Type.Type where
  valueRoots _ = mempty
