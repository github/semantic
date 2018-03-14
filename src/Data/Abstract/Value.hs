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
import Prelude hiding (Float, Integer, String)
import qualified Prelude

type ValueConstructors location term
  = '[Closure location term
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

-- | Identical to 'inj', but wraps the resulting sub-entity in a 'Value'.
injValue :: (f :< ValueConstructors location term) => f (Value location term) -> Value location term
injValue = Value . inj

-- | Identical to 'prj', but unwraps the argument out of its 'Value' wrapper.
prjValue :: (f :< ValueConstructors location term) => Value location term -> Maybe (f (Value location term))
prjValue = prj . deValue

-- | Convenience function for projecting two values.
prjPair :: ( f :< ValueConstructors loc term1 , g :< ValueConstructors loc term2)
        => (Value loc term1, Value loc term2)
        -> Maybe (f (Value loc term1), g (Value loc term2))
prjPair = bitraverse prjValue prjValue


-- TODO: Parameterize Value by the set of constructors s.t. each language can have a distinct value union.

-- | A function value consisting of a list of parameters, the body of the function, and an environment of bindings captured by the body.
data Closure location term value = Closure [Name] term (Environment (Address location value))
  deriving (Eq, Generic1, Ord, Show)

instance (Eq location, Eq term) => Eq1 (Closure location term) where liftEq = genericLiftEq
instance (Ord location, Ord term) => Ord1 (Closure location term) where liftCompare = genericLiftCompare
instance (Show location, Show term) => Show1 (Closure location term) where liftShowsPrec = genericLiftShowsPrec

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
type EnvironmentFor value = Environment (Address (LocationFor value) value)

-- | The store for an abstract value type.
type StoreFor value = Store (LocationFor value) value

-- | The cell for an abstract value type.
type CellFor value = Cell (LocationFor value) value

-- | The address set type for an abstract value type.
type LiveFor value = Live (LocationFor value) value


class Ord (LocationFor value) => AbstractValue value where
  -- | The location type (the body of 'Address'es) which should be used for an abstract value type.
  type LocationFor value :: *

instance Ord location => AbstractValue (Value location term) where
  type LocationFor (Value location term) = location

instance AbstractValue Type.Type where
  type LocationFor Type.Type = Monovariant


-- | Value types, e.g. closures, which can root a set of addresses.
class ValueRoots value where
  -- | Compute the set of addresses rooted by a given value.
  valueRoots :: value -> LiveFor value

instance (FreeVariables term, Ord location) => ValueRoots (Value location term) where
  valueRoots v
    | Just (Closure names body env) <- prjValue v = envRoots env (foldr Set.delete (freeVariables (body :: term)) names)
    | otherwise                                   = mempty

instance ValueRoots Type.Type where
  valueRoots _ = mempty
