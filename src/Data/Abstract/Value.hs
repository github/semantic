{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
module Data.Abstract.Value where

import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Heap
import Data.Abstract.Live
import Data.Abstract.Number
import qualified Data.Abstract.Type as Type
import Data.Scientific (Scientific)
import Prologue
import Prelude hiding (Float, Integer, String, Rational, fail)
import qualified Prelude

type ValueConstructors
  = '[Array
    , Boolean
    , Class
    , Closure
    , Float
    , Integer
    , String
    , Rational
    , Symbol
    , Tuple
    , Unit
    ]

-- | Open union of primitive values that terms can be evaluated to.
--   Fix by another name.
newtype Value = Value { deValue :: Union ValueConstructors Value }
  deriving (Eq, Show, Ord)

-- | Identical to 'inj', but wraps the resulting sub-entity in a 'Value'.
injValue :: (f :< ValueConstructors) => f Value -> Value
injValue = Value . inj

-- | Identical to 'prj', but unwraps the argument out of its 'Value' wrapper.
prjValue :: (f :< ValueConstructors) => Value -> Maybe (f Value)
prjValue = prj . deValue

-- | Convenience function for projecting two values.
prjPair :: (f :< ValueConstructors , g :< ValueConstructors)
        => (Value, Value)
        -> Maybe (f Value, g Value)
prjPair = bitraverse prjValue prjValue

-- TODO: Parameterize Value by the set of constructors s.t. each language can have a distinct value union.

-- | A function value consisting of a list of parameter 'Name's, a 'Label' to jump to the body of the function, and an 'Environment' of bindings captured by the body.
data Closure value = Closure [Name] Label (Environment Precise value)
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Closure where liftEq = genericLiftEq
instance Ord1 Closure where liftCompare = genericLiftCompare
instance Show1 Closure where liftShowsPrec = genericLiftShowsPrec

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
newtype Integer value = Integer (Number Prelude.Integer)
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Integer where liftEq = genericLiftEq
instance Ord1 Integer where liftCompare = genericLiftCompare
instance Show1 Integer where liftShowsPrec = genericLiftShowsPrec

-- | Arbitrary-width rational values values.
newtype Rational value = Rational (Number Prelude.Rational)
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Rational where liftEq = genericLiftEq
instance Ord1 Rational where liftCompare = genericLiftCompare
instance Show1 Rational where liftShowsPrec = genericLiftShowsPrec

-- | String values.
newtype String value = String ByteString
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 String where liftEq = genericLiftEq
instance Ord1 String where liftCompare = genericLiftCompare
instance Show1 String where liftShowsPrec = genericLiftShowsPrec

-- | Possibly-interned Symbol values.
--   TODO: Should this store a 'Text'?
newtype Symbol value = Symbol ByteString
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Symbol where liftEq = genericLiftEq
instance Ord1 Symbol where liftCompare = genericLiftCompare
instance Show1 Symbol where liftShowsPrec = genericLiftShowsPrec

-- | Float values.
newtype Float value = Float (Number Scientific)
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Float where liftEq = genericLiftEq
instance Ord1 Float where liftCompare = genericLiftCompare
instance Show1 Float where liftShowsPrec = genericLiftShowsPrec

-- | Zero or more values. Fixed-size at interpretation time.
--   TODO: Investigate whether we should use Vector for this.
--   TODO: Should we have a Some type over a nonemmpty list? Or does this merit one?
newtype Tuple value = Tuple [value]
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Tuple where liftEq = genericLiftEq
instance Ord1 Tuple where liftCompare = genericLiftCompare
instance Show1 Tuple where liftShowsPrec = genericLiftShowsPrec

-- | Zero or more values. Dynamically resized as needed at interpretation time.
--   TODO: Vector? Seq?
newtype Array value = Array [value]
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Array where liftEq = genericLiftEq
instance Ord1 Array where liftCompare = genericLiftCompare
instance Show1 Array where liftShowsPrec = genericLiftShowsPrec

data Class value = Class
  { _className  :: Name
  , _classScope :: Environment Precise value
  } deriving (Eq, Generic1, Ord, Show)

instance Eq1 Class where liftEq = genericLiftEq
instance Ord1 Class where liftCompare = genericLiftCompare
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec

-- | The environment for an abstract value type.
type EnvironmentFor v = Environment (LocationFor v) v

-- | The exports for an abstract value type.
type ExportsFor v = Exports (LocationFor v) v

-- | The 'Heap' for an abstract value type.
type HeapFor value = Heap (LocationFor value) value

-- | The cell for an abstract value type.
type CellFor value = Cell (LocationFor value) value

-- | The address set type for an abstract value type.
type LiveFor value = Live (LocationFor value) value

-- | The location type (the body of 'Address'es) which should be used for an abstract value type.
type family LocationFor value :: *
type instance LocationFor Value = Precise
type instance LocationFor Type.Type = Monovariant

-- | Value types, e.g. closures, which can root a set of addresses.
class ValueRoots value where
  -- | Compute the set of addresses rooted by a given value.
  valueRoots :: value -> LiveFor value

instance ValueRoots Value where
  valueRoots v
    | Just (Closure _ _ env) <- prjValue v = envAll env
    | otherwise                            = mempty

instance ValueRoots Type.Type where
  valueRoots _ = mempty
