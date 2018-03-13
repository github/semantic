{-# LANGUAGE ConstraintKinds, DataKinds, FunctionalDependencies, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators #-}
module Data.Abstract.Value where

import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Store
import Data.Abstract.FreeVariables
import Data.Abstract.Live
import qualified Data.Abstract.Type as Type
import qualified Data.Set as Set
import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import Prologue
import Prelude hiding (Float, Integer, String, Rational, fail)
import qualified Prelude

type ValueConstructors location term
  = '[Closure location term
    , Interface location
    , Unit
    , Boolean
    , Float
    , Integer
    , String
    , Rational
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

-- | A generalized number type that unifies all interpretable numeric types.
--   This is a GADT, so you can specialize the 'a' parameter and be confident
--   that, say, a @Number Scientific@ contains a 'Scientific' and not an integer
--   in disguise.
data Number a where
  Whole :: Prelude.Integer  -> Number Prelude.Integer
  Ratio :: Prelude.Rational -> Number Prelude.Rational
  Decim :: Scientific       -> Number Scientific

deriving instance Eq a => Eq (Number a)

instance Show (Number a) where
  show (Whole i) = show i
  show (Ratio r) = show r
  show (Decim d) = show d

-- | Every 'Number' can be coerced to a 'Scientific'. Used in the 'Ord' instance.
collapse :: Number a -> Scientific
collapse (Whole i) = fromInteger i
collapse (Ratio r) = fromRational r
collapse (Decim s) = s

instance Eq a => Ord (Number a) where compare = compare `on` collapse

-- | A box that hides the @a@ parameter to a given 'Number'. Pattern-match
--   on it to extract the information contained.
data SomeNumber = forall a . SomeNumber (Number a)

-- | Smart constructors for 'SomeNumber'.
whole :: Prelude.Integer -> SomeNumber
whole = SomeNumber . Whole

ratio :: Prelude.Rational -> SomeNumber
ratio = SomeNumber . Ratio

decim :: Scientific -> SomeNumber
decim = SomeNumber . Decim

-- | Promote a function on 'Real' values into one operating on 'Number's.
--   You pass things like @+@ and @-@ here.
liftSimple :: (forall n . Real n => n -> n -> n)
           -> (Number a -> Number b -> SomeNumber)
liftSimple f = liftThorny f f

-- | Promote two functions, one on 'Integral' and one on 'Fractional' and 'Real' values,
--   to operate on 'Numbers'. Examples of this: 'mod' and 'mod'', 'div' and '/'.
liftThorny :: (forall n . Integral n             => n -> n -> n)
           -> (forall f . (Fractional f, Real f) => f -> f -> f)
           -> (Number a -> Number b -> SomeNumber)
liftThorny f _ (Whole i) (Whole j) = whole (f i j)
liftThorny _ g (Whole i) (Ratio j) = ratio (g (toRational i) j)
liftThorny _ g (Whole i) (Decim j) = decim (g (fromIntegral i) j)
liftThorny _ g (Ratio i) (Ratio j) = ratio (g i j)
liftThorny _ g (Ratio i) (Whole j) = ratio (g i (fromIntegral j))
liftThorny _ g (Ratio i) (Decim j) = decim (g (fromRational i) j)
liftThorny _ g (Decim i) (Whole j) = decim (g i (fromIntegral j))
liftThorny _ g (Decim i) (Ratio j) = decim (g i (fromRational j))
liftThorny _ g (Decim i) (Decim j) = decim (g i j)

-- | Exponential behavior is too hard to generalize, so here's a manually implemented version.
--   TODO: Given a 'Ratio' raised to some 'Whole', we could check to see if it's an integer
--   and round it before the exponentiation, giving back a 'Whole'.
safeExp :: Number a -> Number b -> SomeNumber
safeExp (Whole i) (Whole j) = whole (i ^ j)
safeExp (Ratio i) (Whole j) = ratio (i ^^ j)
safeExp i j                 = decim (fromFloatDigits ((munge i) ** (munge j)))
  where munge = (toRealFloat . collapse) :: Number a -> Double

-- TODO: Parameerize Value by the set of constructors s.t. each language can have a distinct value union.

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

-- | Float values.
newtype Float value = Float (Number Scientific)
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

-- | The cell for an abstract value type.
type CellFor value = Cell (LocationFor value) value

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
