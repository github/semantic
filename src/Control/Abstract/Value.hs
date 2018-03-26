{-# LANGUAGE GADTs, MultiParamTypeClasses, Rank2Types, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Value
( MonadValue(..)
, Comparator(..)
, while
, doWhile
, forLoop
, toBool
, makeNamespace
, ValueRoots(..)
, ValueExc(..)
, EnvironmentFor
, ExportsFor
, HeapFor
, CellFor
, LiveFor
, LocationFor
, ConfigurationFor
) where

import Control.Abstract.Evaluator
import Data.Abstract.FreeVariables
import Data.Abstract.Environment as Env
import Data.Abstract.Address (Address)
import Data.Abstract.Number as Number
import Data.Scientific (Scientific)
import Data.Semigroup.Reducer hiding (unit)
import Prelude
import Prologue

-- | This datum is passed into liftComparison to handle the fact that Ruby and PHP
--   have built-in generalized-comparison ("spaceship") operators. If you want to
--   encapsulate a traditional, boolean-returning operator, wrap it in 'Concrete';
--   if you want the generalized comparator, pass in 'Generalized'. In MonadValue
--   instances, you can then then handle the different cases to return different
--   types, if that's what you need.
data Comparator
  = Concrete (forall a . Ord a => a -> a -> Bool)
  | Generalized

-- | A 'Monad' abstracting the evaluation of (and under) binding constructs (functions, methods, etc).
--
--   This allows us to abstract the choice of whether to evaluate under binders for different value types.
class (Monad m, Show value) => MonadValue value m where
  -- | Construct an abstract unit value.
  --   TODO: This might be the same as the empty tuple for some value types
  unit :: m value

  -- | Construct an abstract integral value.
  integer :: Prelude.Integer -> m value

  -- | Lift a unary operator over a 'Num' to a function on 'value's.
  liftNumeric  :: (forall a . Num a => a -> a)
               -> (value -> m value)

  -- | Lift a pair of binary operators to a function on 'value's.
  --   You usually pass the same operator as both arguments, except in the cases where
  --   Haskell provides different functions for integral and fractional operations, such
  --   as division, exponentiation, and modulus.
  liftNumeric2 :: (forall a b. Number a -> Number b -> SomeNumber)
               -> (value -> value -> m value)

  -- | Lift a Comparator (usually wrapping a function like == or <=) to a function on values.
  liftComparison :: Comparator -> (value -> value -> m value)

  -- | Lift a unary bitwise operator to values. This is usually 'complement'.
  liftBitwise :: (forall a . Bits a => a -> a)
              -> (value -> m value)

  -- | Lift a binary bitwise operator to values. The Integral constraint is
  --   necessary to satisfy implementation details of Haskell left/right shift,
  --   but it's fine, since these are only ever operating on integral values.
  liftBitwise2 :: (forall a . (Integral a, Bits a) => a -> a -> a)
               -> (value -> value -> m value)

  -- | Construct an abstract boolean value.
  boolean :: Bool -> m value

  -- | Construct an abstract string value.
  string :: ByteString -> m value

  -- | Construct a self-evaluating symbol value.
  --   TODO: Should these be interned in some table to provide stronger uniqueness guarantees?
  symbol :: ByteString -> m value

  -- | Construct a floating-point value.
  float :: Scientific -> m value

  -- | Construct a rational value.
  rational :: Prelude.Rational -> m value

  -- | Construct an N-ary tuple of multiple (possibly-disjoint) values
  multiple :: [value] -> m value

  -- | Construct an array of zero or more values.
  array :: [value] -> m value

  -- | Construct a key-value pair for use in a hash.
  kvPair :: value -> value -> m value

  -- | Extract the contents of a key-value pair as a tuple.
  asPair :: value -> m (value, value)

  -- | Construct a hash out of pairs.
  hash :: [(value, value)] -> m value

  -- | Extract a 'ByteString' from a given value.
  asString :: value -> m ByteString

  -- | Eliminate boolean values. TODO: s/boolean/truthy
  ifthenelse :: value -> m a -> m a -> m a

  -- | Construct the nil/null datatype.
  null :: m value

  -- | Build a class value from a name and environment.
  klass :: Name                 -- ^ The new class's identifier
        -> [value]              -- ^ A list of superclasses
        -> EnvironmentFor value -- ^ The environment to capture
        -> m value

  -- | Build a namespace value from a name and environment stack
  --
  -- Namespaces model closures with monoidal environments.
  namespace :: Name                 -- ^ The namespace's identifier
            -> EnvironmentFor value -- ^ The environment to mappend
            -> m value

  -- | Extract the environment from any scoped object (e.g. classes, namespaces, etc).
  scopedEnvironment :: value -> m (EnvironmentFor value)

  -- | Evaluate an abstraction (a binder like a lambda or method definition).
  abstract :: (FreeVariables term, MonadControl term m) => [Name] -> Subterm term (m value) -> m value
  -- | Evaluate an application (like a function call).
  apply :: value -> [m value] -> m value

  -- | Primitive looping combinator, approximately equivalent to 'fix'. This should be used in place of direct recursion, as it allows abstraction over recursion.
  --
  --   The function argument takes an action which recurs through the loop.
  loop :: (m value -> m value) -> m value


-- | Attempt to extract a 'Prelude.Bool' from a given value.
toBool :: MonadValue value m => value -> m Bool
toBool v = ifthenelse v (pure True) (pure False)

forLoop :: (MonadEnvironment value m, MonadValue value m)
        => m value -- ^ Initial statement
        -> m value -- ^ Condition
        -> m value -- ^ Increment/stepper
        -> m value -- ^ Body
        -> m value
forLoop initial cond step body =
  localize (initial *> while cond (body *> step))

-- | The fundamental looping primitive, built on top of ifthenelse.
while :: MonadValue value m
      => m value
      -> m value
      -> m value
while cond body = loop $ \ continue -> do
  this <- cond
  ifthenelse this (body *> continue) unit

-- | Do-while loop, built on top of while.
doWhile :: MonadValue value m
        => m value
        -> m value
        -> m value
doWhile body cond = loop $ \ continue -> body *> do
  this <- cond
  ifthenelse this continue unit

makeNamespace :: ( MonadValue value m
                 , MonadEnvironment value m
                 , MonadHeap value m
                 , Reducer value (CellFor value)
                 , Ord (LocationFor value)
                 )
              => Name
              -> Address (LocationFor value) value
              -> m value
makeNamespace name addr = do
  namespaceEnv <- Env.head <$> getEnv
  v <- namespace name namespaceEnv
  v <$ assign addr v


-- | Value types, e.g. closures, which can root a set of addresses.
class ValueRoots value where
  -- | Compute the set of addresses rooted by a given value.
  valueRoots :: value -> LiveFor value


-- The type of exceptions that can be thrown when constructing values in `MonadValue`.
data ValueExc value resume where
  ValueExc :: Prelude.String -> ValueExc value value
  StringExc :: Prelude.String -> ValueExc value ByteString

instance Eq1 (ValueExc value) where
  liftEq _ (ValueExc a)  (ValueExc b)  = a == b
  liftEq _ (StringExc a) (StringExc b) = a == b
  liftEq _ _             _             = False

deriving instance Show (ValueExc value resume)
instance Show1 (ValueExc value) where
  liftShowsPrec _ _ = showsPrec
