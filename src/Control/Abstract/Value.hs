{-# LANGUAGE FunctionalDependencies, GADTs, KindSignatures, Rank2Types #-}
module Control.Abstract.Value
( MonadValue(..)
, AbstractHole(..)
, Comparator(..)
, while
, doWhile
, forLoop
, makeNamespace
, ValueRoots(..)
) where

import Control.Abstract.Evaluator
import Data.Abstract.Address (Address, Cell)
import Data.Abstract.Environment as Env
import Data.Abstract.FreeVariables
import Data.Abstract.Live (Live)
import Data.Abstract.Number as Number
import Data.Scientific (Scientific)
import Data.Semigroup.Reducer hiding (unit)
import Data.Semilattice.Lower
import Prelude
import Prologue hiding (TypeError)

-- | This datum is passed into liftComparison to handle the fact that Ruby and PHP
--   have built-in generalized-comparison ("spaceship") operators. If you want to
--   encapsulate a traditional, boolean-returning operator, wrap it in 'Concrete';
--   if you want the generalized comparator, pass in 'Generalized'. In MonadValue
--   instances, you can then then handle the different cases to return different
--   types, if that's what you need.
data Comparator
  = Concrete (forall a . Ord a => a -> a -> Bool)
  | Generalized

class AbstractHole value where
  hole :: value

-- | A 'Monad' abstracting the evaluation of (and under) binding constructs (functions, methods, etc).
--
--   This allows us to abstract the choice of whether to evaluate under binders for different value types.
class (Monad (m effects), Show value) => MonadValue location value (effects :: [* -> *]) m | m effects value -> location where
  -- | Construct an abstract unit value.
  --   TODO: This might be the same as the empty tuple for some value types
  unit :: m effects value

  -- | Construct an abstract integral value.
  integer :: Prelude.Integer -> m effects value

  -- | Lift a unary operator over a 'Num' to a function on 'value's.
  liftNumeric  :: (forall a . Num a => a -> a)
               -> (value -> m effects value)

  -- | Lift a pair of binary operators to a function on 'value's.
  --   You usually pass the same operator as both arguments, except in the cases where
  --   Haskell provides different functions for integral and fractional operations, such
  --   as division, exponentiation, and modulus.
  liftNumeric2 :: (forall a b. Number a -> Number b -> SomeNumber)
               -> (value -> value -> m effects value)

  -- | Lift a Comparator (usually wrapping a function like == or <=) to a function on values.
  liftComparison :: Comparator -> (value -> value -> m effects value)

  -- | Lift a unary bitwise operator to values. This is usually 'complement'.
  liftBitwise :: (forall a . Bits a => a -> a)
              -> (value -> m effects value)

  -- | Lift a binary bitwise operator to values. The Integral constraint is
  --   necessary to satisfy implementation details of Haskell left/right shift,
  --   but it's fine, since these are only ever operating on integral values.
  liftBitwise2 :: (forall a . (Integral a, Bits a) => a -> a -> a)
               -> (value -> value -> m effects value)

  -- | Construct an abstract boolean value.
  boolean :: Bool -> m effects value

  -- | Construct an abstract string value.
  string :: ByteString -> m effects value

  -- | Construct a self-evaluating symbol value.
  --   TODO: Should these be interned in some table to provide stronger uniqueness guarantees?
  symbol :: ByteString -> m effects value

  -- | Construct a floating-point value.
  float :: Scientific -> m effects value

  -- | Construct a rational value.
  rational :: Prelude.Rational -> m effects value

  -- | Construct an N-ary tuple of multiple (possibly-disjoint) values
  multiple :: [value] -> m effects value

  -- | Construct an array of zero or more values.
  array :: [value] -> m effects value

  -- | Construct a key-value pair for use in a hash.
  kvPair :: value -> value -> m effects value

  -- | Extract the contents of a key-value pair as a tuple.
  asPair :: value -> m effects (value, value)

  -- | Construct a hash out of pairs.
  hash :: [(value, value)] -> m effects value

  -- | Extract a 'ByteString' from a given value.
  asString :: value -> m effects ByteString

  -- | Eliminate boolean values. TODO: s/boolean/truthy
  ifthenelse :: value -> m effects value -> m effects value -> m effects value

  -- | Extract a 'Bool' from a given value.
  asBool :: value -> m effects Bool

  -- | Construct the nil/null datatype.
  null :: m effects value

  -- | @index x i@ computes @x[i]@, with zero-indexing.
  index :: value -> value -> m effects value

  -- | Determine whether the given datum is a 'Hole'.
  isHole :: value -> m effects Bool

  -- | Build a class value from a name and environment.
  klass :: Name                       -- ^ The new class's identifier
        -> [value]                    -- ^ A list of superclasses
        -> Environment location value -- ^ The environment to capture
        -> m effects value

  -- | Build a namespace value from a name and environment stack
  --
  -- Namespaces model closures with monoidal environments.
  namespace :: Name                       -- ^ The namespace's identifier
            -> Environment location value -- ^ The environment to mappend
            -> m effects value

  -- | Extract the environment from any scoped object (e.g. classes, namespaces, etc).
  scopedEnvironment :: value -> m effects (Environment location value)

  -- | Evaluate an abstraction (a binder like a lambda or method definition).
  lambda :: (FreeVariables term, MonadEvaluator location term value effects m) => [Name] -> Subterm term (m effects value) -> m effects value
  -- | Evaluate an application (like a function call).
  call :: value -> [m effects value] -> m effects value

  -- | Primitive looping combinator, approximately equivalent to 'fix'. This should be used in place of direct recursion, as it allows abstraction over recursion.
  --
  --   The function argument takes an action which recurs through the loop.
  loop :: (m effects value -> m effects value) -> m effects value


-- | Attempt to extract a 'Prelude.Bool' from a given value.
forLoop :: (MonadEvaluator location term value effects m, MonadValue location value effects m)
        => m effects value -- ^ Initial statement
        -> m effects value -- ^ Condition
        -> m effects value -- ^ Increment/stepper
        -> m effects value -- ^ Body
        -> m effects value
forLoop initial cond step body =
  localize (initial *> while cond (body *> step))

-- | The fundamental looping primitive, built on top of ifthenelse.
while :: MonadValue location value effects m
      => m effects value
      -> m effects value
      -> m effects value
while cond body = loop $ \ continue -> do
  this <- cond
  ifthenelse this (body *> continue) unit

-- | Do-while loop, built on top of while.
doWhile :: MonadValue location value effects m
        => m effects value
        -> m effects value
        -> m effects value
doWhile body cond = loop $ \ continue -> body *> do
  this <- cond
  ifthenelse this continue unit

makeNamespace :: ( MonadValue location value effects m
                 , MonadEvaluator location term value effects m
                 , Ord location
                 , Reducer value (Cell location value)
                 )
              => Name
              -> Address location value
              -> Maybe value
              -> m effects value
makeNamespace name addr super = do
  superEnv <- maybe (pure lowerBound) scopedEnvironment super
  namespaceEnv <- Env.head <$> getEnv
  v <- namespace name (Env.mergeNewer superEnv namespaceEnv)
  v <$ assign addr v


-- | Value types, e.g. closures, which can root a set of addresses.
class ValueRoots location value where
  -- | Compute the set of addresses rooted by a given value.
  valueRoots :: value -> Live location value
