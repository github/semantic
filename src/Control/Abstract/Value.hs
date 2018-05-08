{-# LANGUAGE GADTs, Rank2Types #-}
module Control.Abstract.Value
( AbstractValue(..)
, AbstractHole(..)
, Comparator(..)
, while
, doWhile
, forLoop
, makeNamespace
, ValueRoots(..)
) where

import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
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
--   if you want the generalized comparator, pass in 'Generalized'. In 'AbstractValue'
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
class Show value => AbstractValue location value effects where
  -- | Construct an abstract unit value.
  --   TODO: This might be the same as the empty tuple for some value types
  unit :: Evaluator location term value effects value

  -- | Construct an abstract integral value.
  integer :: Prelude.Integer -> Evaluator location term value effects value

  -- | Lift a unary operator over a 'Num' to a function on 'value's.
  liftNumeric  :: (forall a . Num a => a -> a)
               -> (value -> Evaluator location term value effects value)

  -- | Lift a pair of binary operators to a function on 'value's.
  --   You usually pass the same operator as both arguments, except in the cases where
  --   Haskell provides different functions for integral and fractional operations, such
  --   as division, exponentiation, and modulus.
  liftNumeric2 :: (forall a b. Number a -> Number b -> SomeNumber)
               -> (value -> value -> Evaluator location term value effects value)

  -- | Lift a Comparator (usually wrapping a function like == or <=) to a function on values.
  liftComparison :: Comparator -> (value -> value -> Evaluator location term value effects value)

  -- | Lift a unary bitwise operator to values. This is usually 'complement'.
  liftBitwise :: (forall a . Bits a => a -> a)
              -> (value -> Evaluator location term value effects value)

  -- | Lift a binary bitwise operator to values. The Integral constraint is
  --   necessary to satisfy implementation details of Haskell left/right shift,
  --   but it's fine, since these are only ever operating on integral values.
  liftBitwise2 :: (forall a . (Integral a, Bits a) => a -> a -> a)
               -> (value -> value -> Evaluator location term value effects value)

  -- | Construct an abstract boolean value.
  boolean :: Bool -> Evaluator location term value effects value

  -- | Construct an abstract string value.
  string :: ByteString -> Evaluator location term value effects value

  -- | Construct a self-evaluating symbol value.
  --   TODO: Should these be interned in some table to provide stronger uniqueness guarantees?
  symbol :: ByteString -> Evaluator location term value effects value

  -- | Construct a floating-point value.
  float :: Scientific -> Evaluator location term value effects value

  -- | Construct a rational value.
  rational :: Prelude.Rational -> Evaluator location term value effects value

  -- | Construct an N-ary tuple of multiple (possibly-disjoint) values
  multiple :: [value] -> Evaluator location term value effects value

  -- | Construct an array of zero or more values.
  array :: [value] -> Evaluator location term value effects value

  -- | Construct a key-value pair for use in a hash.
  kvPair :: value -> value -> Evaluator location term value effects value

  -- | Extract the contents of a key-value pair as a tuple.
  asPair :: value -> Evaluator location term value effects (value, value)

  -- | Construct a hash out of pairs.
  hash :: [(value, value)] -> Evaluator location term value effects value

  -- | Extract a 'ByteString' from a given value.
  asString :: value -> Evaluator location term value effects ByteString

  -- | Eliminate boolean values. TODO: s/boolean/truthy
  ifthenelse :: value -> Evaluator location term value effects value -> Evaluator location term value effects value -> Evaluator location term value effects value

  -- | Extract a 'Bool' from a given value.
  asBool :: value -> Evaluator location term value effects Bool

  -- | Construct the nil/null datatype.
  null :: Evaluator location term value effects value

  -- | @index x i@ computes @x[i]@, with zero-indexing.
  index :: value -> value -> Evaluator location term value effects value

  -- | Determine whether the given datum is a 'Hole'.
  isHole :: value -> Evaluator location term value effects Bool

  -- | Build a class value from a name and environment.
  klass :: Name                       -- ^ The new class's identifier
        -> [value]                    -- ^ A list of superclasses
        -> Environment location value -- ^ The environment to capture
        -> Evaluator location term value effects value

  -- | Build a namespace value from a name and environment stack
  --
  -- Namespaces model closures with monoidal environments.
  namespace :: Name                       -- ^ The namespace's identifier
            -> Environment location value -- ^ The environment to mappend
            -> Evaluator location term value effects value

  -- | Extract the environment from any scoped object (e.g. classes, namespaces, etc).
  scopedEnvironment :: value -> Evaluator location term value effects (Maybe (Environment location value))

  -- | Evaluate an abstraction (a binder like a lambda or method definition).
  lambda :: FreeVariables term => [Name] -> Subterm term (Evaluator location term value effects value) -> Evaluator location term value effects value
  -- | Evaluate an application (like a function call).
  call :: value -> [Evaluator location term value effects value] -> Evaluator location term value effects value

  -- | Primitive looping combinator, approximately equivalent to 'fix'. This should be used in place of direct recursion, as it allows abstraction over recursion.
  --
  --   The function argument takes an action which recurs through the loop.
  loop :: (Evaluator location term value effects value -> Evaluator location term value effects value) -> Evaluator location term value effects value


-- | Attempt to extract a 'Prelude.Bool' from a given value.
forLoop :: ( AbstractValue location value effects
           , Member (State (Environment location value)) effects
           )
        => Evaluator location term value effects value -- ^ Initial statement
        -> Evaluator location term value effects value -- ^ Condition
        -> Evaluator location term value effects value -- ^ Increment/stepper
        -> Evaluator location term value effects value -- ^ Body
        -> Evaluator location term value effects value
forLoop initial cond step body =
  localize (initial *> while cond (body *> step))

-- | The fundamental looping primitive, built on top of ifthenelse.
while :: AbstractValue location value effects
      => Evaluator location term value effects value
      -> Evaluator location term value effects value
      -> Evaluator location term value effects value
while cond body = loop $ \ continue -> do
  this <- cond
  ifthenelse this (body *> continue) unit

-- | Do-while loop, built on top of while.
doWhile :: AbstractValue location value effects
        => Evaluator location term value effects value
        -> Evaluator location term value effects value
        -> Evaluator location term value effects value
doWhile body cond = loop $ \ continue -> body *> do
  this <- cond
  ifthenelse this continue unit

makeNamespace :: ( AbstractValue location value effects
                 , Member (State (Environment location value)) effects
                 , Member (State (Heap location value)) effects
                 , Ord location
                 , Reducer value (Cell location value)
                 )
              => Name
              -> Address location value
              -> Maybe value
              -> Evaluator location term value effects value
makeNamespace name addr super = do
  superEnv <- maybe (pure (Just lowerBound)) scopedEnvironment super
  let env' = fromMaybe lowerBound superEnv
  namespaceEnv <- Env.head <$> getEnv
  v <- namespace name (Env.mergeNewer env' namespaceEnv)
  v <$ assign addr v


-- | Value types, e.g. closures, which can root a set of addresses.
class ValueRoots location value where
  -- | Compute the set of addresses rooted by a given value.
  valueRoots :: value -> Live location value
