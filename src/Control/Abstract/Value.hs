{-# LANGUAGE GADTs, Rank2Types #-}
module Control.Abstract.Value
( AbstractValue(..)
, AbstractIntro(..)
, AbstractFunction(..)
, Comparator(..)
, asBool
, while
, doWhile
, forLoop
, makeNamespace
, evaluateInScopedEnv
, value
, subtermValue
, ValueRoots(..)
) where

import Control.Abstract.Addressable
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Data.Abstract.Address (Address(..))
import Data.Abstract.Environment as Env
import Data.Abstract.Live (Live)
import Data.Abstract.Name
import Data.Abstract.Number as Number
import Data.Abstract.Ref
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

class Show value => AbstractFunction location value effects where
  -- | Build a closure (a binder like a lambda or method definition).
  closure :: [Name]                                 -- ^ The parameter names.
          -> Set Name                               -- ^ The set of free variables to close over.
          -> Evaluator location value effects value -- ^ The evaluator for the body of the closure.
          -> Evaluator location value effects value
  -- | Evaluate an application (like a function call).
  call :: value -> [Evaluator location value effects value] -> Evaluator location value effects value


class Show value => AbstractIntro value where
  -- | Construct an abstract unit value.
  --   TODO: This might be the same as the empty tuple for some value types
  unit :: value

  -- | Construct an abstract boolean value.
  boolean :: Bool -> value

  -- | Construct an abstract string value.
  string :: ByteString -> value

  -- | Construct a self-evaluating symbol value.
  --   TODO: Should these be interned in some table to provide stronger uniqueness guarantees?
  symbol :: ByteString -> value

  -- | Construct an abstract integral value.
  integer :: Integer -> value

  -- | Construct a floating-point value.
  float :: Scientific -> value

  -- | Construct a rational value.
  rational :: Rational -> value

  -- | Construct an N-ary tuple of multiple (possibly-disjoint) values
  multiple :: [value] -> value

  -- | Construct a key-value pair for use in a hash.
  kvPair :: value -> value -> value

  -- | Construct a hash out of pairs.
  hash :: [(value, value)] -> value

  -- | Construct the nil/null datatype.
  null :: value


-- | A 'Monad' abstracting the evaluation of (and under) binding constructs (functions, methods, etc).
--
--   This allows us to abstract the choice of whether to evaluate under binders for different value types.
class (AbstractFunction location value effects, AbstractIntro value) => AbstractValue location value effects where
  -- | Lift a unary operator over a 'Num' to a function on 'value's.
  liftNumeric  :: (forall a . Num a => a -> a)
               -> (value -> Evaluator location value effects value)

  -- | Lift a pair of binary operators to a function on 'value's.
  --   You usually pass the same operator as both arguments, except in the cases where
  --   Haskell provides different functions for integral and fractional operations, such
  --   as division, exponentiation, and modulus.
  liftNumeric2 :: (forall a b. Number a -> Number b -> SomeNumber)
               -> (value -> value -> Evaluator location value effects value)

  -- | Lift a Comparator (usually wrapping a function like == or <=) to a function on values.
  liftComparison :: Comparator -> (value -> value -> Evaluator location value effects value)

  -- | Lift a unary bitwise operator to values. This is usually 'complement'.
  liftBitwise :: (forall a . Bits a => a -> a)
              -> (value -> Evaluator location value effects value)

  -- | Lift a binary bitwise operator to values. The Integral constraint is
  --   necessary to satisfy implementation details of Haskell left/right shift,
  --   but it's fine, since these are only ever operating on integral values.
  liftBitwise2 :: (forall a . (Integral a, Bits a) => a -> a -> a)
               -> (value -> value -> Evaluator location value effects value)

  -- | Construct an array of zero or more values.
  array :: [value] -> Evaluator location value effects value

  -- | Extract the contents of a key-value pair as a tuple.
  asPair :: value -> Evaluator location value effects (value, value)

  -- | Extract a 'ByteString' from a given value.
  asString :: value -> Evaluator location value effects ByteString

  -- | Eliminate boolean values. TODO: s/boolean/truthy
  ifthenelse :: value -> Evaluator location value effects a -> Evaluator location value effects a -> Evaluator location value effects a

  -- | @index x i@ computes @x[i]@, with zero-indexing.
  index :: value -> value -> Evaluator location value effects value

  -- | Build a class value from a name and environment.
  klass :: Name                 -- ^ The new class's identifier
        -> [value]              -- ^ A list of superclasses
        -> Environment location -- ^ The environment to capture
        -> Evaluator location value effects value

  -- | Build a namespace value from a name and environment stack
  --
  -- Namespaces model closures with monoidal environments.
  namespace :: Name                 -- ^ The namespace's identifier
            -> Environment location -- ^ The environment to mappend
            -> Evaluator location value effects value

  -- | Extract the environment from any scoped object (e.g. classes, namespaces, etc).
  scopedEnvironment :: value -> Evaluator location value effects (Maybe (Environment location))

  -- | Primitive looping combinator, approximately equivalent to 'fix'. This should be used in place of direct recursion, as it allows abstraction over recursion.
  --
  --   The function argument takes an action which recurs through the loop.
  loop :: (Evaluator location value effects value -> Evaluator location value effects value) -> Evaluator location value effects value


-- | Extract a 'Bool' from a given value.
asBool :: AbstractValue location value effects => value -> Evaluator location value effects Bool
asBool value = ifthenelse value (pure True) (pure False)

-- | C-style for loops.
forLoop :: ( AbstractValue location value effects
           , Member (State (Environment location)) effects
           )
        => Evaluator location value effects value -- ^ Initial statement
        -> Evaluator location value effects value -- ^ Condition
        -> Evaluator location value effects value -- ^ Increment/stepper
        -> Evaluator location value effects value -- ^ Body
        -> Evaluator location value effects value
forLoop initial cond step body =
  localEnv id (initial *> while cond (body *> step))

-- | The fundamental looping primitive, built on top of 'ifthenelse'.
while :: AbstractValue location value effects
      => Evaluator location value effects value
      -> Evaluator location value effects value
      -> Evaluator location value effects value
while cond body = loop $ \ continue -> do
  this <- cond
  ifthenelse this (body *> continue) (pure unit)

-- | Do-while loop, built on top of while.
doWhile :: AbstractValue location value effects
        => Evaluator location value effects value
        -> Evaluator location value effects value
        -> Evaluator location value effects value
doWhile body cond = loop $ \ continue -> body *> do
  this <- cond
  ifthenelse this continue (pure unit)

makeNamespace :: ( AbstractValue location value effects
                 , Member (State (Environment location)) effects
                 , Member (State (Heap location (Cell location) value)) effects
                 , Ord location
                 , Reducer value (Cell location value)
                 )
              => Name
              -> Address location value
              -> Maybe value
              -> Evaluator location value effects value
makeNamespace name addr super = do
  superEnv <- maybe (pure (Just lowerBound)) scopedEnvironment super
  let env' = fromMaybe lowerBound superEnv
  namespaceEnv <- Env.head <$> getEnv
  v <- namespace name (Env.mergeNewer env' namespaceEnv)
  v <$ assign addr v


-- | Evaluate a term within the context of the scoped environment of 'scopedEnvTerm'.
evaluateInScopedEnv :: ( AbstractValue location value effects
                       , Member (Env location) effects
                       )
                    => Evaluator location value effects value
                    -> Evaluator location value effects value
                    -> Evaluator location value effects value
evaluateInScopedEnv scopedEnvTerm term = do
  scopedEnv <- scopedEnvTerm >>= scopedEnvironment
  maybe term (\ env -> locally $ bindAll env >> term) scopedEnv


-- | Evaluates a 'Value' returning the referenced value
value :: ( AbstractValue location value effects
         , Member (Allocator location value) effects
         , Member (Env location) effects
         , Member (Resumable (EnvironmentError location)) effects
         )
      => ValueRef value
      -> Evaluator location value effects value
value (LvalLocal var) = variable var
value (LvalMember obj prop) = evaluateInScopedEnv (pure obj) (variable prop)
value (Rval val) = pure val

-- | Evaluates a 'Subterm' to its rval
subtermValue :: ( AbstractValue location value effects
                , Member (Allocator location value) effects
                , Member (Env location) effects
                , Member (Resumable (EnvironmentError location)) effects
                )
             => Subterm term (Evaluator location value effects (ValueRef value))
             -> Evaluator location value effects value
subtermValue = value <=< subtermRef


-- | Value types, e.g. closures, which can root a set of addresses.
class ValueRoots location value where
  -- | Compute the set of addresses rooted by a given value.
  valueRoots :: value -> Live location value
