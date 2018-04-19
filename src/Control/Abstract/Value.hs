{-# LANGUAGE FunctionalDependencies, GADTs, Rank2Types, TypeFamilies, TypeOperators, UndecidableInstances, ScopedTypeVariables #-}
module Control.Abstract.Value
( MonadValue(..)
, Comparator(..)
, while
, doWhile
, forLoop
, makeNamespace
, ValueRoots(..)
, ValueError(..)
) where

import Control.Abstract.Evaluator
import Data.Abstract.Address (Address, Cell)
import Data.Abstract.Environment as Env
import Data.Abstract.FreeVariables
import Data.Abstract.Live (Live)
import Data.Abstract.Number as Number
import Data.Scientific (Scientific)
import Data.Semigroup.Reducer hiding (unit)
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

-- | A 'Monad' abstracting the evaluation of (and under) binding constructs (functions, methods, etc).
--
--   This allows us to abstract the choice of whether to evaluate under binders for different value types.
class (Monad m, Show value) => MonadValue location value m | m value -> location where
  -- | Construct an abstract unit value.
  --   TODO: This might be the same as the empty tuple for some value types
  unit :: m value

  -- | Construct an abstract hole.
  hole :: m value

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
  ifthenelse :: value -> m value -> m value -> m value

  -- | Extract a 'Bool' from a given value.
  asBool :: value -> m Bool

  -- | Construct the nil/null datatype.
  null :: m value

  isHole :: value -> m Bool

  -- | Build a class value from a name and environment.
  klass :: Name                       -- ^ The new class's identifier
        -> [value]                    -- ^ A list of superclasses
        -> Environment location value -- ^ The environment to capture
        -> m value

  -- | Build a namespace value from a name and environment stack
  --
  -- Namespaces model closures with monoidal environments.
  namespace :: Name                       -- ^ The namespace's identifier
            -> Environment location value -- ^ The environment to mappend
            -> m value

  -- | Extract the environment from any scoped object (e.g. classes, namespaces, etc).
  scopedEnvironment :: value -> m (Maybe (Environment location value))

  -- | Evaluate an abstraction (a binder like a lambda or method definition).
  lambda :: (FreeVariables term, MonadControl term m) => [Name] -> Subterm term (m value) -> m value
  -- | Evaluate an application (like a function call).
  call :: value -> [m value] -> m value

  -- | Primitive looping combinator, approximately equivalent to 'fix'. This should be used in place of direct recursion, as it allows abstraction over recursion.
  --
  --   The function argument takes an action which recurs through the loop.
  loop :: (m value -> m value) -> m value


-- | Attempt to extract a 'Prelude.Bool' from a given value.
forLoop :: (MonadEnvironment location value m, MonadValue location value m)
        => m value -- ^ Initial statement
        -> m value -- ^ Condition
        -> m value -- ^ Increment/stepper
        -> m value -- ^ Body
        -> m value
forLoop initial cond step body =
  localize (initial *> while cond (body *> step))

-- | The fundamental looping primitive, built on top of ifthenelse.
while :: MonadValue location value m
      => m value
      -> m value
      -> m value
while cond body = loop $ \ continue -> do
  this <- cond
  ifthenelse this (body *> continue) unit

-- | Do-while loop, built on top of while.
doWhile :: MonadValue location value m
        => m value
        -> m value
        -> m value
doWhile body cond = loop $ \ continue -> body *> do
  this <- cond
  ifthenelse this continue unit

makeNamespace :: forall value location m. ( MonadValue location value m
                 , MonadThrow (ValueError location value) m
                 , MonadEnvironment location value m
                 , MonadHeap location value m
                 , Ord location
                 , Reducer value (Cell location value)
                 )
              => Name
              -> Address location value
              -> [value]
              -> m value
makeNamespace name addr supers = do
  superEnv <- mconcat <$> traverse scopedEnvironment supers
  case superEnv of
    Just superEnv' -> do
      namespaceEnv <- Env.head <$> getEnv
      v <- namespace name (Env.mergeNewer superEnv' namespaceEnv)
      v <$ assign addr v
    Nothing -> throwException @(ValueError location value) $ NamespaceError (show name)


-- | Value types, e.g. closures, which can root a set of addresses.
class ValueRoots location value where
  -- | Compute the set of addresses rooted by a given value.
  valueRoots :: value -> Live location value


-- The type of exceptions that can be thrown when constructing values in `MonadValue`.
data ValueError location value resume where
  StringError            :: value          -> ValueError location value ByteString
  BoolError              :: value          -> ValueError location value Bool
  NamespaceError         :: Prelude.String -> ValueError location value value
  ScopedEnvironmentError :: [value]          -> ValueError location value value
  CallError              :: value          -> ValueError location value value
  NumericError           :: value          -> ValueError location value value
  Numeric2Error          :: value -> value -> ValueError location value value
  ComparisonError        :: value -> value -> ValueError location value value
  BitwiseError           :: value          -> ValueError location value value
  Bitwise2Error          :: value -> value -> ValueError location value value
  KeyValueError          :: value          -> ValueError location value (value, value)

instance Eq value => Eq1 (ValueError location value) where
  liftEq _ (StringError a) (StringError b)                       = a == b
  liftEq _ (NamespaceError a) (NamespaceError b)                 = a == b
  liftEq _ (ScopedEnvironmentError a) (ScopedEnvironmentError b) = a == b
  liftEq _ (CallError a) (CallError b)                           = a == b
  liftEq _ (BoolError a) (BoolError c)                           = a == c
  liftEq _ (Numeric2Error a b) (Numeric2Error c d)               = (a == c) && (b == d)
  liftEq _ (ComparisonError a b) (ComparisonError c d)           = (a == c) && (b == d)
  liftEq _ (Bitwise2Error a b) (Bitwise2Error c d)               = (a == c) && (b == d)
  liftEq _ (BitwiseError a) (BitwiseError b)                     = a == b
  liftEq _ (KeyValueError a) (KeyValueError b)                   = a == b
  liftEq _ _             _                                       = False

deriving instance (Show value) => Show (ValueError location value resume)
instance (Show value) => Show1 (ValueError location value) where
  liftShowsPrec _ _ = showsPrec
