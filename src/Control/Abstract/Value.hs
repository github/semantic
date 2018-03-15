{-# LANGUAGE MultiParamTypeClasses, Rank2Types, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Value where

import Control.Abstract.Addressable
import Control.Abstract.Analysis
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Number as Number
import Data.Abstract.Type as Type
import Data.Abstract.Value as Value
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import Prelude hiding (fail)
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
class (MonadAnalysis term value m, Show value) => MonadValue term value m where
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

  -- | Eliminate boolean values. TODO: s/boolean/truthy
  ifthenelse :: value -> m a -> m a -> m a

  -- | Evaluate an abstraction (a binder like a lambda or method definition).
  abstract :: [Name] -> Subterm term (m value) -> m value
  -- | Evaluate an application (like a function call).
  apply :: value -> [m value] -> m value

  -- | Primitive looping combinator, approximately equivalent to 'fix'. This should be used in place of direct recursion, as it allows abstraction over recursion.
  --
  --   The function argument takes an action which recurs through the loop.
  loop :: (m value -> m value) -> m value

-- | Attempt to extract a 'Prelude.Bool' from a given value.
toBool :: MonadValue term value m => value -> m Bool
toBool v = ifthenelse v (pure True) (pure False)

forLoop :: MonadValue term value m
        => m value -- | Initial statement
        -> m value -- | Condition
        -> m value -- | Increment/stepper
        -> m value -- | Body
        -> m value
forLoop initial cond step body = do
  void initial
  env <- getGlobalEnv
  localEnv (mappend env) (while cond (body *> step))

-- | The fundamental looping primitive, built on top of ifthenelse.
while :: MonadValue term value m
      => m value
      -> m value
      -> m value
while cond body = loop $ \ continue -> do
  this <- cond
  ifthenelse this (body *> continue) unit

-- | Do-while loop, built on top of while.
doWhile :: MonadValue term value m
        => m value
        -> m value
        -> m value
doWhile body cond = loop $ \ continue -> body *> do
  this <- cond
  ifthenelse this continue unit

-- | Construct a 'Value' wrapping the value arguments (if any).
instance ( FreeVariables term
         , MonadAddressable location (Value location term) m
         , MonadAnalysis term (Value location term) m
         , Show location
         , Show term
         )
         => MonadValue term (Value location term) m where

  unit     = pure . injValue $ Value.Unit
  integer  = pure . injValue . Value.Integer . Number.Integer
  boolean  = pure . injValue . Boolean
  string   = pure . injValue . Value.String
  float    = pure . injValue . Value.Float . Decimal
  symbol   = pure . injValue . Value.Symbol
  rational = pure . injValue . Value.Rational . Ratio

  multiple = pure . injValue . Value.Tuple

  array    = pure . injValue . Value.Array

  ifthenelse cond if' else'
    | Just (Boolean b) <- prjValue cond = if b then if' else else'
    | otherwise = fail ("not defined for non-boolean conditions: " <> show cond)

  liftNumeric f arg
    | Just (Value.Integer (Number.Integer i)) <- prjValue arg = integer $ f i
    | Just (Value.Float (Decimal d))          <- prjValue arg = float   $ f d
    | Just (Value.Rational (Ratio r))         <- prjValue arg = rational $ f r
    | otherwise = fail ("Invalid operand to liftNumeric: " <> show arg)

  liftNumeric2 f left right
    | Just (Value.Integer i, Value.Integer j)   <- prjPair pair = f i j & specialize
    | Just (Value.Integer i, Value.Rational j)  <- prjPair pair = f i j & specialize
    | Just (Value.Integer i, Value.Float j)     <- prjPair pair = f i j & specialize
    | Just (Value.Rational i, Value.Integer j)  <- prjPair pair = f i j & specialize
    | Just (Value.Rational i, Value.Rational j) <- prjPair pair = f i j & specialize
    | Just (Value.Rational i, Value.Float j)    <- prjPair pair = f i j & specialize
    | Just (Value.Float i, Value.Integer j)     <- prjPair pair = f i j & specialize
    | Just (Value.Float i, Value.Rational j)    <- prjPair pair = f i j & specialize
    | Just (Value.Float i, Value.Float j)       <- prjPair pair = f i j & specialize
    | otherwise = fail ("Invalid operands to liftNumeric2: " <> show pair)
      where
        -- Dispatch whatever's contained inside a 'SomeNumber' to its appropriate 'MonadValue' ctor
        specialize :: MonadValue term value m => SomeNumber -> m value
        specialize (SomeNumber (Number.Integer i)) = integer i
        specialize (SomeNumber (Ratio r))          = rational r
        specialize (SomeNumber (Decimal d))        = float d
        pair = (left, right)

  liftComparison comparator left right
    | Just (Value.Integer (Number.Integer i), Value.Integer (Number.Integer j)) <- prjPair pair = go i j
    | Just (Value.Integer (Number.Integer i), Value.Float (Decimal j))          <- prjPair pair = go (fromIntegral i) j
    | Just (Value.Float (Decimal i), Value.Integer (Number.Integer j))          <- prjPair pair = go i (fromIntegral j)
    | Just (Value.Float (Decimal i), Value.Float (Decimal j))                   <- prjPair pair = go i j
    | Just (Value.String i, Value.String j)                                     <- prjPair pair = go i j
    | Just (Boolean i, Boolean j)                                               <- prjPair pair = go i j
    | Just (Value.Unit, Value.Unit)                                             <- prjPair pair = boolean True
    | otherwise = fail ("Type error: invalid arguments to liftComparison: " <> show pair)
      where
        -- Explicit type signature is necessary here because we're passing all sorts of things
        -- to these comparison functions.
        go :: (Ord a, MonadValue term value m) => a -> a -> m value
        go l r = case comparator of
          Concrete f  -> boolean (f l r)
          Generalized -> integer (orderingToInt (compare l r))

        -- Map from [LT, EQ, GT] to [-1, 0, 1]
        orderingToInt :: Ordering -> Prelude.Integer
        orderingToInt = toInteger . pred . fromEnum

        pair = (left, right)

  liftBitwise operator target
    | Just (Value.Integer (Number.Integer i)) <- prjValue target = integer $ operator i
    | otherwise = fail ("Type error: invalid unary bitwise operation on " <> show target)

  liftBitwise2 operator left right
    | Just (Value.Integer (Number.Integer i), Value.Integer (Number.Integer j)) <- prjPair pair = integer $ operator i j
    | otherwise = fail ("Type error: invalid binary bitwise operation on " <> show pair)
      where pair = (left, right)

  abstract names (Subterm body _) = injValue . Closure names body . bindEnv (foldr Set.delete (freeVariables body) names) <$> askLocalEnv

  apply op params = do
    Closure names body env <- maybe (fail ("expected a closure, got: " <> show op)) pure (prjValue op)
    bindings <- foldr (\ (name, param) rest -> do
      v <- param
      a <- alloc name
      assign a v
      envInsert name a <$> rest) (pure env) (zip names params)
    localEnv (mappend bindings) (evaluateTerm body)

  loop = fix

-- | Discard the value arguments (if any), constructing a 'Type.Type' instead.
instance (Alternative m, MonadAnalysis term Type m, MonadFresh m) => MonadValue term Type m where
  abstract names (Subterm _ body) = do
    (env, tvars) <- foldr (\ name rest -> do
      a <- alloc name
      tvar <- Var <$> fresh
      assign a tvar
      (env, tvars) <- rest
      pure (envInsert name a env, tvar : tvars)) (pure mempty) names
    ret <- localEnv (mappend env) body
    pure (Product tvars :-> ret)

  unit       = pure Type.Unit
  integer _  = pure Int
  boolean _  = pure Bool
  string _   = pure Type.String
  float _    = pure Type.Float
  symbol _   = pure Type.Symbol
  rational _ = pure Type.Rational
  multiple   = pure . Type.Product
  array      = pure . Type.Array

  ifthenelse cond if' else' = unify cond Bool *> (if' <|> else')

  liftNumeric _ Type.Float = pure Type.Float
  liftNumeric _ Int        = pure Int
  liftNumeric _ _          = fail "Invalid type in unary numeric operation"

  liftNumeric2 _ left right = case (left, right) of
    (Type.Float, Int) -> pure Type.Float
    (Int, Type.Float) -> pure Type.Float
    _                 -> unify left right

  liftBitwise _ Int = pure Int
  liftBitwise _ t   = fail ("Invalid type passed to unary bitwise operation: " <> show t)

  liftBitwise2 _ Int Int = pure Int
  liftBitwise2 _ t1 t2   = fail ("Invalid types passed to binary bitwise operation: " <> show (t1, t2))

  liftComparison (Concrete _) left right = case (left, right) of
    (Type.Float, Int) ->                     pure Bool
    (Int, Type.Float) ->                     pure Bool
    _                 -> unify left right *> pure Bool
  liftComparison Generalized left right = case (left, right) of
    (Type.Float, Int) ->                     pure Int
    (Int, Type.Float) ->                     pure Int
    _                 -> unify left right *> pure Int

  apply op params = do
    tvar <- fresh
    paramTypes <- sequenceA params
    _ :-> ret <- op `unify` (Product paramTypes :-> Var tvar)
    pure ret

  loop f = f empty
