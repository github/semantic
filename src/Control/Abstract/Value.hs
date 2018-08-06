{-# LANGUAGE GADTs, Rank2Types #-}
module Control.Abstract.Value
( AbstractValue(..)
, AbstractIntro(..)
, Comparator(..)
, function
, call
, Function(..)
, asBool
, while
, doWhile
, forLoop
, makeNamespace
, evaluateInScopedEnv
, address
, value
, rvalBox
, subtermValue
, subtermAddress
) where

import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Data.Abstract.Environment as Env
import Data.Abstract.Name
import Data.Abstract.Number as Number
import Data.Abstract.Ref
import Data.Scientific (Scientific)
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

function :: Member (Function address value) effects => [Name] -> Set Name -> Evaluator address value effects address -> Evaluator address value effects value
function names fvs (Evaluator body) = send (Function names fvs body)

call :: Member (Function address value) effects => value -> [address] -> Evaluator address value effects address
call fn args = send (Call fn args)

data Function address value m result where
  Function :: [Name] -> Set Name -> m address -> Function address value m value
  Call     :: value -> [address]              -> Function address value m address

instance PureEffect (Function address value) where
  handle handler (Request (Function name fvs body) k) = Request (Function name fvs (handler body)) (handler . k)
  handle handler (Request (Call fn addrs)          k) = Request (Call fn addrs)                    (handler . k)


class Show value => AbstractIntro value where
  -- | Construct an abstract unit value.
  --   TODO: This might be the same as the empty tuple for some value types
  unit :: value

  -- | Construct an abstract boolean value.
  boolean :: Bool -> value

  -- | Construct an abstract string value.
  string :: Text -> value

  -- | Construct a self-evaluating symbol value.
  --   TODO: Should these be interned in some table to provide stronger uniqueness guarantees?
  symbol :: Text -> value

  -- | Construct an abstract regex value.
  regex :: Text -> value

  -- | Construct an abstract integral value.
  integer :: Integer -> value

  -- | Construct a floating-point value.
  float :: Scientific -> value

  -- | Construct a rational value.
  rational :: Rational -> value

  -- | Construct a key-value pair for use in a hash.
  kvPair :: value -> value -> value

  -- | Construct a hash out of pairs.
  hash :: [(value, value)] -> value

  -- | Construct the nil/null datatype.
  null :: value

-- | A 'Monad' abstracting the evaluation of (and under) binding constructs (functions, methods, etc).
--
--   This allows us to abstract the choice of whether to evaluate under binders for different value types.
class AbstractIntro value => AbstractValue address value effects where
  -- | Lift a unary operator over a 'Num' to a function on 'value's.
  liftNumeric  :: (forall a . Num a => a -> a)
               -> (value -> Evaluator address value effects value)

  -- | Lift a pair of binary operators to a function on 'value's.
  --   You usually pass the same operator as both arguments, except in the cases where
  --   Haskell provides different functions for integral and fractional operations, such
  --   as division, exponentiation, and modulus.
  liftNumeric2 :: (forall a b. Number a -> Number b -> SomeNumber)
               -> (value -> value -> Evaluator address value effects value)

  -- | Lift a Comparator (usually wrapping a function like == or <=) to a function on values.
  liftComparison :: Comparator -> (value -> value -> Evaluator address value effects value)

  -- | Lift a unary bitwise operator to values. This is usually 'complement'.
  liftBitwise :: (forall a . Bits a => a -> a)
              -> (value -> Evaluator address value effects value)

  -- | Lift a binary bitwise operator to values. The Integral constraint is
  --   necessary to satisfy implementation details of Haskell left/right shift,
  --   but it's fine, since these are only ever operating on integral values.
  liftBitwise2 :: (forall a . (Integral a, Bits a) => a -> a -> a)
               -> (value -> value -> Evaluator address value effects value)

  -- | Construct an N-ary tuple of multiple (possibly-disjoint) values
  tuple :: [address] -> Evaluator address value effects value

  -- | Construct an array of zero or more values.
  array :: [address] -> Evaluator address value effects value

  -- | Extract the contents of a key-value pair as a tuple.
  asPair :: value -> Evaluator address value effects (value, value)

  -- | Extract a 'Text' from a given value.
  asString :: value -> Evaluator address value effects Text

  -- | Eliminate boolean values. TODO: s/boolean/truthy
  ifthenelse :: value -> Evaluator address value effects a -> Evaluator address value effects a -> Evaluator address value effects a

  -- | Compute the disjunction (boolean or) of two computed values. This should have short-circuiting semantics where applicable.
  disjunction :: Evaluator address value effects value -> Evaluator address value effects value -> Evaluator address value effects value

  -- | @index x i@ computes @x[i]@, with zero-indexing.
  index :: value -> value -> Evaluator address value effects address

  -- | Build a class value from a name and environment.
  klass :: Name             -- ^ The new class's identifier
        -> [address]        -- ^ A list of superclasses
        -> Bindings address -- ^ The environment to capture
        -> Evaluator address value effects value

  -- | Build a namespace value from a name and environment stack
  --
  -- Namespaces model closures with monoidal environments.
  namespace :: Name                 -- ^ The namespace's identifier
            -> Maybe address        -- The ancestor of the namespace
            -> Bindings address     -- ^ The environment to mappend
            -> Evaluator address value effects value

  -- | Extract the environment from any scoped object (e.g. classes, namespaces, etc).
  scopedEnvironment :: address -> Evaluator address value effects (Maybe (Environment address))

  -- | Primitive looping combinator, approximately equivalent to 'fix'. This should be used in place of direct recursion, as it allows abstraction over recursion.
  --
  --   The function argument takes an action which recurs through the loop.
  loop :: (Evaluator address value effects value -> Evaluator address value effects value) -> Evaluator address value effects value


-- | Extract a 'Bool' from a given value.
asBool :: AbstractValue address value effects => value -> Evaluator address value effects Bool
asBool value = ifthenelse value (pure True) (pure False)

-- | C-style for loops.
forLoop :: ( AbstractValue address value effects
           , Member (Env address) effects
           )
        => Evaluator address value effects value -- ^ Initial statement
        -> Evaluator address value effects value -- ^ Condition
        -> Evaluator address value effects value -- ^ Increment/stepper
        -> Evaluator address value effects value -- ^ Body
        -> Evaluator address value effects value
forLoop initial cond step body =
  locally (initial *> while cond (body *> step))

-- | The fundamental looping primitive, built on top of 'ifthenelse'.
while :: AbstractValue address value effects
      => Evaluator address value effects value
      -> Evaluator address value effects value
      -> Evaluator address value effects value
while cond body = loop $ \ continue -> do
  this <- cond
  ifthenelse this (body *> continue) (pure unit)

-- | Do-while loop, built on top of while.
doWhile :: AbstractValue address value effects
        => Evaluator address value effects value
        -> Evaluator address value effects value
        -> Evaluator address value effects value
doWhile body cond = loop $ \ continue -> body *> do
  this <- cond
  ifthenelse this continue (pure unit)

makeNamespace :: ( AbstractValue address value effects
                 , Member (Env address) effects
                 , Member (Allocator address value) effects
                 )
              => Name
              -> address
              -> Maybe address
              -> Evaluator address value effects ()
              -> Evaluator address value effects value
makeNamespace name addr super body = do
  namespaceBinds <- Env.head <$> locally (body >> getEnv)
  v <- namespace name super namespaceBinds
  v <$ assign addr v


-- | Evaluate a term within the context of the scoped environment of 'scopedEnvTerm'.
evaluateInScopedEnv :: ( AbstractValue address value effects
                       , Member (Env address) effects
                       )
                    => address
                    -> Evaluator address value effects a
                    -> Evaluator address value effects a
evaluateInScopedEnv scopedEnvTerm term = do
  scopedEnv <- scopedEnvironment scopedEnvTerm
  env <- maybeM getEnv scopedEnv
  withEnv env term


-- | Evaluates a 'Value' returning the referenced value
value :: ( AbstractValue address value effects
         , Member (Deref address value) effects
         , Member (Env address) effects
         , Member (Resumable (EnvironmentError address)) effects
         )
      => ValueRef address
      -> Evaluator address value effects value
value = deref <=< address

-- | Evaluates a 'Subterm' to its rval
subtermValue :: ( AbstractValue address value effects
                , Member (Deref address value) effects
                , Member (Env address) effects
                , Member (Resumable (EnvironmentError address)) effects
                )
             => Subterm term (Evaluator address value effects (ValueRef address))
             -> Evaluator address value effects value
subtermValue = value <=< subtermRef

-- | Returns the address of a value referenced by a 'ValueRef'
address :: ( AbstractValue address value effects
           , Member (Env address) effects
           , Member (Resumable (EnvironmentError address)) effects
           )
        => ValueRef address
        -> Evaluator address value effects address
address (LvalLocal var) = variable var
address (LvalMember ptr prop) = evaluateInScopedEnv ptr (variable prop)
address (Rval addr) = pure addr

-- | Evaluates a 'Subterm' to the address of its rval
subtermAddress :: ( AbstractValue address value effects
                  , Member (Env address) effects
                  , Member (Resumable (EnvironmentError address)) effects
                  )
               => Subterm term (Evaluator address value effects (ValueRef address))
               -> Evaluator address value effects address
subtermAddress = address <=< subtermRef

-- | Convenience function for boxing a raw value and wrapping it in an Rval
rvalBox :: ( Member (Allocator address value) effects
           , Member Fresh effects
           )
        => value
        -> Evaluator address value effects (ValueRef address)
rvalBox val = Rval <$> box val
