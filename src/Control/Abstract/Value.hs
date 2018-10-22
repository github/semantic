{-# LANGUAGE DeriveAnyClass, GADTs, KindSignatures, Rank2Types, TypeOperators #-}
module Control.Abstract.Value
( AbstractValue(..)
, AbstractIntro(..)
, Comparator(..)
-- * Value effects
-- $valueEffects
, function
, BuiltIn(..)
, builtIn
, call
, Function(..)
, runFunction
, FunctionC(..)
, boolean
, asBool
, ifthenelse
, disjunction
, Boolean(..)
, runBoolean
, BooleanC(..)
, while
, doWhile
, forLoop
, While(..)
, runWhile
, WhileC(..)
, makeNamespace
, evaluateInScopedEnv
, address
, value
, rvalBox
) where

import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Effect.Carrier
import Data.Coerce
import Data.Abstract.BaseError
import Data.Abstract.Environment as Env
import Data.Abstract.Module
import Data.Abstract.Name
import Data.Abstract.Number as Number
import Data.Abstract.Ref
import Data.Scientific (Scientific)
import Data.Span
import Prologue hiding (TypeError, catchError)

-- | This datum is passed into liftComparison to handle the fact that Ruby and PHP
--   have built-in generalized-comparison ("spaceship") operators. If you want to
--   encapsulate a traditional, boolean-returning operator, wrap it in 'Concrete';
--   if you want the generalized comparator, pass in 'Generalized'. In 'AbstractValue'
--   instances, you can then then handle the different cases to return different
--   types, if that's what you need.
data Comparator
  = Concrete (forall a . Ord a => a -> a -> Bool)
  | Generalized

-- Value effects

-- $valueEffects
-- Value effects are effects modelling the /introduction/ & /elimination/ of some specific kind of value.
--
-- Modelling each of these as effects has several advantages∷
--
-- * It is strictly more flexible than modelling them as methods in a typeclass, as effect list–indexed typeclasses must be constrained at every list of effects at which they can be applied, whereas effect membership constraints can be deduced recursively (i.e. if @X@ is constrained to be a member of @effects@, it is automatically deducible that it is also a member of @Y \': effects@).
-- * It offers us the potential of specializing the handlers on a language-by-language basis without the need for extra type parameters (albeit at the cost of automatic selection).
-- * It offers us the potential of fine-grained specialization of the abstract domain, enabling convenient, piecemeal customization of the domain, and even semi-abstract domains.
-- * Finally, it should eventually allow us to customize _which_ value effects are available for a given language; e.g. a language without OO features would not require OO value effects. (This would also rely on 'Evaluatable' instances being able to specify membership constraints, which is not currently possible.)
--
-- In the concrete domain, introductions & eliminations respectively construct & pattern match against values, while in abstract domains they respectively construct & project finite sets of discrete observations of abstract values. For example, an abstract domain modelling integers as a sign (-, 0, or +) would introduce abstract values by mapping integers to their sign and eliminate them by mapping signs back to some canonical integer, e.g. - -> -1, 0 -> 0, + -> 1.

function :: (Member (Function term address value) sig, Carrier sig m) => Maybe Name -> [Name] -> term -> Evaluator term address value m value
function name params body = sendFunction (Function name params body ret)

data BuiltIn
  = Print
  | Show
  deriving (Eq, Ord, Show, Generic, NFData)

builtIn :: (Member (Function term address value) sig, Carrier sig m) => BuiltIn -> Evaluator term address value m value
builtIn = sendFunction . flip BuiltIn ret

call :: (Member (Function term address value) sig, Carrier sig m) => value -> address -> [address] -> Evaluator term address value m address
call fn self args = sendFunction (Call fn self args ret)

sendFunction :: (Member (Function term address value) sig, Carrier sig m) => Function term address value (Evaluator term address value m) (Evaluator term address value m a) -> Evaluator term address value m a
sendFunction = send

data Function term address value (m :: * -> *) k
  = Function (Maybe Name) [Name] term (value -> k)
  | BuiltIn BuiltIn (value -> k)
  | Call value address [address] (address -> k)
  deriving (Functor)

instance HFunctor (Function term address value) where
  hmap _ = coerce

instance Effect (Function term address value) where
  handle state handler (Function name params body k) = Function name params body (handler . (<$ state) . k)
  handle state handler (BuiltIn builtIn           k) = BuiltIn builtIn           (handler . (<$ state) . k)
  handle state handler (Call fn self addrs        k) = Call fn self addrs        (handler . (<$ state) . k)


runFunction :: Carrier (Function term address value :+: sig) (FunctionC term address value (Evaluator term address value m))
            => (term -> Evaluator term address value (FunctionC term address value (Evaluator term address value m)) address)
            -> Evaluator term address value (FunctionC term address value
              (Evaluator term address value m)) a
            -> Evaluator term address value m a
runFunction eval = flip runFunctionC eval . interpret . runEvaluator

newtype FunctionC term address value m a = FunctionC { runFunctionC :: (term -> Evaluator term address value (FunctionC term address value m) address) -> m a }


-- | Construct a boolean value in the abstract domain.
boolean :: (Member (Boolean value) sig, Carrier sig m) => Bool -> Evaluator term address value m value
boolean = send . flip Boolean ret

-- | Extract a 'Bool' from a given value.
asBool :: (Member (Boolean value) sig, Carrier sig m) => value -> Evaluator term address value m Bool
asBool = send . flip AsBool ret

-- | Eliminate boolean values. TODO: s/boolean/truthy
ifthenelse :: (Member (Boolean value) sig, Carrier sig m) => value -> Evaluator term address value m a -> Evaluator term address value m a -> Evaluator term address value m a
ifthenelse v t e = asBool v >>= \ c -> if c then t else e

-- | Compute the disjunction (boolean or) of two computed values. This should have short-circuiting semantics where applicable.
disjunction :: (Member (Boolean value) sig, Carrier sig m) => Evaluator term address value m value -> Evaluator term address value m value -> Evaluator term address value m value
disjunction a b = send (Disjunction a b ret)

data Boolean value m k
  = Boolean Bool (value -> k)
  | AsBool value (Bool -> k)
  | Disjunction (m value) (m value) (value -> k)
  deriving (Functor)

instance HFunctor (Boolean value) where
  hmap _ (Boolean b        k) = Boolean b k
  hmap _ (AsBool v         k) = AsBool v  k
  hmap f (Disjunction a b  k) = Disjunction (f a) (f b) k

runBoolean :: Carrier (Boolean value :+: sig) (BooleanC value (Eff m))
           => Evaluator term address value (BooleanC value (Eff m)) a
           -> Evaluator term address value m a
runBoolean = raiseHandler $ runBooleanC . interpret

newtype BooleanC value m a = BooleanC { runBooleanC :: m a }


-- | The fundamental looping primitive, built on top of 'ifthenelse'.
while :: (Member (While value) sig, Carrier sig m)
      => Evaluator term address value m value -- ^ Condition
      -> Evaluator term address value m value -- ^ Body
      -> Evaluator term address value m value
while cond body = send (While cond body ret)

-- | Do-while loop, built on top of while.
doWhile :: (Member (While value) sig, Carrier sig m)
  => Evaluator term address value m value -- ^ Body
  -> Evaluator term address value m value -- ^ Condition
  -> Evaluator term address value m value
doWhile body cond = body *> while cond body

-- | C-style for loops.
forLoop :: (Member (While value) sig, Member (Env address) sig, Carrier sig m)
  => Evaluator term address value m value -- ^ Initial statement
  -> Evaluator term address value m value -- ^ Condition
  -> Evaluator term address value m value -- ^ Increment/stepper
  -> Evaluator term address value m value -- ^ Body
  -> Evaluator term address value m value
forLoop initial cond step body =
  locally (initial *> while cond (body *> step))

data While value m k
  = While (m value) (m value) (value -> k)
  deriving (Functor)

instance HFunctor (While value) where
  hmap f (While cond body k) = While (f cond) (f body) k


runWhile :: Carrier (While value :+: sig) (WhileC (Evaluator term address value m))
         => Evaluator term address value (WhileC
           (Evaluator term address value m)) a
         -> Evaluator term address value m a
runWhile = runWhileC . interpret . runEvaluator

newtype WhileC m a = WhileC { runWhileC :: m a }


class Show value => AbstractIntro value where
  -- | Construct an abstract unit value.
  --   TODO: This might be the same as the empty tuple for some value types
  unit :: value

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
class AbstractIntro value => AbstractValue term address value carrier where
  -- | Cast numbers to integers
  castToInteger :: value -> Evaluator term address value carrier value


  -- | Lift a unary operator over a 'Num' to a function on 'value's.
  liftNumeric  :: (forall a . Num a => a -> a)
               -> (value -> Evaluator term address value carrier value)

  -- | Lift a pair of binary operators to a function on 'value's.
  --   You usually pass the same operator as both arguments, except in the cases where
  --   Haskell provides different functions for integral and fractional operations, such
  --   as division, exponentiation, and modulus.
  liftNumeric2 :: (forall a b. Number a -> Number b -> SomeNumber)
               -> (value -> value -> Evaluator term address value carrier value)

  -- | Lift a Comparator (usually wrapping a function like == or <=) to a function on values.
  liftComparison :: Comparator -> (value -> value -> Evaluator term address value carrier value)

  -- | Lift a unary bitwise operator to values. This is usually 'complement'.
  liftBitwise :: (forall a . Bits a => a -> a)
              -> (value -> Evaluator term address value carrier value)

  -- | Lift a binary bitwise operator to values. The Integral constraint is
  --   necessary to satisfy implementation details of Haskell left/right shift,
  --   but it's fine, since these are only ever operating on integral values.
  liftBitwise2 :: (forall a . (Integral a, Bits a) => a -> a -> a)
               -> (value -> value -> Evaluator term address value carrier value)

  unsignedRShift :: value -> value -> Evaluator term address value carrier value

  -- | Construct an N-ary tuple of multiple (possibly-disjoint) values
  tuple :: [address] -> Evaluator term address value carrier value

  -- | Construct an array of zero or more values.
  array :: [address] -> Evaluator term address value carrier value

  asArray :: value -> Evaluator term address value carrier [address]

  -- | Extract the contents of a key-value pair as a tuple.
  asPair :: value -> Evaluator term address value carrier (value, value)

  -- | Extract a 'Text' from a given value.
  asString :: value -> Evaluator term address value carrier Text

  -- | @index x i@ computes @x[i]@, with zero-indexing.
  index :: value -> value -> Evaluator term address value carrier address

  -- | Build a class value from a name and environment.
  klass :: Name             -- ^ The new class's identifier
        -> [address]        -- ^ A list of superclasses
        -> Bindings address -- ^ The environment to capture
        -> Evaluator term address value carrier value

  -- | Build a namespace value from a name and environment stack
  --
  -- Namespaces model closures with monoidal environments.
  namespace :: Name                 -- ^ The namespace's identifier
            -> Maybe address        -- The ancestor of the namespace
            -> Bindings address     -- ^ The environment to mappend
            -> Evaluator term address value carrier value

  -- | Extract the environment from any scoped object (e.g. classes, namespaces, etc).
  scopedEnvironment :: address -> Evaluator term address value carrier (Maybe (Environment address))


makeNamespace :: ( AbstractValue term address value m
                 , Member (Deref value) sig
                 , Member (Env address) sig
                 , Member (State (Heap address value)) sig
                 , Carrier sig m
                 , Ord address
                 )
              => Name
              -> address
              -> Maybe address
              -> Evaluator term address value m ()
              -> Evaluator term address value m value
makeNamespace name addr super body = do
  namespaceBinds <- Env.head <$> locally (body >> getEnv)
  v <- namespace name super namespaceBinds
  v <$ assign addr v


-- | Evaluate a term within the context of the scoped environment of 'scopedEnvTerm'.
evaluateInScopedEnv :: ( AbstractValue term address value m
                       , Member (Env address) sig
                       , Carrier sig m
                       )
                    => address
                    -> Evaluator term address value m a
                    -> Evaluator term address value m a
evaluateInScopedEnv receiver term = do
  scopedEnv <- scopedEnvironment receiver
  env <- maybeM getEnv scopedEnv
  withEvalContext (EvalContext (Just receiver) env) term


-- | Evaluates a 'Value' returning the referenced value
value :: ( AbstractValue term address value m
         , Carrier sig m
         , Member (Deref value) sig
         , Member (Env address) sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (AddressError address value))) sig
         , Member (Resumable (BaseError (EnvironmentError address))) sig
         , Member (State (Heap address value)) sig
         , Ord address
         )
      => ValueRef address
      -> Evaluator term address value m value
value = deref <=< address

-- | Returns the address of a value referenced by a 'ValueRef'
address :: ( AbstractValue term address value m
           , Carrier sig m
           , Member (Env address) sig
           , Member (Reader ModuleInfo) sig
           , Member (Reader Span) sig
           , Member (Resumable (BaseError (EnvironmentError address))) sig
           )
        => ValueRef address
        -> Evaluator term address value m address
address (LvalLocal var)       = variable var
address (LvalMember ptr prop) = evaluateInScopedEnv ptr (variable prop)
address (Rval addr)           = pure addr

-- | Convenience function for boxing a raw value and wrapping it in an Rval
rvalBox :: ( Member (Allocator address) sig
           , Member (Deref value) sig
           , Member Fresh sig
           , Member (State (Heap address value)) sig
           , Carrier sig m
           , Ord address
           )
        => value
        -> Evaluator term address value m (ValueRef address)
rvalBox val = Rval <$> box val
