{-# LANGUAGE DeriveAnyClass, GADTs, KindSignatures, LambdaCase, Rank2Types, ScopedTypeVariables, TypeOperators #-}
module Control.Abstract.Value
( AbstractValue(..)
, AbstractIntro(..)
, Comparator(..)
-- * Domain effects
-- $domainEffects
, function
, BuiltIn(..)
, bindThis
, builtIn
, call
, Function(..)
, runFunction
, FunctionC(..)
, boolean
, asBool
, ifthenelse
, Boolean(..)
, runBoolean
, BooleanC(..)
, while
, doWhile
, forLoop
, While(..)
, runWhile
, WhileC(..)
, unit
, Unit(..)
, runUnit
, UnitC(..)
, string
, asString
, String(..)
, StringC(..)
, runString
, integer
, float
, rational
, liftNumeric
, liftNumeric2
, Numeric(..)
, NumericC(..)
, object
, scopedEnvironment
, klass
, Object(..)
, ObjectC(..)
, runObject
, runNumeric
, castToInteger
, liftBitwise
, liftBitwise2
, unsignedRShift
, Bitwise(..)
, BitwiseC(..)
, runBitwise
, array
, asArray
, Array(..)
, ArrayC(..)
, runArray
, Control.Abstract.Value.hash
, kvPair
, Hash(..)
, runHash
, HashC(..)
) where

import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Abstract.ScopeGraph (Allocator, CurrentScope, Declaration, ScopeGraph)
import Control.Effect.Carrier
import Data.Abstract.BaseError
import Data.Abstract.Module
import Data.Abstract.Name
import Data.Abstract.Number (Number, SomeNumber)
import Data.Scientific (Scientific)
import Data.Span
import Prelude hiding (String)
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

-- Domain effects

-- $domainEffects
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

function :: (Member (Function term address value) sig, Carrier sig m) => Name -> [Name] -> term -> address -> Evaluator term address value m value
function name params body scope = sendFunction (Function name params body scope ret)

data BuiltIn
  = Print
  | Show
  deriving (Eq, Ord, Show, Generic, NFData)

builtIn :: (Member (Function term address value) sig, Carrier sig m) => address -> BuiltIn -> Evaluator term address value m value
builtIn address = sendFunction . flip (BuiltIn address) ret

call :: (Member (Function term address value) sig, Carrier sig m) => value -> [value] -> Evaluator term address value m value
call fn args = sendFunction (Call fn args ret)

sendFunction :: (Member (Function term address value) sig, Carrier sig m) => Function term address value (Evaluator term address value m) (Evaluator term address value m a) -> Evaluator term address value m a
sendFunction = send

bindThis :: (Member (Function term address value) sig, Carrier sig m) => value -> value -> Evaluator term address value m value
bindThis this that = sendFunction (Bind this that ret)

data Function term address value (m :: * -> *) k
  = Function Name [Name] term address (value -> k) -- ^ A function is parameterized by its name, parameter names, body, parent scope, and returns a ValueRef.
  | BuiltIn address BuiltIn (value -> k)           -- ^ A built-in is parameterized by its parent scope, BuiltIn type, and returns a value.
  | Call value [value] (value -> k)                -- ^ A Call takes a set of values as parameters and returns a ValueRef.
  | Bind value value (value -> k)
  deriving (Functor)

instance HFunctor (Function term address value) where
  hmap _ = coerce

instance Effect (Function term address value) where
  handle state handler = coerce . fmap (handler . (<$ state))


runFunction :: Carrier (Function term address value :+: sig) (FunctionC term address value (Eff m))
            => (term -> Evaluator term address value (FunctionC term address value (Eff m)) value)
            -> Evaluator term address value (FunctionC term address value (Eff m)) a
            -> Evaluator term address value m a
runFunction eval = raiseHandler (flip runFunctionC (runEvaluator . eval) . interpret)

newtype FunctionC term address value m a = FunctionC { runFunctionC :: (term -> Eff (FunctionC term address value m) value) -> m a }


-- | Construct a boolean value in the abstract domain.
boolean :: (Member (Boolean value) sig, Carrier sig m) => Bool -> m value
boolean = send . flip Boolean ret

-- | Extract a 'Bool' from a given value.
asBool :: (Member (Boolean value) sig, Carrier sig m) => value -> m Bool
asBool = send . flip AsBool ret

-- | Eliminate boolean values. TODO: s/boolean/truthy
ifthenelse :: (Member (Boolean value) sig, Carrier sig m, Monad m) => value -> m a -> m a -> m a
ifthenelse v t e = asBool v >>= \ c -> if c then t else e

data Boolean value (m :: * -> *) k
  = Boolean Bool (value -> k)
  | AsBool value (Bool -> k)
  deriving (Functor)

instance HFunctor (Boolean value) where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect (Boolean value) where
  handle state handler = \case
    Boolean b k -> Boolean b (handler . (<$ state) . k)
    AsBool  v k -> AsBool  v (handler . (<$ state) . k)

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
forLoop :: ( Carrier sig m
           , Member (Allocator address) sig
           , Member (Reader ModuleInfo) sig
           , Member (Reader Span) sig
           , Member (Resumable (BaseError (HeapError address))) sig
           , Member (State (Heap address address value)) sig
           , Member (State (ScopeGraph address)) sig
           , Member (Reader (CurrentFrame address)) sig
           , Member (Reader (CurrentScope address)) sig
           , Member (While value) sig
           , Member Fresh sig
           , Ord address
           )
  => Evaluator term address value m value -- ^ Initial statement
  -> Evaluator term address value m value -- ^ Condition
  -> Evaluator term address value m value -- ^ Increment/stepper
  -> Evaluator term address value m value -- ^ Body
  -> Evaluator term address value m value
forLoop initial cond step body = initial *> while cond (withLexicalScopeAndFrame body *> step)

data While value m k
  = While (m value) (m value) (value -> k)
  deriving (Functor)

instance HFunctor (While value) where
  hmap f (While cond body k) = While (f cond) (f body) k

runWhile :: Carrier (While value :+: sig) (WhileC value (Eff m))
         => Evaluator term address value (WhileC value (Eff m)) a
         -> Evaluator term address value m a
runWhile = raiseHandler $ runWhileC . interpret

newtype WhileC value m a = WhileC { runWhileC :: m a }


-- | Construct an abstract unit value.
unit :: (Carrier sig m, Member (Unit value) sig) => Evaluator term address value m value
unit = send (Unit ret)

newtype Unit value (m :: * -> *) k
  = Unit (value -> k)
  deriving (Functor)

instance HFunctor (Unit value) where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect (Unit value) where
  handle state handler (Unit k) = Unit (handler . (<$ state) . k)

runUnit :: Carrier (Unit value :+: sig) (UnitC value (Eff m))
        => Evaluator term address value (UnitC value (Eff m)) a
        -> Evaluator term address value m a
runUnit = raiseHandler $ runUnitC . interpret

newtype UnitC value m a = UnitC { runUnitC :: m a }


-- | Construct a String value in the abstract domain.
string :: (Member (String value) sig, Carrier sig m) => Text -> m value
string t = send (String t ret)

-- | Extract 'Text' from a given value.
asString :: (Member (String value) sig, Carrier sig m) => value -> m Text
asString v = send (AsString v ret)

data String value (m :: * -> *) k
  = String Text (value -> k)
  | AsString value (Text -> k)
  deriving (Functor)

instance HFunctor (String value) where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect (String value) where
  handle state handler (String text k) = String text (handler . (<$ state) . k)
  handle state handler (AsString v k) = AsString v (handler . (<$ state) . k)

newtype StringC value m a = StringC { runStringC :: m a }

runString :: Carrier (String value :+: sig) (StringC value (Eff m))
              -- Have to mention String because we don't know what the value type is
              -- Enables single effect handler function across abstract, concret, type
              -- Allows us to define something once in evaluate in Semantic.Analysis
              -- instead of crazy composition
          => Evaluator term address value (StringC value (Eff m)) a
          -> Evaluator term address value m a
runString = raiseHandler $ runStringC . interpret


-- | Construct an abstract integral value.
integer :: (Member (Numeric value) sig, Carrier sig m) => Integer -> m value
integer t = send (Integer t ret)

-- | Construct a floating-point value.
float :: (Member (Numeric value) sig, Carrier sig m) => Scientific -> m value
float t = send (Float t ret)

-- | Construct a rational value.
rational :: (Member (Numeric value) sig, Carrier sig m) => Rational -> m value
rational t = send (Rational t ret)

-- | Lift a unary operator over a 'Num' to a function on 'value's.
liftNumeric  :: (Member (Numeric value) sig, Carrier sig m)
             => (forall a . Num a => a -> a)
             -> value
             -> m value
liftNumeric t v = send (LiftNumeric t v ret)

-- | Lift a pair of binary operators to a function on 'value's.
--   You usually pass the same operator as both arguments, except in the cases where
--   Haskell provides different functions for integral and fractional operations, such
--   as division, exponentiation, and modulus.
liftNumeric2 :: (Member (Numeric value) sig, Carrier sig m)
             => (forall a b. Number a -> Number b -> SomeNumber)
             -> value
             -> value
             -> m value
liftNumeric2 t v1 v2 = send (LiftNumeric2 t v1 v2 ret)

data Numeric value (m :: * -> *) k
  = Integer Integer (value -> k)
  | Float Scientific (value -> k)
  | Rational Rational (value -> k)
  | LiftNumeric (forall a . Num a => a -> a) value (value -> k)
  | LiftNumeric2 (forall a b. Number a -> Number b -> SomeNumber) value value (value -> k)
  deriving (Functor)

instance HFunctor (Numeric value) where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect (Numeric value) where
  handle state handler = coerce . fmap (handler . (<$ state))

newtype NumericC value m a = NumericC { runNumericC :: m a }

runNumeric :: Carrier (Numeric value :+: sig) (NumericC value (Eff m))
           => Evaluator term address value (NumericC value (Eff m)) a
           -> Evaluator term address value m a
runNumeric = raiseHandler $ runNumericC . interpret


-- | Cast numbers to integers
castToInteger :: (Member (Bitwise value) sig, Carrier sig m) => value -> m value
castToInteger t = send (CastToInteger t ret)

-- | Lift a unary bitwise operator to values. This is usually 'complement'.
liftBitwise :: (Member (Bitwise value) sig, Carrier sig m)
            => (forall a . Bits a => a -> a)
            -> value
            -> m value
liftBitwise t v = send (LiftBitwise t v ret)

-- | Lift a binary bitwise operator to values. The Integral constraint is
--   necessary to satisfy implementation details of Haskell left/right shift,
--   but it's fine, since these are only ever operating on integral values.
liftBitwise2 :: (Member (Bitwise value) sig, Carrier sig m)
             => (forall a . (Integral a, Bits a) => a -> a -> a)
             -> value
             -> value
             -> m value
liftBitwise2 t v1 v2 = send (LiftBitwise2 t v1 v2 ret)

unsignedRShift :: (Member (Bitwise value) sig, Carrier sig m)
               => value
               -> value
               -> m value
unsignedRShift v1 v2 = send (UnsignedRShift v1 v2 ret)

data Bitwise value (m :: * -> *) k
  = CastToInteger value (value -> k)
  | LiftBitwise (forall a . Bits a => a -> a) value (value -> k)
  | LiftBitwise2 (forall a . (Integral a, Bits a) => a -> a -> a) value value (value -> k)
  | UnsignedRShift value value (value -> k)
  deriving (Functor)

instance HFunctor (Bitwise value) where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect (Bitwise value) where
  handle state handler = coerce . fmap (handler . (<$ state))

runBitwise :: Carrier (Bitwise value :+: sig) (BitwiseC value (Eff m))
           => Evaluator term address value (BitwiseC value (Eff m)) a
           -> Evaluator term address value m a
runBitwise = raiseHandler $ runBitwiseC . interpret

newtype BitwiseC value m a = BitwiseC { runBitwiseC :: m a }


object :: (Member (Object address value) sig, Carrier sig m) => address -> m value
object address = send (Object address ret)

-- | Extract the environment from any scoped object (e.g. classes, namespaces, etc).
scopedEnvironment :: (Member (Object address value) sig, Carrier sig m) => value -> m (Maybe address)
scopedEnvironment value = send (ScopedEnvironment value ret)

-- | Build a class value from a name and environment.
-- declaration is the new class's identifier
-- address is the environment to capture
klass :: (Member (Object address value) sig, Carrier sig m) => Declaration -> address -> m value
klass d a = send (Klass d a ret)

data Object address value (m :: * -> *) k
  = Object address (value -> k)
  | ScopedEnvironment value (Maybe address -> k)
  | Klass Declaration address (value -> k)
  deriving (Functor)

instance HFunctor (Object address value) where
    hmap _ = coerce
    {-# INLINE hmap #-}

instance Effect (Object address value) where
    handle state handler = coerce . fmap (handler . (<$ state))

newtype ObjectC address value m a = ObjectC { runObjectC :: m a }

runObject :: Carrier (Object address value :+: sig) (ObjectC address value (Eff m))
           => Evaluator term address value (ObjectC address value (Eff m)) a
           -> Evaluator term address value m a
runObject = raiseHandler $ runObjectC . interpret

-- | Construct an array of zero or more values.
-- array :: [value] -> Evaluator term address value carrier value
array :: (Member (Array value) sig, Carrier sig m) => [value] -> m value
array v = send (Array v ret)

-- asArray :: value -> Evaluator term address value carrier [value]
asArray :: (Member (Array value) sig, Carrier sig m) => value -> m [value]
asArray v = send (AsArray v ret)

data Array value (m :: * -> *) k
  = Array [value] (value -> k)
  | AsArray value ([value] -> k)
  deriving (Functor)

instance HFunctor (Array value) where
    hmap _ = coerce
    {-# INLINE hmap #-}

instance Effect (Array value) where
    handle state handler = coerce . fmap (handler . (<$ state))

newtype ArrayC value m a = ArrayC { runArrayC :: m a }

runArray :: Carrier (Array value :+: sig) (ArrayC value (Eff m))
           => Evaluator term address value (ArrayC value (Eff m)) a
           -> Evaluator term address value m a
runArray = raiseHandler $ runArrayC . interpret

-- | Construct a hash out of pairs.
-- hash :: [(value, value)] -> value
hash :: (Member (Hash value) sig, Carrier sig m) => [(value, value)] -> m value
hash v = send (Hash v ret)

-- | Construct a key-value pair for use in a hash.
-- kvPair :: value -> value -> value
kvPair :: (Member (Hash value) sig, Carrier sig m) => value -> value -> m value
kvPair v1 v2 = send (KvPair v1 v2 ret)

data Hash value (m :: * -> *) k
  = Hash [(value, value)] (value -> k)
  | KvPair value value (value -> k)
  deriving (Functor)

instance HFunctor (Hash value) where
    hmap _ = coerce
    {-# INLINE hmap #-}

instance Effect (Hash value) where
    handle state handler = coerce . fmap (handler . (<$ state))

newtype HashC value m a = HashC { runHashC :: m a }

runHash :: Carrier (Hash value :+: sig) (HashC value (Eff m))
           => Evaluator term address value (HashC value (Eff m)) a
           -> Evaluator term address value m a
runHash = raiseHandler $ runHashC . interpret

class Show value => AbstractIntro value where
  -- | Construct the nil/null datatype.
  null :: value

-- | A 'Monad' abstracting the evaluation of (and under) binding constructs (functions, methods, etc).
--
--   This allows us to abstract the choice of whether to evaluate under binders for different value types.
class AbstractIntro value => AbstractValue term address value carrier where
  -- | Lift a Comparator (usually wrapping a function like == or <=) to a function on values.
  liftComparison :: Comparator -> (value -> value -> Evaluator term address value carrier value)

  -- | Construct an N-ary tuple of multiple (possibly-disjoint) values
  tuple :: [value] -> Evaluator term address value carrier value

  -- | Extract the contents of a key-value pair as a tuple.
  asPair :: value -> Evaluator term address value carrier (value, value)

  -- | @index x i@ computes @x[i]@, with zero-indexing.
  index :: value -> value -> Evaluator term address value carrier value

  -- | Build a namespace value from a name and environment stack
  --
  -- Namespaces model closures with monoidal environments.
  namespace :: Name                 -- ^ The namespace's identifier
            -> address              -- ^ The frame of the namespace.
            -> Evaluator term address value carrier value
