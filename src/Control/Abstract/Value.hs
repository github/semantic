{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, Rank2Types, ScopedTypeVariables, TypeOperators #-}
module Control.Abstract.Value
( AbstractValue(..)
, AbstractFunction(..)
, AbstractHole(..)
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
import Control.Abstract.Context
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Monad.Effect.Fail
import Data.Abstract.Address (Address)
import Data.Abstract.Environment as Env
import Data.Abstract.Live (Live)
import Data.Abstract.Name
import Data.Abstract.Number as Number
import Data.Abstract.Ref
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import Data.Semigroup.Reducer hiding (unit)
import Data.Semilattice.Lower
import qualified Data.Set as Set
import Prelude hiding (fail)
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


lambda :: (Effectful m, Member (Function value effects) effects) => [Name] -> Set Name -> m effects value -> m effects value
lambda paramNames fvs body = send (Lambda paramNames fvs (lowerEff body))

call' :: (Effectful m, Member (Function value effects) effects) => value -> [m effects value] -> m effects value
call' fn params = send (Call fn (map lowerEff params))


lambda' :: (Effectful m, Members '[Fresh, Function value effects] effects, Monad (m effects))
        => (Name -> m effects value)
        -> m effects value
lambda' body = do
  var <- nameI <$> fresh
  lambda [var] lowerBound (body var)

lookup' :: (Effectful (m location opaque), Functor (m location opaque effects), Member (Reader (Map Name location)) effects) => Name -> m location opaque effects (Maybe location)
lookup' name = Map.lookup name <$> ask

allocType :: (Applicative (m Name opaque effects), Effectful (m Name opaque)) => Name -> m Name opaque effects Name
allocType = pure

assignType :: (Effectful (m location opaque), Member (State (Map location (Set Type))) effects, Monad (m location opaque effects), Ord location) => location -> Type -> m location opaque effects ()
assignType addr value = do
  cell <- gets (Map.lookup addr) >>= maybeM (pure (Set.empty))
  modify' (Map.insert addr (Set.insert value cell))

derefType :: (Alternative (m location opaque effects), Effectful (m location opaque), Members '[Fail, NonDet, State (Map location (Set Type))] effects, Monad (m location opaque effects), Ord location, Show location) => location -> m location opaque effects (Maybe Type)
derefType addr = do
  cell <- gets (Map.lookup addr) >>= maybeM (raiseEff (fail ("unallocated address: " <> show addr)))
  if Set.null cell then
    pure Nothing
  else
    Set.foldr ((<|>) . pure . Just) empty cell

runEnv :: Effectful (m location opaque) => m location opaque (Reader (Map Name location) ': effects) a -> m location opaque effects a
runEnv = runReader Map.empty

runHeapType :: Effectful (m Name opaque) => m Name opaque (State (Map Name (Set Type)) ': effects) a -> m Name opaque effects (a, Map Name (Set Type))
runHeapType = runState Map.empty


prog :: ( Effectful m
        , Members '[ Boolean value
                   , Fresh
                   , Function value effects
                   , Unit value
                   , Variable value
                   ] effects
        , Monad (m effects)
        )
     => value -> m effects value
prog b = do
  identity <- lambda' variable'
  iff b unit' (call' identity [unit'])

newtype Eval location opaque effects a = Eval { runEval :: Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)

deriving instance Member NonDet effects => Alternative (Eval location opaque effects)

data EmbedAny effect effects return where
  EmbedAny :: (effect \\ effects') effects => Eff effects' a -> EmbedAny effect effects a

type Embed effect effects = Eff (effect effects ': effects)

runType :: ( effects ~ (Function Type effects ': Unit Type ': Boolean Type ': Variable Type ': State (Map Name (Set Type)) ': Reader (Map Name Name) ': Fail ': NonDet ': rest)
           , (Function Type effects \\ effects) effects'
           , effects' ~ (Unit Type ': Boolean Type ': Variable Type ': State (Map Name (Set Type)) ': Reader (Map Name Name) ': Fail ': NonDet ': rest)
           , (Unit Type \\ effects') effects''
           , effects'' ~ (Boolean Type ': Variable Type ': State (Map Name (Set Type)) ': Reader (Map Name Name) ': Fail ': NonDet ': rest)
           , (Boolean Type \\ effects'') effects'''
           , effects''' ~ (Variable Type ': State (Map Name (Set Type)) ': Reader (Map Name Name) ': Fail ': NonDet ': rest)
           , (Variable Type \\ effects''') effects''''
           , effects'''' ~ (State (Map Name (Set Type)) ': Reader (Map Name Name) ': Fail ': NonDet ': rest)
           )
        => Eval Name opaque effects a
        -> Eval Name opaque rest [Either String (a, Map Name (Set Type))]
runType = runNonDetA . runFail . runEnv . runHeapType . runVariable derefType . runBooleanType . runUnitType . runFunctionType allocType assignType


data Function value effects return where
  Lambda :: [Name] -> Set Name -> Eff effects value -> Function value effects value
  Call   :: value -> [Eff effects value]            -> Function value effects value

variable' :: (Effectful m, Member (Variable value) effects) => Name -> m effects value
variable' = send . Variable

data Variable value return where
  Variable :: Name -> Variable value value

runVariable :: forall m location opaque effects effects' value a
            .  ( Effectful (m location opaque)
               , (Variable value \\ effects) effects'
               , Members '[ Fail
                          , Reader (Map Name location)
                          , State (Map location value)
                          ] effects'
               , Monad (m location opaque effects')
               , Show location
               )
            => (location -> m location opaque effects' (Maybe value))
            -> m location opaque effects a
            -> m location opaque effects' a
runVariable deref = go
  where go :: forall a . m location opaque effects a -> m location opaque effects' a
        go = interpretAny (\ (Variable name) -> do
          addr <- lookup' name >>= maybeM (raiseEff (fail ("free variable: " <> show name)))
          deref addr >>= maybeM (raiseEff (fail ("uninitialized address: " <> show addr))))


unit' :: (Effectful m, Member (Unit value) effects) => m effects value
unit' = send Unit


data Unit value return where
  Unit :: Unit value value


bool :: (Effectful m, Member (Boolean value) effects) => Bool -> m effects value
bool = send . Bool

asBool' :: (Effectful m, Member (Boolean value) effects) => value -> m effects Bool
asBool' = send . AsBool

iff :: (Effectful m, Member (Boolean value) effects, Monad (m effects)) => value -> m effects a -> m effects a -> m effects a
iff c t e = asBool' c >>= \ c' -> if c' then t else e

data Boolean value return where
  Bool :: Bool -> Boolean value value
  AsBool :: value -> Boolean value Bool


data Value location effects
  = Closure [Name] (Eff effects (Value location effects)) (Map Name location)
  | Unit'
  | Bool' Bool

liftHandler :: (forall a . Eff effects a -> Eff effects' a) -> Value location effects -> Value location effects'
liftHandler handler = go where go (Closure names body env) = Closure names (handler (go <$> body)) env

runFunctionValue :: forall m location opaque effects effects' a
                 .  ( Effectful (m location opaque)
                    , Members '[ Reader (Map Name location)
                               , Reader ModuleInfo
                               , Reader PackageInfo
                               ] effects
                    , Members '[ Reader (Map Name location)
                               , Reader ModuleInfo
                               , Reader PackageInfo
                               ] effects'
                    , Monad (m location opaque effects)
                    , Monad (m location opaque effects')
                    , (Function (Value location effects) effects \\ effects) effects'
                    )
                 => (Name -> m location opaque effects location)
                 -> (location -> Value location effects -> m location opaque effects ())
                 -> m location opaque effects a
                 -> m location opaque effects' a
runFunctionValue alloc assign = go
  where go :: forall a . m location opaque effects a -> m location opaque effects' a
        go = interpretAny $ \ eff -> case eff of
          Lambda params fvs body -> do
            packageInfo <- currentPackage
            moduleInfo <- currentModule
            env <- Map.filterWithKey (fmap (`Set.member` fvs) . const) <$> ask
            let body' = withCurrentPackage packageInfo (withCurrentModule moduleInfo body)
            pure (Closure params body' env)
          Call (Closure paramNames body env) params -> go $ do
            bindings <- foldr (\ (name, param) rest -> do
              v <- param
              a <- alloc name
              assign a v
              Map.insert name a <$> rest) (pure env) (zip paramNames (map raiseEff params))
            local (Map.unionWith const bindings) (raiseEff body)

runUnitValue :: ( Applicative (m location opaque effects')
                , Effectful (m location opaque)
                , (Unit (Value location effects) \\ effects) effects'
                )
             => m location opaque effects a
             -> m location opaque effects' a
runUnitValue = interpretAny (\ Unit -> pure Unit')

runBooleanValue :: ( Applicative (m location opaque effects')
                   , Effectful (m location opaque)
                   , (Boolean (Value location effects) \\ effects) effects'
                   )
                => m location opaque effects a
                -> m location opaque effects' a
runBooleanValue = interpretAny (\ eff -> case eff of
  Bool b -> pure (Bool' b)
  AsBool (Bool' b) -> pure b)


data Type
  = Type :-> Type
  | Product [Type]
  | TVar Int
  | BoolT
  deriving (Eq, Ord, Show)

runFunctionType :: forall m location opaque effects effects' a
                .  ( Alternative (m location opaque effects)
                   , Effectful (m location opaque)
                   , Members '[ Fresh
                              , Reader (Map Name location)
                              , Reader ModuleInfo
                              , Reader PackageInfo
                              ] effects
                   , Monad (m location opaque effects)
                   , (Function Type effects \\ effects) effects'
                   )
                => (Name -> m location opaque effects location)
                -> (location -> Type -> m location opaque effects ())
                -> m location opaque effects a
                -> m location opaque effects' a
runFunctionType alloc assign = go
  where go :: forall a . m location opaque effects a -> m location opaque effects' a
        go = interpretAny $ \ eff -> case eff of
          Lambda params _ body -> go $ do
            (bindings, tvars) <- foldr (\ name rest -> do
              a <- alloc name
              tvar <- TVar <$> fresh
              assign a tvar
              bimap (Map.insert name a) (tvar :) <$> rest) (pure (Map.empty, [])) params
            (Product tvars :->) <$> local (Map.unionWith const bindings) (raiseEff body)
          Call fn params -> go $ do
            paramTypes <- traverse raiseEff params
            case fn of
              Product argTypes :-> ret -> do
                guard (and (zipWith (==) paramTypes argTypes))
                pure ret
              _ -> empty

runUnitType :: ( Applicative (m location opaque effects')
               , Effectful (m location opaque)
               , (Unit Type \\ effects) effects'
               )
            => m location opaque effects a
            -> m location opaque effects' a
runUnitType = interpretAny (\ Unit -> pure (Product []))

runBooleanType :: ( Alternative (m location opaque effects')
                  , Effectful (m location opaque)
                  , (Boolean Type \\ effects) effects'
                  )
               => m location opaque effects a
               -> m location opaque effects' a
runBooleanType = interpretAny (\ eff -> case eff of
  Bool _ -> pure BoolT
  AsBool BoolT -> pure True <|> pure False)


class Show value => AbstractFunction location value effects where
  -- | Build a closure (a binder like a lambda or method definition).
  closure :: [Name]                                 -- ^ The parameter names.
          -> Set Name                               -- ^ The set of free variables to close over.
          -> Evaluator location value effects value -- ^ The evaluator for the body of the closure.
          -> Evaluator location value effects value
  -- | Evaluate an application (like a function call).
  call :: value -> [Evaluator location value effects value] -> Evaluator location value effects value


-- | A 'Monad' abstracting the evaluation of (and under) binding constructs (functions, methods, etc).
--
--   This allows us to abstract the choice of whether to evaluate under binders for different value types.
class AbstractFunction location value effects => AbstractValue location value effects where
  -- | Construct an abstract unit value.
  --   TODO: This might be the same as the empty tuple for some value types
  unit :: Evaluator location value effects value

  -- | Construct an abstract integral value.
  integer :: Integer -> Evaluator location value effects value

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

  -- | Construct an abstract boolean value.
  boolean :: Bool -> Evaluator location value effects value

  -- | Construct an abstract string value.
  string :: ByteString -> Evaluator location value effects value

  -- | Construct a self-evaluating symbol value.
  --   TODO: Should these be interned in some table to provide stronger uniqueness guarantees?
  symbol :: ByteString -> Evaluator location value effects value

  -- | Construct a floating-point value.
  float :: Scientific -> Evaluator location value effects value

  -- | Construct a rational value.
  rational :: Rational -> Evaluator location value effects value

  -- | Construct an N-ary tuple of multiple (possibly-disjoint) values
  multiple :: [value] -> Evaluator location value effects value

  -- | Construct an array of zero or more values.
  array :: [value] -> Evaluator location value effects value

  -- | Construct a key-value pair for use in a hash.
  kvPair :: value -> value -> Evaluator location value effects value

  -- | Extract the contents of a key-value pair as a tuple.
  asPair :: value -> Evaluator location value effects (value, value)

  -- | Construct a hash out of pairs.
  hash :: [(value, value)] -> Evaluator location value effects value

  -- | Extract a 'ByteString' from a given value.
  asString :: value -> Evaluator location value effects ByteString

  -- | Eliminate boolean values. TODO: s/boolean/truthy
  ifthenelse :: value -> Evaluator location value effects a -> Evaluator location value effects a -> Evaluator location value effects a

  -- | Construct the nil/null datatype.
  null :: Evaluator location value effects value

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
  localize (initial *> while cond (body *> step))

-- | The fundamental looping primitive, built on top of 'ifthenelse'.
while :: AbstractValue location value effects
      => Evaluator location value effects value
      -> Evaluator location value effects value
      -> Evaluator location value effects value
while cond body = loop $ \ continue -> do
  this <- cond
  ifthenelse this (body *> continue) unit

-- | Do-while loop, built on top of while.
doWhile :: AbstractValue location value effects
        => Evaluator location value effects value
        -> Evaluator location value effects value
        -> Evaluator location value effects value
doWhile body cond = loop $ \ continue -> body *> do
  this <- cond
  ifthenelse this continue unit

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
                       , Member (State (Environment location)) effects
                       )
                    => Evaluator location value effects value
                    -> Evaluator location value effects value
                    -> Evaluator location value effects value
evaluateInScopedEnv scopedEnvTerm term = do
  scopedEnv <- scopedEnvTerm >>= scopedEnvironment
  maybe term (flip localEnv term . mergeEnvs) scopedEnv


-- | Evaluates a 'Value' returning the referenced value
value :: ( AbstractValue location value effects
         , Members '[ Allocator location value
                    , Reader (Environment location)
                    , Resumable (EnvironmentError value)
                    , State (Environment location)
                    , State (Heap location (Cell location) value)
                    ] effects
         )
      => ValueRef value
      -> Evaluator location value effects value
value (LvalLocal var) = variable var
value (LvalMember obj prop) = evaluateInScopedEnv (pure obj) (variable prop)
value (Rval val) = pure val

-- | Evaluates a 'Subterm' to its rval
subtermValue :: ( AbstractValue location value effects
                , Members '[ Allocator location value
                           , Reader (Environment location)
                           , Resumable (EnvironmentError value)
                           , State (Environment location)
                           , State (Heap location (Cell location) value)
                           ] effects
                )
             => Subterm term (Evaluator location value effects (ValueRef value))
             -> Evaluator location value effects value
subtermValue = value <=< subtermRef


-- | Value types, e.g. closures, which can root a set of addresses.
class ValueRoots location value where
  -- | Compute the set of addresses rooted by a given value.
  valueRoots :: value -> Live location value
