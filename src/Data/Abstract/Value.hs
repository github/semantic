{-# LANGUAGE GADTs, RankNTypes, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Value where

import Control.Abstract
import Data.Abstract.Address
import Data.Abstract.Environment (Environment, emptyEnv, mergeEnvs)
import qualified Data.Abstract.Environment as Env
import Data.Abstract.Name
import qualified Data.Abstract.Number as Number
import Data.Coerce
import Data.List (genericIndex, genericLength)
import Data.Scientific (Scientific)
import Data.Scientific.Exts
import Data.Semigroup.Reducer
import qualified Data.Set as Set
import Prologue

data Value location body
  = Closure PackageInfo ModuleInfo [Name] (ClosureBody location body) (Environment location)
  | Unit
  | Boolean Bool
  | Integer  (Number.Number Integer)
  | Rational (Number.Number Rational)
  | Float    (Number.Number Scientific)
  | String ByteString
  | Symbol ByteString
  | Tuple [Value location body]
  | Array [Value location body]
  | Class Name (Environment location)
  | Namespace Name (Environment location)
  | KVPair (Value location body) (Value location body)
  | Hash [Value location body]
  | Null
  | Hole
  deriving (Eq, Ord, Show)

data ClosureBody location body = ClosureBody { closureBodyId :: Int, closureBody :: body (Value location body) }

instance Eq   (ClosureBody location body) where
  (==) = (==) `on` closureBodyId

instance Ord  (ClosureBody location body) where
  compare = compare `on` closureBodyId

instance Show (ClosureBody location body) where
  showsPrec d (ClosureBody i _) = showsBinaryWith showsPrec (const showChar) "ClosureBody" d i '_'


instance Ord location => ValueRoots location (Value location body) where
  valueRoots v
    | Closure _ _ _ _ env <- v = Env.addresses env
    | otherwise                = mempty


instance AbstractHole (Value location body) where
  hole = Hole

instance ( Coercible body (Eff effects)
         , Member (Allocator location (Value location body)) effects
         , Member Fresh effects
         , Member (Reader ModuleInfo) effects
         , Member (Reader PackageInfo) effects
         , Member (Resumable (ValueError location body)) effects
         , Member (Return (Value location body)) effects
         , Member (State (Environment location)) effects
         , Member (State (Heap location (Cell location) (Value location body))) effects
         , Ord location
         , Reducer (Value location body) (Cell location (Value location body))
         , Show location
         )
      => AbstractFunction location (Value location body) effects where
  closure parameters freeVariables body = do
    packageInfo <- currentPackage
    moduleInfo <- currentModule
    i <- fresh
    Closure packageInfo moduleInfo parameters (ClosureBody i (coerce (lowerEff body))) . Env.intersect (foldr Set.delete freeVariables parameters) <$> getEnv

  call op params = do
    case op of
      Closure packageInfo moduleInfo names (ClosureBody _ body) env -> do
        -- Evaluate the bindings and body with the closure’s package/module info in scope in order to
        -- charge them to the closure's origin.
        withCurrentPackage packageInfo . withCurrentModule moduleInfo $ do
          bindings <- foldr (\ (name, param) rest -> do
            v <- param
            a <- alloc name
            assign a v
            Env.insert name (unAddress a) <$> rest) (pure env) (zip names params)
          localEnv (mergeEnvs bindings) (raiseEff (coerce body) `catchReturn` \ (Return value) -> pure value)
      _ -> throwValueError (CallError op)


instance Show location => AbstractIntro (Value location body) where
  unit     = Unit
  integer  = Integer . Number.Integer
  boolean  = Boolean
  string   = String
  float    = Float . Number.Decimal
  symbol   = Symbol
  rational = Rational . Number.Ratio

  multiple = Tuple

  kvPair = KVPair
  hash = Hash . map (uncurry KVPair)

  null     = Null


-- | Construct a 'Value' wrapping the value arguments (if any).
instance ( Coercible body (Eff effects)
         , Member (Allocator location (Value location body)) effects
         , Member Fresh effects
         , Member (LoopControl (Value location body)) effects
         , Member (Reader (Environment location)) effects
         , Member (Reader ModuleInfo) effects
         , Member (Reader PackageInfo) effects
         , Member (Resumable (ValueError location body)) effects
         , Member (Return (Value location body)) effects
         , Member (State (Environment location)) effects
         , Member (State (Heap location (Cell location) (Value location body))) effects
         , Ord location
         , Reducer (Value location body) (Cell location (Value location body))
         , Show location
         )
      => AbstractValue location (Value location body) effects where
  asPair val
    | KVPair k v <- val = pure (k, v)
    | otherwise = throwValueError $ KeyValueError val

  array    = pure . Array

  klass n [] env = pure $ Class n env
  klass n supers env = do
    product <- foldl mergeEnvs emptyEnv . catMaybes <$> traverse scopedEnvironment supers
    pure $ Class n (mergeEnvs product env)

  namespace n env = do
    maybeAddr <- lookupEnv n
    env' <- maybe (pure emptyEnv) (asNamespaceEnv <=< deref) maybeAddr
    pure (Namespace n (Env.mergeNewer env' env))
    where asNamespaceEnv v
            | Namespace _ env' <- v = pure env'
            | otherwise             = throwValueError $ NamespaceError ("expected " <> show v <> " to be a namespace")

  scopedEnvironment o
    | Class _ env <- o = pure (Just env)
    | Namespace _ env <- o = pure (Just env)
    | otherwise = pure Nothing

  asString v
    | String n <- v = pure n
    | otherwise     = throwValueError $ StringError v

  ifthenelse cond if' else' = do
    bool <- case cond of { Boolean b -> pure b ; _ -> throwValueError (BoolError cond) }
    if bool then if' else else'

  index = go where
    tryIdx list ii
      | ii > genericLength list = throwValueError (BoundsError list ii)
      | otherwise               = pure (genericIndex list ii)
    go arr idx
      | (Array arr, Integer (Number.Integer i)) <- (arr, idx) = tryIdx arr i
      | (Tuple tup, Integer (Number.Integer i)) <- (arr, idx) = tryIdx tup i
      | otherwise = throwValueError (IndexError arr idx)

  liftNumeric f arg
    | Integer (Number.Integer i) <- arg = pure . integer  $ f i
    | Float (Number.Decimal d)   <- arg = pure . float    $ f d
    | Rational (Number.Ratio r)  <- arg = pure . rational $ f r
    | otherwise = throwValueError (NumericError arg)

  liftNumeric2 f left right
    | (Integer  i, Integer j)  <- pair = tentative f i j & specialize
    | (Integer  i, Rational j) <- pair = tentative f i j & specialize
    | (Integer  i, Float j)    <- pair = tentative f i j & specialize
    | (Rational i, Integer j)  <- pair = tentative f i j & specialize
    | (Rational i, Rational j) <- pair = tentative f i j & specialize
    | (Rational i, Float j)    <- pair = tentative f i j & specialize
    | (Float    i, Integer j)  <- pair = tentative f i j & specialize
    | (Float    i, Rational j) <- pair = tentative f i j & specialize
    | (Float    i, Float j)    <- pair = tentative f i j & specialize
    | otherwise = throwValueError (Numeric2Error left right)
      where
        tentative x i j = attemptUnsafeArithmetic (x i j)

        -- Dispatch whatever's contained inside a 'Number.SomeNumber' to its appropriate 'MonadValue' ctor
        specialize :: (AbstractValue location (Value location body) effects, Member (Resumable (ValueError location body)) effects) => Either ArithException Number.SomeNumber -> Evaluator location (Value location body) effects (Value location body)
        specialize (Left exc) = throwValueError (ArithmeticError exc)
        specialize (Right (Number.SomeNumber (Number.Integer i))) = pure $ integer i
        specialize (Right (Number.SomeNumber (Number.Ratio r)))   = pure $ rational r
        specialize (Right (Number.SomeNumber (Number.Decimal d))) = pure $ float d
        pair = (left, right)

  liftComparison comparator left right
    | (Integer (Number.Integer i), Integer (Number.Integer j)) <- pair = go i j
    | (Integer (Number.Integer i), Float   (Number.Decimal j)) <- pair = go (fromIntegral i) j
    | (Float   (Number.Decimal i), Integer (Number.Integer j)) <- pair = go i                (fromIntegral j)
    | (Float   (Number.Decimal i), Float   (Number.Decimal j)) <- pair = go i j
    | (String  i,                  String  j)                  <- pair = go i j
    | (Boolean i,                  Boolean j)                  <- pair = go i j
    | (Unit,                       Unit)                       <- pair = pure $ boolean True
    | otherwise = throwValueError (ComparisonError left right)
      where
        -- Explicit type signature is necessary here because we're passing all sorts of things
        -- to these comparison functions.
        go :: (AbstractValue location (Value location body) effects, Ord a) => a -> a -> Evaluator location (Value location body) effects (Value location body)
        go l r = case comparator of
          Concrete f  -> pure $ boolean (f l r)
          Generalized -> pure $ integer (orderingToInt (compare l r))

        -- Map from [LT, EQ, GT] to [-1, 0, 1]
        orderingToInt :: Ordering -> Prelude.Integer
        orderingToInt = toInteger . pred . fromEnum

        pair = (left, right)


  liftBitwise operator target
    | Integer (Number.Integer i) <- target = pure . integer $ operator i
    | otherwise = throwValueError (BitwiseError target)

  liftBitwise2 operator left right
    | (Integer (Number.Integer i), Integer (Number.Integer j)) <- pair = pure . integer $ operator i j
    | otherwise = throwValueError (Bitwise2Error left right)
      where pair = (left, right)

  loop x = catchLoopControl (fix x) (\ control -> case control of
    Break value -> pure value
    -- FIXME: Figure out how to deal with this. Ruby treats this as the result of the current block iteration, while PHP specifies a breakout level and TypeScript appears to take a label.
    Continue _  -> loop x)


-- | The type of exceptions that can be thrown when constructing values in 'Value'’s 'MonadValue' instance.
data ValueError location body resume where
  StringError            :: Value location body                        -> ValueError location body ByteString
  BoolError              :: Value location body                        -> ValueError location body Bool
  IndexError             :: Value location body -> Value location body -> ValueError location body (Value location body)
  NamespaceError         :: Prelude.String                             -> ValueError location body (Environment location)
  CallError              :: Value location body                        -> ValueError location body (Value location body)
  NumericError           :: Value location body                        -> ValueError location body (Value location body)
  Numeric2Error          :: Value location body -> Value location body -> ValueError location body (Value location body)
  ComparisonError        :: Value location body -> Value location body -> ValueError location body (Value location body)
  BitwiseError           :: Value location body                        -> ValueError location body (Value location body)
  Bitwise2Error          :: Value location body -> Value location body -> ValueError location body (Value location body)
  KeyValueError          :: Value location body                        -> ValueError location body (Value location body, Value location body)
  -- Indicates that we encountered an arithmetic exception inside Haskell-native number crunching.
  ArithmeticError        :: ArithException                             -> ValueError location body (Value location body)
  -- Out-of-bounds error
  BoundsError            :: [Value location body] -> Prelude.Integer   -> ValueError location body (Value location body)


instance Eq location => Eq1 (ValueError location body) where
  liftEq _ (StringError a) (StringError b)                       = a == b
  liftEq _ (NamespaceError a) (NamespaceError b)                 = a == b
  liftEq _ (CallError a) (CallError b)                           = a == b
  liftEq _ (BoolError a) (BoolError c)                           = a == c
  liftEq _ (IndexError a b) (IndexError c d)                     = (a == c) && (b == d)
  liftEq _ (Numeric2Error a b) (Numeric2Error c d)               = (a == c) && (b == d)
  liftEq _ (ComparisonError a b) (ComparisonError c d)           = (a == c) && (b == d)
  liftEq _ (Bitwise2Error a b) (Bitwise2Error c d)               = (a == c) && (b == d)
  liftEq _ (BitwiseError a) (BitwiseError b)                     = a == b
  liftEq _ (KeyValueError a) (KeyValueError b)                   = a == b
  liftEq _ (BoundsError a b) (BoundsError c d)                   = (a == c) && (b == d)
  liftEq _ _             _                                       = False

deriving instance Show location => Show (ValueError location body resume)
instance Show location => Show1 (ValueError location body) where
  liftShowsPrec _ _ = showsPrec

throwValueError :: Member (Resumable (ValueError location body)) effects => ValueError location body resume -> Evaluator location (Value location body) effects resume
throwValueError = throwResumable

runValueError :: Effectful (m location (Value location body)) => m location (Value location body) (Resumable (ValueError location body) ': effects) a -> m location (Value location body) effects (Either (SomeExc (ValueError location body)) a)
runValueError = runResumable

runValueErrorWith :: Effectful (m location (Value location body)) => (forall resume . ValueError location body resume -> m location (Value location body) effects resume) -> m location (Value location body) (Resumable (ValueError location body) ': effects) a -> m location (Value location body) effects a
runValueErrorWith = runResumableWith
