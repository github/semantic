{-# LANGUAGE GADTs, RankNTypes, TypeOperators, UndecidableInstances, LambdaCase #-}
module Data.Abstract.Value.Concrete where

import Control.Abstract
import Data.Abstract.Environment (Environment, Bindings)
import qualified Data.Abstract.Environment as Env
import Data.Abstract.Name
import qualified Data.Abstract.Number as Number
import Data.Coerce
import Data.List (genericIndex, genericLength)
import Data.Scientific (Scientific)
import Data.Scientific.Exts
import qualified Data.Set as Set
import Prologue

data Value address body
  = Closure PackageInfo ModuleInfo [Name] (ClosureBody address body) (Environment address)
  | Unit
  | Boolean Bool
  | Integer  (Number.Number Integer)
  | Rational (Number.Number Rational)
  | Float    (Number.Number Scientific)
  | String Text
  | Symbol Text
  | Tuple [address]
  | Array [address]
  | Class Name [address] (Bindings address)
  | Namespace Name (Environment address)
  | KVPair (Value address body) (Value address body)
  | Hash [Value address body]
  | Null
  | Hole
  deriving (Eq, Ord, Show)

data ClosureBody address body = ClosureBody { closureBodyId :: Int, closureBody :: body address }

instance Eq   (ClosureBody address body) where
  (==) = (==) `on` closureBodyId

instance Ord  (ClosureBody address body) where
  compare = compare `on` closureBodyId

instance Show (ClosureBody address body) where
  showsPrec d (ClosureBody i _) = showsUnaryWith showsPrec "ClosureBody" d i


instance Ord address => ValueRoots address (Value address body) where
  valueRoots v
    | Closure _ _ _ _ env <- v = Env.addresses env
    | otherwise                = mempty


instance AbstractHole (Value address body) where
  hole = Hole

instance ( Coercible body (Eff effects)
         , Member (Allocator address (Value address body)) effects
         , Member (Env address) effects
         , Member Fresh effects
         , Member (Reader ModuleInfo) effects
         , Member (Reader PackageInfo) effects
         , Member (Resumable (ValueError address body)) effects
         , Member (Exc (Return address)) effects
         , Show address
         )
      => AbstractFunction address (Value address body) effects where
  closure parameters freeVariables body = do
    packageInfo <- currentPackage
    moduleInfo <- currentModule
    i <- fresh
    Closure packageInfo moduleInfo parameters (ClosureBody i (coerce (lowerEff body))) <$> close (foldr Set.delete freeVariables parameters)

  call op params = do
    case op of
      Closure packageInfo moduleInfo names (ClosureBody _ body) env -> do
        -- Evaluate the bindings and body with the closure’s package/module info in scope in order to
        -- charge them to the closure's origin.
        withCurrentPackage packageInfo . withCurrentModule moduleInfo $ do
          bindings <- foldr (\ (name, param) rest -> do
            addr <- param
            Env.insert name addr <$> rest) (pure env) (zip names params)
          locally (catchReturn (bindAll bindings *> raiseEff (coerce body)))
      _ -> box =<< throwValueError (CallError op)


instance Show address => AbstractIntro (Value address body) where
  unit     = Unit
  integer  = Integer . Number.Integer
  boolean  = Boolean
  string   = String
  float    = Float . Number.Decimal
  symbol   = Symbol
  rational = Rational . Number.Ratio

  kvPair = KVPair
  hash = Hash . map (uncurry KVPair)

  null     = Null


-- | Construct a 'Value' wrapping the value arguments (if any).
instance ( Coercible body (Eff effects)
         , Member (Allocator address (Value address body)) effects
         , Member (Env address) effects
         , Member (Exc (LoopControl address)) effects
         , Member (Exc (Return address)) effects
         , Member Fresh effects
         , Member (Reader ModuleInfo) effects
         , Member (Reader PackageInfo) effects
         , Member (Resumable (ValueError address body)) effects
         , Show address
         )
      => AbstractValue address (Value address body) effects where
  asPair val
    | KVPair k v <- val = pure (k, v)
    | otherwise = throwValueError $ KeyValueError val

  tuple = pure . Tuple
  array = pure . Array

  klass n supers binds = do
    pure $ Class n supers binds

  namespace name env = do
    maybeAddr <- lookupEnv name
    env' <- maybe (pure lowerBound) (asNamespaceEnv <=< deref) maybeAddr
    pure (Namespace name (Env.mergeNewer env' env))
    where asNamespaceEnv v
            | Namespace _ env' <- v = pure env'
            | otherwise             = throwValueError $ NamespaceError ("expected " <> show v <> " to be a namespace")

  scopedEnvironment ptr = do
    ancestors <- ancestorBinds [ptr]
    pure (Env.Environment <$> nonEmpty ancestors)
      where ancestorBinds = (pure . concat) <=< traverse (deref >=> \case
                Class _ supers binds -> (binds :) <$> ancestorBinds (reverse supers)
                Namespace _ env -> pure . toList . Env.unEnvironment $ env
                _ -> pure [])

  asString v
    | String n <- v = pure n
    | otherwise     = throwValueError $ StringError v

  ifthenelse cond if' else' = do
    bool <- case cond of { Boolean b -> pure b ; _ -> throwValueError (BoolError cond) }
    if bool then if' else else'

  disjunction a b = do
    a' <- a
    ifthenelse a' (pure a') b


  index = go where
    tryIdx list ii
      | ii > genericLength list = box =<< throwValueError (BoundsError list ii)
      | otherwise               = pure (genericIndex list ii)
    go arr idx
      | (Array arr, Integer (Number.Integer i)) <- (arr, idx) = tryIdx arr i
      | (Tuple tup, Integer (Number.Integer i)) <- (arr, idx) = tryIdx tup i
      | otherwise = box =<< throwValueError (IndexError arr idx)

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
        specialize :: (AbstractValue address (Value address body) effects, Member (Resumable (ValueError address body)) effects) => Either ArithException Number.SomeNumber -> Evaluator address (Value address body) effects (Value address body)
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
        go :: (AbstractValue address (Value address body) effects, Ord a) => a -> a -> Evaluator address (Value address body) effects (Value address body)
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
    Break value -> deref value
    -- FIXME: Figure out how to deal with this. Ruby treats this as the result of the current block iteration, while PHP specifies a breakout level and TypeScript appears to take a label.
    Continue _  -> loop x)


-- | The type of exceptions that can be thrown when constructing values in 'Value'’s 'MonadValue' instance.
data ValueError address body resume where
  StringError            :: Value address body                       -> ValueError address body Text
  BoolError              :: Value address body                       -> ValueError address body Bool
  IndexError             :: Value address body -> Value address body -> ValueError address body (Value address body)
  NamespaceError         :: Prelude.String                           -> ValueError address body (Environment address)
  CallError              :: Value address body                       -> ValueError address body (Value address body)
  NumericError           :: Value address body                       -> ValueError address body (Value address body)
  Numeric2Error          :: Value address body -> Value address body -> ValueError address body (Value address body)
  ComparisonError        :: Value address body -> Value address body -> ValueError address body (Value address body)
  BitwiseError           :: Value address body                       -> ValueError address body (Value address body)
  Bitwise2Error          :: Value address body -> Value address body -> ValueError address body (Value address body)
  KeyValueError          :: Value address body                       -> ValueError address body (Value address body, Value address body)
  -- Indicates that we encountered an arithmetic exception inside Haskell-native number crunching.
  ArithmeticError        :: ArithException                           -> ValueError address body (Value address body)
  -- Out-of-bounds error
  BoundsError            :: [address]          -> Prelude.Integer    -> ValueError address body (Value address body)


instance Eq address => Eq1 (ValueError address body) where
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

deriving instance Show address => Show (ValueError address body resume)
instance Show address => Show1 (ValueError address body) where
  liftShowsPrec _ _ = showsPrec

throwValueError :: Member (Resumable (ValueError address body)) effects => ValueError address body resume -> Evaluator address (Value address body) effects resume
throwValueError = throwResumable

runValueError :: (Effectful (m address (Value address body)), Effects effects) => m address (Value address body) (Resumable (ValueError address body) ': effects) a -> m address (Value address body) effects (Either (SomeExc (ValueError address body)) a)
runValueError = runResumable

runValueErrorWith :: (Effectful (m address (Value address body)), Effects effects) => (forall resume . ValueError address body resume -> m address (Value address body) effects resume) -> m address (Value address body) (Resumable (ValueError address body) ': effects) a -> m address (Value address body) effects a
runValueErrorWith = runResumableWith
