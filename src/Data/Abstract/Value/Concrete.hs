{-# LANGUAGE GADTs, RankNTypes, TypeOperators, UndecidableInstances, LambdaCase, ScopedTypeVariables #-}
module Data.Abstract.Value.Concrete
  ( Value (..)
  , ValueError (..)
  , ClosureBody (..)
  , runFunction
  , runBoolean
  , runValueError
  , runValueErrorWith
  ) where

import Control.Abstract.ScopeGraph (Allocator)
import qualified Control.Abstract as Abstract
import Control.Abstract hiding (Boolean(..), Function(..))
import Data.Abstract.BaseError
import Data.Abstract.Environment (Environment, Bindings, EvalContext(..))
import qualified Data.Abstract.Environment as Env
import Data.Abstract.Name
import qualified Data.Abstract.Number as Number
import Data.Bits
import Data.Coerce
import Data.List (genericIndex, genericLength)
import Data.Scientific (Scientific, coefficient, normalize)
import Data.Scientific.Exts
import qualified Data.Set as Set
import Data.Word
import Prologue
import qualified Data.Map.Strict as Map

data Value address body
  = Closure PackageInfo ModuleInfo Name [Name] (ClosureBody (Value address body) body) address
  | Unit
  | Boolean Bool
  | Integer  (Number.Number Integer)
  | Rational (Number.Number Rational)
  | Float    (Number.Number Scientific)
  | String Text
  | Symbol Text
  | Regex Text
  | Tuple [(Value address body)]
  | Array [(Value address body)]
  | Class Declaration [(Value address body)] address
  | Namespace Name (Maybe address) (Bindings address)
  | KVPair (Value address body) (Value address body)
  | Hash [Value address body]
  | Null
  | Hole
  deriving (Eq, Ord, Show)

data ClosureBody value body = ClosureBody { closureBodyId :: Int, closureBody :: body value }

instance Eq   (ClosureBody address body) where
  (==) = (==) `on` closureBodyId

instance Ord  (ClosureBody address body) where
  compare = compare `on` closureBodyId

instance Show (ClosureBody address body) where
  showsPrec d (ClosureBody i _) = showsUnaryWith showsPrec "ClosureBody" d i


instance Ord address => ValueRoots address (Value address body) where
  valueRoots v
    | Closure _ _ _ _ _ env <- v = undefined -- Env.addresses env
    | otherwise                = mempty


runFunction :: forall address effects body a. ( Member (Allocator address) effects
               , Member (Deref (Value address body)) effects
               , Member (Exc (Return (Value address body))) effects
               , Member Fresh effects
               , Member (Reader ModuleInfo) effects
               , Member (Reader PackageInfo) effects
               , Member (Reader Span) effects
               , Member (State Span) effects
               , Member (Resumable (BaseError (AddressError address (Value address body)))) effects
               , Member (Resumable (BaseError (ValueError address body))) effects
               , Member (Resumable (BaseError (ScopeError address))) effects
               , Member (Resumable (BaseError (HeapError address))) effects
               , Member (State (Heap address address (Value address body))) effects
               , Member (State (ScopeGraph address)) effects
               , Ord address
               , PureEffects effects
               )
            => (body (Value address body) -> Evaluator address (Value address body) (Abstract.Function address (Value address body) ': effects) (Value address body))
            -> (Evaluator address (Value address body) (Abstract.Function address (Value address body) ': effects) (Value address body) -> body (Value address body))
            -> Evaluator address (Value address body) (Abstract.Function address (Value address body) ': effects) a
            -> Evaluator address (Value address body) effects a
runFunction toEvaluator fromEvaluator = interpret $ \case
  Abstract.Function name params fvs body -> do
    packageInfo <- currentPackage
    moduleInfo <- currentModule
    i <- fresh
    -- TODO: Declare all params
    span <- get @Span
    declare (Declaration name) span Nothing

    currentScope' <- currentScope
    let lexicalEdges = Map.singleton Lexical [ currentScope' ]
    scope <- newScope lexicalEdges

    withScope scope $ do
      for_ params $ \name -> do
        span <- get @Span -- TODO: This is definitely wrong
        declare (Declaration name) span Nothing

    let closure = (Closure packageInfo moduleInfo name params (ClosureBody i (fromEvaluator (Evaluator body))) scope)
    address <- lookupDeclaration (Declaration name)
    assign address closure
    pure closure
  Abstract.Call op self params -> do
    case op of
      Closure packageInfo moduleInfo _ names (ClosureBody _ body) scope -> do
        -- Evaluate the bindings and body with the closure’s package/module info in scope in order to
        -- charge them to the closure's origin.
        withCurrentPackage packageInfo . withCurrentModule moduleInfo $ do
          currentScope' <- currentScope
          currentFrame' <- currentFrame
          let frameEdges = Map.singleton Lexical (Map.singleton currentScope' currentFrame')
          frameAddress <- newFrame scope frameEdges
          withFrame frameAddress $ do
            for_ (zip names params) $ \(name, param) -> do
              addr <- lookupDeclaration (Declaration name)
              assign addr param
            catchReturn (runFunction toEvaluator fromEvaluator (toEvaluator body))
      _ -> throwValueError (CallError op)

runBoolean :: ( Member (Reader ModuleInfo) effects
              , Member (Reader Span) effects
              , Member (Resumable (BaseError (ValueError address body))) effects
              , PureEffects effects
              )
           => Evaluator address (Value address body) (Abstract.Boolean (Value address body) ': effects) a
           -> Evaluator address (Value address body) effects a
runBoolean = interpret $ \case
  Abstract.Boolean b          -> pure $! Boolean b
  Abstract.AsBool (Boolean b) -> pure b
  Abstract.AsBool other       -> throwValueError $! BoolError other
  Abstract.Disjunction a b    -> do
    a' <- runBoolean (Evaluator a)
    a'' <- runBoolean (asBool a')
    if a'' then pure a' else runBoolean (Evaluator b)


instance AbstractHole (Value address body) where
  hole = Hole

instance Show address => AbstractIntro (Value address body) where
  unit     = Unit
  integer  = Integer . Number.Integer
  string   = String
  float    = Float . Number.Decimal
  symbol   = Symbol
  rational = Rational . Number.Ratio
  regex    = Regex

  kvPair = KVPair
  hash = Hash . map (uncurry KVPair)

  null     = Null

-- materializeEnvironment :: ( Member (Deref (Value address body)) effects
--                           , Member (Reader ModuleInfo) effects
--                           , Member (Reader Span) effects
--                           , Member (Resumable (BaseError (AddressError address (Value address body)))) effects
--                           , Member (State (Heap address address (Value address body))) effects
--                           , Ord address
--                           )
--                        => Value address body
--                        -> Evaluator address (Value address body) effects (Maybe (Environment address))
-- materializeEnvironment val = do
--   ancestors <- rec val
--   pure (Env.Environment <$> nonEmpty ancestors)
--     where
--       rec val = do
--         supers <- concat <$> traverse (deref >=> rec) (parents val)
--         pure . maybe [] (: supers) $ bindsFrom val

--       bindsFrom = \case
--         Class _ _ binds -> Just binds
--         Namespace _ _ binds -> Just binds
--         _ -> Nothing

--       parents = \case
--         Class _ supers _ -> supers
--         Namespace _ supers _ -> toList supers
--         _ -> []

-- | Construct a 'Value' wrapping the value arguments (if any).
instance ( Coercible body (Eff effects)
         , Member (Allocator address) effects
         , Member (Abstract.Boolean (Value address body)) effects
         , Member (Deref (Value address body)) effects
         , Member (Exc (LoopControl (Value address body))) effects
         , Member (Exc (Return (Value address body))) effects
         , Member Fresh effects
         , Member (Reader ModuleInfo) effects
         , Member (Reader PackageInfo) effects
         , Member (Reader Span) effects
         , Member (Resumable (BaseError (ValueError address body))) effects
         , Member (Resumable (BaseError (AddressError address (Value address body)))) effects
         , Member (State (Heap address address (Value address body))) effects
         , Member Trace effects
         , Ord address
         , Show address
         )
      => AbstractValue address (Value address body) effects where
  asPair val
    | KVPair k v <- val = pure (k, v)
    | otherwise = throwValueError $ KeyValueError val

  tuple = pure . Tuple
  array = pure . Array

  asArray val
    | Array addresses <- val = pure addresses
    | otherwise = throwValueError $ ArrayError val

  klass n supers binds = do
    pure $ Class n supers binds

  namespace name super binds = undefined -- do
    -- maybeNs <- lookupEnv name >>= traverse deref
    -- binds' <- maybe (pure lowerBound) asNamespaceBinds maybeNs
    -- let super' = (maybeNs >>= asNamespaceSuper) <|> super
    -- pure (Namespace name super' (binds <> binds'))
    -- where
    --   asNamespaceSuper = \case
    --     Namespace _ super _ -> super
    --     _ -> Nothing
    --   asNamespaceBinds v
    --     | Namespace _ _ binds' <- v = pure binds'
    --     | otherwise                 = throwValueError $ NamespaceError ("expected " <> show v <> " to be a namespace")

  -- scopedEnvironment = deref >=> materializeEnvironment

  asString v
    | String n <- v = pure n
    | otherwise     = throwValueError $ StringError v


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
        specialize :: ( AbstractValue address (Value address body) effects
                      , Member (Reader ModuleInfo) effects
                      , Member (Reader Span) effects
                      , Member (Resumable (BaseError (ValueError address body))) effects
                      )
                   => Either ArithException Number.SomeNumber
                   -> Evaluator address (Value address body) effects (Value address body)
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
    | (Unit,                       Unit)                       <- pair = boolean True
    | otherwise = throwValueError (ComparisonError left right)
      where
        -- Explicit type signature is necessary here because we're passing all sorts of things
        -- to these comparison functions.
        go :: (AbstractValue address (Value address body) effects, Member (Abstract.Boolean (Value address body)) effects, Ord a) => a -> a -> Evaluator address (Value address body) effects (Value address body)
        go l r = case comparator of
          Concrete f  -> boolean (f l r)
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

  unsignedRShift left right
    | (Integer (Number.Integer i), Integer (Number.Integer j)) <- pair =
      if i >= 0 then pure . integer $ ourShift (fromIntegral i) (fromIntegral j)
      else throwValueError (Bitwise2Error left right)
    | otherwise = throwValueError (Bitwise2Error left right)
      where
        pair = (left, right)
        ourShift :: Word64 -> Int -> Integer
        ourShift a b = toInteger (shiftR a b)

  loop x = catchLoopControl (fix x) (\ control -> case control of
    Break value -> pure value
    -- FIXME: Figure out how to deal with this. Ruby treats this as the result of the current block iteration, while PHP specifies a breakout level and TypeScript appears to take a label.
    Continue _  -> loop x)

  castToInteger (Integer (Number.Integer i)) = pure (Integer (Number.Integer i))
  castToInteger (Float (Number.Decimal i)) = pure (Integer (Number.Integer (coefficient (normalize i))))
  castToInteger i = throwValueError (NumericError i)

-- | The type of exceptions that can be thrown when constructing values in 'Value'’s 'MonadValue' instance.
data ValueError address body resume where
  StringError            :: Value address body                       -> ValueError address body Text
  BoolError              :: Value address body                       -> ValueError address body Bool
  IndexError             :: Value address body -> Value address body -> ValueError address body (Value address body)
  NamespaceError         :: Prelude.String                           -> ValueError address body (Bindings address)
  CallError              :: Value address body                       -> ValueError address body (Value address body)
  NumericError           :: Value address body                       -> ValueError address body (Value address body)
  Numeric2Error          :: Value address body -> Value address body -> ValueError address body (Value address body)
  ComparisonError        :: Value address body -> Value address body -> ValueError address body (Value address body)
  BitwiseError           :: Value address body                       -> ValueError address body (Value address body)
  Bitwise2Error          :: Value address body -> Value address body -> ValueError address body (Value address body)
  KeyValueError          :: Value address body                       -> ValueError address body (Value address body, Value address body)
  ArrayError             :: Value address body                       -> ValueError address body [(Value address body)]
  -- Indicates that we encountered an arithmetic exception inside Haskell-native number crunching.
  ArithmeticError        :: ArithException                           -> ValueError address body (Value address body)
  -- Out-of-bounds error
  BoundsError            :: [Value address body]          -> Prelude.Integer    -> ValueError address body (Value address body)


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

runValueError :: (Effectful (m address (Value address body)), Effects effects)
              => m address (Value address body) (Resumable (BaseError (ValueError address body)) ': effects) a
              -> m address (Value address body) effects (Either (SomeExc (BaseError (ValueError address body))) a)
runValueError = runResumable

runValueErrorWith :: (Effectful (m address (Value address body)), Effects effects)
                  => (forall resume . BaseError (ValueError address body) resume -> m address (Value address body) effects resume)
                  -> m address (Value address body) (Resumable (BaseError (ValueError address body)) ': effects) a
                  -> m address (Value address body) effects a
runValueErrorWith = runResumableWith

throwValueError :: ( Member (Resumable (BaseError (ValueError address body))) effects
                   , Member (Reader ModuleInfo) effects
                   , Member (Reader Span) effects
                   )
                => ValueError address body resume
                -> Evaluator address (Value address body) effects resume
throwValueError = throwBaseError
