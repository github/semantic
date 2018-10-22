{-# LANGUAGE DeriveAnyClass, GADTs, RankNTypes, TypeOperators, ScopedTypeVariables, UndecidableInstances, LambdaCase #-}
module Data.Abstract.Value.Concrete
  ( Value (..)
  , ValueError (..)
  , materializeEnvironment
  , runValueError
  , runValueErrorWith
  ) where

import qualified Control.Abstract as Abstract
import Control.Abstract hiding (Boolean(..), Function(..), While(..))
import Data.Abstract.BaseError
import Data.Abstract.Evaluatable (UnspecializedError(..))
import Data.Abstract.Environment (Environment, Bindings, EvalContext(..))
import qualified Data.Abstract.Environment as Env
import Data.Abstract.FreeVariables
import Data.Abstract.Name
import qualified Data.Abstract.Number as Number
import Data.Bits
import Data.List (genericIndex, genericLength)
import Data.Scientific (Scientific, coefficient, normalize)
import Data.Scientific.Exts
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Word
import Prologue hiding (catchError)

data Value term address
  = Closure PackageInfo ModuleInfo (Maybe Name) [Name] (Either BuiltIn term) (Environment address)
  | Unit
  | Boolean Bool
  | Integer  (Number.Number Integer)
  | Rational (Number.Number Rational)
  | Float    (Number.Number Scientific)
  | String Text
  | Symbol Text
  | Regex Text
  | Tuple [address]
  | Array [address]
  | Class Name [address] (Bindings address)
  | Namespace Name (Maybe address) (Bindings address)
  | KVPair (Value term address) (Value term address)
  | Hash [Value term address]
  | Null
  | Hole
  deriving (Eq, Ord, Show, Generic, NFData)


instance Ord address => ValueRoots address (Value term address) where
  valueRoots v
    | Closure _ _ _ _ _ env <- v = Env.addresses env
    | otherwise                  = mempty


instance ( FreeVariables term
         , Member (Allocator address) sig
         , Member (Deref (Value term address)) sig
         , Member (Env address) sig
         , Member (Error (Return address)) sig
         , Member Fresh sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader PackageInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (AddressError address (Value term address)))) sig
         , Member (Resumable (BaseError (ValueError term address))) sig
         , Member (State (Heap address (Value term address))) sig
         , Member Trace sig
         , Ord address
         , Carrier sig m
         , Show address
         , Show term
         )
      => Carrier (Abstract.Function term address (Value term address) :+: sig) (Abstract.FunctionC term address (Value term address) (Evaluator term address (Value term address) m)) where
  gen = FunctionC . const . gen
  alg op = FunctionC (\ eval -> (algF eval \/ (alg . handlePure (flip runFunctionC eval))) op)
    where algF eval = \case
            Abstract.Function name params body k -> do
              packageInfo <- currentPackage
              moduleInfo <- currentModule
              Closure packageInfo moduleInfo name params (Right body) <$> close (foldr Set.delete (freeVariables body) params) >>= flip runFunctionC eval . k
            Abstract.BuiltIn builtIn k -> do
              packageInfo <- currentPackage
              moduleInfo <- currentModule
              runFunctionC (k (Closure packageInfo moduleInfo Nothing [] (Left builtIn) lowerBound)) eval
            Abstract.Call op self params k -> do
              boxed <- case op of
                Closure _ _ _ _ (Left Print) _ -> traverse (deref >=> trace . show) params *> box Unit
                Closure _ _ _ _ (Left Show) _ -> deref self >>= box . String . pack . show
                Closure packageInfo moduleInfo _ names (Right body) env -> do
                  -- Evaluate the bindings and body with the closure’s package/module info in scope in order to
                  -- charge them to the closure's origin.
                  withCurrentPackage packageInfo . withCurrentModule moduleInfo $ do
                    bindings <- foldr (\ (name, addr) rest -> Env.insert name addr <$> rest) (pure lowerBound) (zip names params)
                    let fnCtx = EvalContext (Just self) (Env.push env)
                    withEvalContext fnCtx (catchReturn (bindAll bindings *> runFunction eval (eval body)))
                _ -> throwValueError (CallError op) >>= box
              runFunctionC (k boxed) eval


instance ( Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (ValueError term address))) sig
         , Carrier sig m
         )
      => Carrier (Abstract.Boolean (Value term address) :+: sig) (BooleanC (Evaluator term address (Value term address) m)) where
  gen = BooleanC . gen
  alg = BooleanC . (algB \/ (alg . handlePure runBooleanC))
    where algB :: Abstract.Boolean (Value term address) (BooleanC (Evaluator term address (Value term address) m)) (BooleanC (Evaluator term address (Value term address) m) a) -> Evaluator term address (Value term address) m a
          algB = \case
            Abstract.Boolean b          k -> runBooleanC . k $! Boolean b
            Abstract.AsBool (Boolean b) k -> runBooleanC (k b)
            Abstract.AsBool other       k -> (throwValueError $! BoolError other) >>= runBooleanC . k
            Abstract.Disjunction a b    k -> do
              a' <- runBooleanC a
              a'' <- runBoolean (asBool a')
              if a'' then
                runBooleanC (k a')
              else
                runBooleanC b >>= runBooleanC . k


-- instance ( Carrier sig m
--          , Member (Deref (Value term address)) sig
--          , Member (Abstract.Boolean (Value term address)) sig
--          , Member (Error (LoopControl address)) sig
--          , Member (Reader ModuleInfo) sig
--          , Member (Reader Span) sig
--          , Member (Resumable (BaseError (AddressError address (Value term address)))) sig
--          , Member (Resumable (BaseError (ValueError term address))) sig
--          , Member (Resumable (BaseError (UnspecializedError (Value term address)))) sig
--          , Member (State (Heap address (Value term address))) sig
--          , Ord address
--          , Show address
--          , Show term
--          )
--       => Carrier (Abstract.While (Value term address) :+: sig) (WhileC (Evaluator term address (Value term address) (InterposeC (Resumable (BaseError (UnspecializedError (Value term address)))) (Evaluator term address (Value term address) m)))) where
--   gen = WhileC . gen
--   alg = WhileC . (algW \/ (alg . handlePure runWhileC))
--     where algW = \case
--             Abstract.While cond body k -> interpose @(Resumable (BaseError (UnspecializedError (Value term address))))
--                   (\(Resumable (BaseError _ _ (UnspecializedError _)) k) -> throwAbort) (runEvaluator (loop (\continue -> do
--               cond' <- runWhileC cond
--
--               -- `interpose` is used to handle 'UnspecializedError's and abort out of the
--               -- loop, otherwise under concrete semantics we run the risk of the
--               -- conditional always being true and getting stuck in an infinite loop.
--
--               ifthenelse cond' (runWhileC body *> continue) (pure unit)))) >>= runWhileC . k
--             where
--               loop x = catchLoopControl (fix x) $ \case
--                 Break value -> deref value
--                 Abort -> pure unit
--                 -- FIXME: Figure out how to deal with this. Ruby treats this as the result
--                 -- of the current block iteration, while PHP specifies a breakout level
--                 -- and TypeScript appears to take a label.
--                 Continue _  -> loop x


interpose :: (Member eff sig, HFunctor eff, Carrier sig m)
          => (forall v. eff m (m v) -> m v)
          -> Eff (InterposeC eff m) a
          -> m a
interpose handler = runInterposeC handler . interpret

newtype InterposeC eff m a = InterposeC ((forall x . eff m (m x) -> m x) -> m a)

runInterposeC :: (forall x . eff m (m x) -> m x) -> InterposeC eff m a -> m a
runInterposeC f (InterposeC m) = m f

instance (Member eff sig, HFunctor eff, Carrier sig m) => Carrier sig (InterposeC eff m) where
  gen a = InterposeC (const (gen a))
  alg op
    | Just e <- prj op = InterposeC (\ handler -> handler (handlePure (runInterposeC handler) e))
    | otherwise        = InterposeC (\ handler -> alg (handlePure (runInterposeC handler) op))


instance AbstractHole (Value term address) where
  hole = Hole

instance (Show address, Show term) => AbstractIntro (Value term address) where
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

materializeEnvironment :: ( Member (Deref (Value term address)) sig
                          , Member (Reader ModuleInfo) sig
                          , Member (Reader Span) sig
                          , Member (Resumable (BaseError (AddressError address (Value term address)))) sig
                          , Member (State (Heap address (Value term address))) sig
                          , Ord address
                          , Carrier sig m
                          )
                       => Value term address
                       -> Evaluator term address (Value term address) m (Maybe (Environment address))
materializeEnvironment val = do
  ancestors <- rec val
  pure (Env.Environment <$> nonEmpty ancestors)
    where
      rec val = do
        supers <- concat <$> traverse (deref >=> rec) (parents val)
        pure . maybe [] (: supers) $ bindsFrom val

      bindsFrom = \case
        Class _ _ binds -> Just binds
        Namespace _ _ binds -> Just binds
        _ -> Nothing

      parents = \case
        Class _ supers _ -> supers
        Namespace _ supers _ -> toList supers
        _ -> []

-- | Construct a 'Value' wrapping the value arguments (if any).
instance ( Member (Allocator address) sig
         , Member (Abstract.Boolean (Value term address)) sig
         , Member (Deref (Value term address)) sig
         , Member (Env address) sig
         , Member (Error (LoopControl address)) sig
         , Member (Error (Return address)) sig
         , Member Fresh sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader PackageInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (ValueError term address))) sig
         , Member (Resumable (BaseError (AddressError address (Value term address)))) sig
         , Member (State (Heap address (Value term address))) sig
         , Member Trace sig
         , Ord address
         , Show address
         , Show term
         , Carrier sig m
         )
      => AbstractValue term address (Value term address) m where
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

  namespace name super binds = do
    maybeNs <- lookupEnv name >>= traverse deref
    binds' <- maybe (pure lowerBound) asNamespaceBinds maybeNs
    let super' = (maybeNs >>= asNamespaceSuper) <|> super
    pure (Namespace name super' (binds <> binds'))
    where
      asNamespaceSuper = \case
        Namespace _ super _ -> super
        _ -> Nothing
      asNamespaceBinds v
        | Namespace _ _ binds' <- v = pure binds'
        | otherwise                 = throwValueError $ NamespaceError ("expected " <> show v <> " to be a namespace")

  scopedEnvironment = deref >=> materializeEnvironment

  asString v
    | String n <- v = pure n
    | otherwise     = throwValueError $ StringError v


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
        specialize :: AbstractValue term address (Value term address) m
                   => Either ArithException Number.SomeNumber
                   -> Evaluator term address (Value term address) m (Value term address)
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
        go :: (AbstractValue term address (Value term address) m, Ord a) => a -> a -> Evaluator term address (Value term address) m (Value term address)
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

  castToInteger (Integer (Number.Integer i)) = pure (Integer (Number.Integer i))
  castToInteger (Float (Number.Decimal i)) = pure (Integer (Number.Integer (coefficient (normalize i))))
  castToInteger i = throwValueError (NumericError i)

-- | The type of exceptions that can be thrown when constructing values in 'Value'’s 'MonadValue' instance.
data ValueError term address resume where
  StringError            :: Value term address                       -> ValueError term address Text
  BoolError              :: Value term address                       -> ValueError term address Bool
  IndexError             :: Value term address -> Value term address -> ValueError term address (Value term address)
  NamespaceError         :: Prelude.String                           -> ValueError term address (Bindings address)
  CallError              :: Value term address                       -> ValueError term address (Value term address)
  NumericError           :: Value term address                       -> ValueError term address (Value term address)
  Numeric2Error          :: Value term address -> Value term address -> ValueError term address (Value term address)
  ComparisonError        :: Value term address -> Value term address -> ValueError term address (Value term address)
  BitwiseError           :: Value term address                       -> ValueError term address (Value term address)
  Bitwise2Error          :: Value term address -> Value term address -> ValueError term address (Value term address)
  KeyValueError          :: Value term address                       -> ValueError term address (Value term address, Value term address)
  ArrayError             :: Value term address                       -> ValueError term address [address]
  -- Indicates that we encountered an arithmetic exception inside Haskell-native number crunching.
  ArithmeticError        :: ArithException                           -> ValueError term address (Value term address)
  -- Out-of-bounds error
  BoundsError            :: [address]          -> Prelude.Integer    -> ValueError term address (Value term address)

instance (NFData term, NFData address) => NFData1 (ValueError term address) where
  liftRnf _ x = case x of
    StringError i       -> rnf i
    BoolError   i       -> rnf i
    IndexError  i j     -> rnf i `seq` rnf j
    NamespaceError i    -> rnf i
    CallError i         -> rnf i
    NumericError i      -> rnf i
    Numeric2Error i j   -> rnf i `seq` rnf j
    ComparisonError i j -> rnf i `seq` rnf j
    BitwiseError i      -> rnf i
    Bitwise2Error i j   -> rnf i `seq` rnf j
    KeyValueError i     -> rnf i
    ArrayError i        -> rnf i
    ArithmeticError i   -> i `seq` ()
    BoundsError i j     -> rnf i `seq` rnf j

instance (NFData term, NFData address, NFData resume) => NFData (ValueError term address resume) where
  rnf = liftRnf rnf

instance (Eq address, Eq term) => Eq1 (ValueError term address) where
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

deriving instance (Show address, Show term) => Show (ValueError term address resume)
instance (Show address, Show term) => Show1 (ValueError term address) where
  liftShowsPrec _ _ = showsPrec

runValueError :: (Carrier sig m, Effect sig)
              => Evaluator term address (Value term address) (ResumableC (BaseError (ValueError term address)) (Evaluator term address (Value term address) m)) a
              -> Evaluator term address (Value term address) m (Either (SomeError (BaseError (ValueError term address))) a)
runValueError = runResumable . runEvaluator

runValueErrorWith :: Carrier sig m
                  => (forall resume . BaseError (ValueError term address) resume -> Evaluator term address (Value term address) m resume)
                  -> Evaluator term address (Value term address) (ResumableWithC (BaseError (ValueError term address)) (Evaluator term address (Value term address) m)) a
                  -> Evaluator term address (Value term address) m a
runValueErrorWith f = runResumableWith f . runEvaluator

throwValueError :: ( Member (Resumable (BaseError (ValueError term address))) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Span) sig
                   , Carrier sig m
                   )
                => ValueError term address resume
                -> Evaluator term address (Value term address) m resume
throwValueError = throwBaseError
