{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, LambdaCase, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Value.Concrete
  ( Value (..)
  , ValueError (..)
  , runValueError
  , runValueErrorWith
  ) where

import Control.Abstract.ScopeGraph (Allocator, ScopeError)
import Control.Abstract.Heap (scopeLookup)
import qualified Control.Abstract as Abstract
import Control.Abstract hiding (Boolean(..), Function(..), Numeric(..), Object(..), Array(..), Hash(..), String(..), Unit(..), While(..))
import Control.Effect.Carrier
import Control.Effect.Interpose
import Data.Abstract.BaseError
import Data.Abstract.Evaluatable (UnspecializedError(..), EvalError(..), Declarations)
import Data.Abstract.FreeVariables
import Data.Abstract.Name
import qualified Data.Abstract.Number as Number
import Data.Bits
import Data.List (genericIndex, genericLength)
import Data.Scientific (Scientific, coefficient, normalize)
import Data.Scientific.Exts
import Data.Text (pack)
import Data.Word
import Prologue
import qualified Data.Map.Strict as Map

data Value term address
    -- TODO: Split Closure up into a separate data type.                                                  Scope   Frame
  = Closure PackageInfo ModuleInfo (Maybe Name) (Maybe (Value term address)) [Name] (Either BuiltIn term) address address
  | Unit
  | Boolean Bool
  | Integer  (Number.Number Integer)
  | Rational (Number.Number Rational)
  | Float    (Number.Number Scientific)
  | String Text
  | Tuple [Value term address]
  | Array [Value term address]
  | Class Declaration [Value term address] address
  | Object address
  | Namespace Name address
  | KVPair (Value term address) (Value term address)
  | Hash [Value term address]
  | Null
  | Hole
  deriving (Eq, Ord, Show)


instance ValueRoots address (Value term address) where
  valueRoots _ = lowerBound


instance ( FreeVariables term
         , Member (Allocator address) sig
         , Member (Deref (Value term address)) sig
         , Member Fresh sig
         , Member (Reader (CurrentFrame address)) sig
         , Member (Reader (CurrentScope address)) sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader PackageInfo) sig
         , Member (Reader Span) sig
         , Member (State Span) sig
         , Member (State (ScopeGraph address)) sig
         , Member (Resumable (BaseError (AddressError address (Value term address)))) sig
         , Member (Resumable (BaseError (EvalError term address (Value term address)))) sig
         , Member (Resumable (BaseError (ValueError term address))) sig
         , Member (Resumable (BaseError (HeapError address))) sig
         , Member (Resumable (BaseError (ScopeError address))) sig
         , Member (State (Heap address address (Value term address))) sig
         , Member (Error (Return (Value term address))) sig
         , Declarations term
         , Member Trace sig
         , Ord address
         , Carrier sig m
         , Show address
         , Show term
         )
      => Carrier (Abstract.Function term address (Value term address) :+: sig) (Abstract.FunctionC term address (Value term address) m) where
  eff (R other) = FunctionC . eff . R . handleCoercible $ other
  eff (L op) = runEvaluator $ do
    eval <- Evaluator . FunctionC $ ask
    let closure maybeName params body scope = do
          packageInfo <- currentPackage
          moduleInfo <- currentModule
          Closure packageInfo moduleInfo maybeName Nothing params body scope <$> currentFrame

    case op of
      Abstract.Function name params body scope k -> do
        val <- closure (Just name) params (Right body) scope
        Evaluator (k val)
      Abstract.BuiltIn associatedScope builtIn k -> do
        val <- closure Nothing [] (Left builtIn) associatedScope
        Evaluator (k val)
      Abstract.Bind obj@Object{} (Closure packageInfo moduleInfo name _ names body scope parentFrame) k ->
        Evaluator (k (Closure packageInfo moduleInfo name (Just obj) names body scope parentFrame))
      Abstract.Bind _ value k -> Evaluator (k value)
      Abstract.Call op params k -> do
        boxed <- case op of
          Closure _ _ _ _ _ (Left Print) _ _ -> traverse (trace . show) params $> Unit
          Closure _ _ _ _ _ (Left Show) _ _ -> pure . String . pack $ show params
          Closure packageInfo moduleInfo _ maybeSelf names (Right body) associatedScope parentFrame -> do
            -- Evaluate the bindings and body with the closure’s package/module info in scope in order to
            -- charge them to the closure's origin.
            withCurrentPackage packageInfo . withCurrentModule moduleInfo $ do
              parentScope <- scopeLookup parentFrame
              let frameEdges = Map.singleton Lexical (Map.singleton parentScope parentFrame)
              frameAddress <- newFrame associatedScope frameEdges
              withScopeAndFrame frameAddress $ do
                case maybeSelf of
                  Just object -> do
                    maybeSlot <- maybeLookupDeclaration (Declaration __self)
                    maybe (pure ()) (`assign` object) maybeSlot
                  Nothing -> pure ()
                for_ (zip names params) $ \(name, param) -> do
                  slot <- lookupSlot (Declaration name)
                  assign slot param
                catchReturn (Evaluator (eval body))
          _ -> throwValueError (CallError op)
        Evaluator (k boxed)

instance ( Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (ValueError term address))) sig
         , Carrier sig m
         , Monad m
         )
      => Carrier (Abstract.Boolean (Value term address) :+: sig) (BooleanC (Value term address) m) where
  eff (R other) = BooleanC . eff . handleCoercible $ other
  eff (L op) = case op of
    Abstract.Boolean b          k -> k $! Boolean b
    Abstract.AsBool (Boolean b) k -> k b
    Abstract.AsBool other       k -> throwBaseError (BoolError other) >>= k

instance ( Carrier sig m
         , Member (Abstract.Boolean (Value term address)) sig
         , Member (Error (LoopControl (Value term address))) sig
         , Member (Interpose (Resumable (BaseError (UnspecializedError address (Value term address))))) sig
         )
      => Carrier (Abstract.While (Value term address) :+: sig) (WhileC (Value term address) m) where
  eff (R other) = WhileC . eff . handleCoercible $ other
  eff (L (Abstract.While cond body k)) = do

    let loop x = catchError x $ \case
          Break value -> pure value
          Abort -> pure Unit
          -- FIXME: Figure out how to deal with this. Ruby treats this as the result
          -- of the current block iteration, while PHP specifies a breakout level
          -- and TypeScript appears to take a label.
          Continue _  -> loop x

    interpose @(Resumable (BaseError (UnspecializedError address (Value term address)))) (loop (do
      cond' <- cond

      -- `interpose` is used to handle 'UnspecializedError's and abort out of the
      -- loop, otherwise under concrete semantics we run the risk of the
      -- conditional always being true and getting stuck in an infinite loop.
      ifthenelse cond' (body *> throwError (Continue @(Value term address) Unit)) (pure Unit)))
      (\case
        Resumable (BaseError _ _ (UnspecializedError _))    _ -> throwError (Abort @(Value term address))
        Resumable (BaseError _ _ (RefUnspecializedError _)) _ -> throwError (Abort @(Value term address))) >>= k



  --   Abstract.While cond body k -> interpose @(Resumable (BaseError (UnspecializedError address (Value term address)))) (runEvaluator (loop (\continue -> do      cond' <- Evaluator (runWhileC cond)

  --     -- `interpose` is used to handle 'UnspecializedError's and abort out of the
  --     -- loop, otherwise under concrete semantics we run the risk of the
  --     -- conditional always being true and getting stuck in an infinite loop.

  --     ifthenelse cond' (Evaluator (runWhileC body) *> continue) (pure Unit))))
  --     (
  --       >>= runWhileC . k)
  --   where
  --     loop x = catchLoopControl (fix x) $ \case
  --       Break value -> pure value
  --       Abort -> pure Unit
  --       -- FIXME: Figure out how to deal with this. Ruby treats this as the result
  --       -- of the current block iteration, while PHP specifies a breakout level
  --       -- and TypeScript appears to take a label.
  --       Continue _  -> loop x

    -- case op of
    --   Abstract.While cond body k -> interpose @(Resumable (BaseError (UnspecializedError address (Value term address)))) (runEvaluator (loop (\continue -> do
    --     cond' <- Evaluator (runWhileC cond)

    --     -- `interpose` is used to handle 'UnspecializedError's and abort out of the
    --     -- loop, otherwise under concrete semantics we run the risk of the
    --     -- conditional always being true and getting stuck in an infinite loop.

    --     ifthenelse cond' (Evaluator (runWhileC body) *> continue) (pure Unit)
    --     case _ of
    --       Resumable (BaseError _ _ (UnspecializedError _))    _ -> throwError (Abort @(Value term address)) >>= k
    --       Resumable (BaseError _ _ (RefUnspecializedError _)) _ -> throwError (Abort @(Value term address))) >>= k


instance Carrier sig m
      => Carrier (Abstract.Unit (Value term address) :+: sig) (UnitC (Value term address) m) where
  eff (R other) = UnitC . eff . handleCoercible $ other
  eff (L (Abstract.Unit k )) = k Unit

instance ( Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (ValueError term address))) sig
         , Carrier sig m
         , Monad m
         )
      => Carrier (Abstract.String (Value term address) :+: sig) (StringC (Value term address) m) where
  eff (R other) = StringC . eff . handleCoercible $ other
  eff (L op) = case op of
    Abstract.String   t          k -> k (String t)
    Abstract.AsString (String t) k -> k t
    Abstract.AsString other      k -> throwBaseError (StringError other) >>= k

instance ( Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (ValueError term address))) sig
         , Carrier sig m
         , Monad m
         )
      => Carrier (Abstract.Numeric (Value term address) :+: sig) (NumericC (Value term address) m) where
  eff (R other) = NumericC . eff . handleCoercible $ other
  eff (L op) = case op of
    Abstract.Integer  t k -> k (Integer (Number.Integer t))
    Abstract.Float    t k -> k (Float (Number.Decimal t))
    Abstract.Rational t k -> k (Rational (Number.Ratio t))
    Abstract.LiftNumeric f arg k -> k =<< case arg of
      Integer (Number.Integer i) -> pure $ Integer (Number.Integer (runNumericFunction f i))
      Float (Number.Decimal d)   -> pure $ Float (Number.Decimal (runNumericFunction f d))
      Rational (Number.Ratio r)  -> pure $ Rational (Number.Ratio (runNumericFunction f r))
      other                      -> throwBaseError (NumericError other)
    Abstract.LiftNumeric2 f left right k -> k =<< case (left, right) of
      (Integer  i, Integer j)  -> attemptUnsafeArithmetic (runNumeric2Function f i j) & specialize
      (Integer  i, Rational j) -> attemptUnsafeArithmetic (runNumeric2Function f i j) & specialize
      (Integer  i, Float j)    -> attemptUnsafeArithmetic (runNumeric2Function f i j) & specialize
      (Rational i, Integer j)  -> attemptUnsafeArithmetic (runNumeric2Function f i j) & specialize
      (Rational i, Rational j) -> attemptUnsafeArithmetic (runNumeric2Function f i j) & specialize
      (Rational i, Float j)    -> attemptUnsafeArithmetic (runNumeric2Function f i j) & specialize
      (Float    i, Integer j)  -> attemptUnsafeArithmetic (runNumeric2Function f i j) & specialize
      (Float    i, Rational j) -> attemptUnsafeArithmetic (runNumeric2Function f i j) & specialize
      (Float    i, Float j)    -> attemptUnsafeArithmetic (runNumeric2Function f i j) & specialize
      _                        -> throwBaseError (Numeric2Error left right)

-- Dispatch whatever's contained inside a 'Number.SomeNumber' to its appropriate 'MonadValue' ctor
specialize :: ( Member (Reader ModuleInfo) sig
              , Member (Reader Span) sig
              , Member (Resumable (BaseError (ValueError term address))) sig
              , Carrier sig m
              )
           => Either ArithException Number.SomeNumber
           -> m (Value term address)
specialize (Left exc) = throwBaseError (ArithmeticError exc)
specialize (Right (Number.SomeNumber (Number.Integer t))) = pure (Integer (Number.Integer t))
specialize (Right (Number.SomeNumber (Number.Decimal t))) = pure (Float (Number.Decimal t))
specialize (Right (Number.SomeNumber (Number.Ratio t)))   = pure (Rational (Number.Ratio t))


instance ( Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (ValueError term address))) sig
         , Carrier sig m
         , Monad m
         )
      => Carrier (Abstract.Bitwise (Value term address) :+: sig) (BitwiseC (Value term address) m) where
  eff (R other) = BitwiseC . eff . handleCoercible $ other
  eff (L op) = case op of
    CastToInteger (Integer (Number.Integer i)) k -> k (Integer (Number.Integer i))
    CastToInteger (Float (Number.Decimal i)) k -> k (Integer (Number.Integer (coefficient (normalize i))))
    CastToInteger i k -> throwBaseError (NumericError i) >>= k
    LiftBitwise operator (Integer (Number.Integer i)) k -> k . Integer . Number.Integer . runBitwiseFunction operator $ i
    LiftBitwise _ other k -> throwBaseError (BitwiseError other) >>= k
    LiftBitwise2 operator (Integer (Number.Integer i)) (Integer (Number.Integer j)) k -> k . Integer . Number.Integer $ runBitwise2Function operator i j
    LiftBitwise2 _ left right k -> throwBaseError (Bitwise2Error left right) >>= k
    UnsignedRShift (Integer (Number.Integer i)) (Integer (Number.Integer j)) k | i >= 0 -> k . Integer . Number.Integer $ ourShift (fromIntegral i) (fromIntegral j)
    UnsignedRShift left right k -> throwBaseError (Bitwise2Error left right) >>= k

ourShift :: Word64 -> Int -> Integer
ourShift a b = toInteger (shiftR a b)


instance Carrier sig m => Carrier (Abstract.Object address (Value term address) :+: sig) (ObjectC address (Value term address) m) where
  eff (R other) = ObjectC . eff . handleCoercible $ other
  eff (L op) = case op of
    Abstract.Object address k -> k (Object address)
    Abstract.ScopedEnvironment (Object address) k -> k (Just address)
    Abstract.ScopedEnvironment (Class _ _ address) k -> k (Just address)
    Abstract.ScopedEnvironment (Namespace _ address) k -> k (Just address)
    Abstract.ScopedEnvironment _ k -> k Nothing
    Abstract.Klass n frame k -> k (Class n mempty frame)

instance ( Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (ValueError term address))) sig
         , Carrier sig m
         , Monad m
         )
      => Carrier (Abstract.Array (Value term address) :+: sig) (ArrayC (Value term address) m) where
  eff (R other) = ArrayC . eff . handleCoercible $ other
  eff (L op) = case op of
    Abstract.Array t k -> k (Array t)
    Abstract.AsArray (Array addresses) k -> k addresses
    Abstract.AsArray val k -> throwBaseError (ArrayError val) >>= k

instance ( Carrier sig m ) => Carrier (Abstract.Hash (Value term address) :+: sig) (HashC (Value term address) m) where
  eff (R other) = HashC . eff . handleCoercible $ other
  eff (L op) = case op of
    Abstract.Hash t k -> k ((Hash . map (uncurry KVPair)) t)
    Abstract.KvPair t v k -> k (KVPair t v)


instance AbstractHole (Value term address) where
  hole = Hole

instance (Show address, Show term) => AbstractIntro (Value term address) where
  null     = Null

-- | Construct a 'Value' wrapping the value arguments (if any).
instance ( Member (Abstract.Boolean (Value term address)) sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (ValueError term address))) sig
         , Show address
         , Show term
         , Carrier sig m
         )
      => AbstractValue term address (Value term address) m where
  asPair val
    | KVPair k v <- val = pure (k, v)
    | otherwise = throwValueError $ KeyValueError val

  tuple = pure . Tuple

  namespace name = pure . Namespace name

  index = go where
    tryIdx list ii
      | ii > genericLength list = throwValueError (BoundsError list ii)
      | otherwise               = pure (genericIndex list ii)
    go arr idx
      | (Array arr, Integer (Number.Integer i)) <- (arr, idx) = tryIdx arr i
      | (Tuple tup, Integer (Number.Integer i)) <- (arr, idx) = tryIdx tup i
      | otherwise = throwValueError (IndexError arr idx)

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
        go :: Ord a => a -> a -> Evaluator term address (Value term address) m (Value term address)
        go l r = case comparator of
          Concrete f  -> boolean (f l r)
          Generalized -> pure $ Integer (Number.Integer (orderingToInt (compare l r)))

        -- Map from [LT, EQ, GT] to [-1, 0, 1]
        orderingToInt :: Ordering -> Prelude.Integer
        orderingToInt = toInteger . pred . fromEnum

        pair = (left, right)

-- | The type of exceptions that can be thrown when constructing values in 'Value'’s 'MonadValue' instance.
data ValueError term address resume where
  StringError            :: Value term address                       -> ValueError term address Text
  BoolError              :: Value term address                       -> ValueError term address Bool
  IndexError             :: Value term address -> Value term address -> ValueError term address (Value term address)
  CallError              :: Value term address                       -> ValueError term address (Value term address)
  NumericError           :: Value term address                       -> ValueError term address (Value term address)
  Numeric2Error          :: Value term address -> Value term address -> ValueError term address (Value term address)
  ComparisonError        :: Value term address -> Value term address -> ValueError term address (Value term address)
  BitwiseError           :: Value term address                       -> ValueError term address (Value term address)
  Bitwise2Error          :: Value term address -> Value term address -> ValueError term address (Value term address)
  KeyValueError          :: Value term address                       -> ValueError term address (Value term address, Value term address)
  ArrayError             :: Value term address                       -> ValueError term address [Value term address]
  -- Indicates that we encountered an arithmetic exception inside Haskell-native number crunching.
  ArithmeticError        :: ArithException                           -> ValueError term address (Value term address)
  -- Out-of-bounds error
  BoundsError            :: [Value term address] -> Prelude.Integer  -> ValueError term address (Value term address)

instance (Eq address, Eq term) => Eq1 (ValueError term address) where
  liftEq _ (StringError a) (StringError b)                       = a == b
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

runValueError :: Evaluator term address (Value term address) (ResumableC (BaseError (ValueError term address)) m) a
              -> Evaluator term address (Value term address) m (Either (SomeError (BaseError (ValueError term address))) a)
runValueError = Evaluator . runResumable . runEvaluator

runValueErrorWith :: (forall resume . BaseError (ValueError term address) resume -> Evaluator term address (Value term address) m resume)
                  -> Evaluator term address (Value term address) (ResumableWithC (BaseError (ValueError term address)) m) a
                  -> Evaluator term address (Value term address) m a
runValueErrorWith f = Evaluator . runResumableWith (runEvaluator . f) . runEvaluator

throwValueError :: ( Member (Resumable (BaseError (ValueError term address))) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Span) sig
                   , Carrier sig m
                   )
                => ValueError term address resume
                -> Evaluator term address (Value term address) m resume
throwValueError = throwBaseError
