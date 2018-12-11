{-# LANGUAGE DeriveAnyClass, GADTs, RankNTypes, TypeOperators, UndecidableInstances, LambdaCase, ScopedTypeVariables #-}
module Data.Abstract.Value.Concrete
  ( Value (..)
  , ValueError (..)
  , runValueError
  , runValueErrorWith
  ) where

import Control.Abstract.ScopeGraph (Allocator, ScopeError)
import Control.Abstract.Heap (scopeLookup)
import qualified Control.Abstract as Abstract
import Control.Abstract hiding (Boolean(..), Function(..), String(..), Unit(..), While(..))
import Control.Effect.Carrier
import Control.Effect.Interpose
import Control.Effect.Sum
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
                                                                         --  Scope   Frame
  = Closure PackageInfo ModuleInfo (Maybe Name) [Name] (Either BuiltIn term) address address
  | Unit
  | Boolean Bool
  | Integer  (Number.Number Integer)
  | Rational (Number.Number Rational)
  | Float    (Number.Number Scientific)
  | String Text
  | Symbol Text
  | Regex Text
  | Tuple [Value term address]
  | Array [Value term address]
  | Class Declaration [Value term address] address
  | Object address
  | Namespace Name address
  | KVPair (Value term address) (Value term address)
  | Hash [Value term address]
  | Null
  | Hole
  deriving (Eq, Ord, Show, Generic, NFData)


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
         , Member (Resumable (BaseError (EvalError address (Value term address)))) sig
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
      => Carrier (Abstract.Function term address (Value term address) :+: sig) (Abstract.FunctionC term address (Value term address) (Eff m)) where
  ret = FunctionC . const . ret
  eff op =
    let closure maybeName params body scope = do
          packageInfo <- currentPackage
          moduleInfo <- currentModule
          Closure packageInfo moduleInfo maybeName params body scope <$> currentFrame

    in FunctionC (\ eval -> handleSum (eff . handleReader eval runFunctionC) (\case
    Abstract.Function name params body scope k -> runEvaluator $ do
      val <- closure (Just name) params (Right body) scope
      Evaluator $ runFunctionC (k val) eval
    Abstract.BuiltIn associatedScope builtIn k -> runEvaluator $ do
      val <- closure Nothing [] (Left builtIn) associatedScope
      Evaluator $ runFunctionC (k val) eval
    Abstract.Call op params k -> runEvaluator $ do
      boxed <- case op of
        Closure _ _ _ _ (Left Print) _ _ -> traverse (trace . show) params $> Unit
        Closure _ _ _ _ (Left Show) _ _ -> pure . String . pack $ show params
        Closure packageInfo moduleInfo _ names (Right body) associatedScope parentFrame -> do
          -- Evaluate the bindings and body with the closure’s package/module info in scope in order to
          -- charge them to the closure's origin.
          withCurrentPackage packageInfo . withCurrentModule moduleInfo $ do
            parentScope <- scopeLookup parentFrame
            let frameEdges = Map.singleton Lexical (Map.singleton parentScope parentFrame)
            frameAddress <- newFrame associatedScope frameEdges
            withScopeAndFrame frameAddress $ do
              for_ (zip names params) $ \(name, param) -> do
                addr <- lookupDeclaration (Declaration name)
                assign addr param
              catchReturn (runFunction (Evaluator . eval) (Evaluator (eval body)))
        _ -> throwValueError (CallError op)
      Evaluator $ runFunctionC (k boxed) eval) op)

instance ( Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (ValueError term address))) sig
         , Carrier sig m
         , Monad m
         )
      => Carrier (Abstract.Boolean (Value term address) :+: sig) (BooleanC (Value term address) m) where
  ret = BooleanC . ret
  eff = BooleanC . handleSum (eff . handleCoercible) (\case
    Abstract.Boolean b          k -> runBooleanC . k $! Boolean b
    Abstract.AsBool (Boolean b) k -> runBooleanC (k b)
    Abstract.AsBool other       k -> throwBaseError (BoolError other) >>= runBooleanC . k)


instance ( Carrier sig m
         , Member (Abstract.Boolean (Value term address)) sig
         , Member (Error (LoopControl (Value term address))) sig
         , Member (Interpose (Resumable (BaseError (UnspecializedError address (Value term address))))) sig
         )
      => Carrier (Abstract.While (Value term address) :+: sig) (WhileC (Value term address) (Eff m)) where
  ret = WhileC . ret
  eff = WhileC . handleSum (eff . handleCoercible) (\case
    Abstract.While cond body k -> interpose @(Resumable (BaseError (UnspecializedError address (Value term address)))) (runEvaluator (loop (\continue -> do
      cond' <- Evaluator (runWhileC cond)

      -- `interpose` is used to handle 'UnspecializedError's and abort out of the
      -- loop, otherwise under concrete semantics we run the risk of the
      -- conditional always being true and getting stuck in an infinite loop.

      ifthenelse cond' (Evaluator (runWhileC body) *> continue) (pure Unit))))
      (\case
        Resumable (BaseError _ _ (UnspecializedError _))    _ -> throwError (Abort @(Value term address))
        Resumable (BaseError _ _ (RefUnspecializedError _)) _ -> throwError (Abort @(Value term address)))
        >>= runWhileC . k)
    where
      loop x = catchLoopControl (fix x) $ \case
        Break value -> pure value
        Abort -> pure Unit
        -- FIXME: Figure out how to deal with this. Ruby treats this as the result
        -- of the current block iteration, while PHP specifies a breakout level
        -- and TypeScript appears to take a label.
        Continue _  -> loop x


instance Carrier sig m
      => Carrier (Abstract.Unit (Value term address) :+: sig) (UnitC (Value term address) m) where
  ret = UnitC . ret
  eff = UnitC . handleSum
    (eff . handleCoercible)
    (\ (Abstract.Unit k) -> runUnitC (k Unit))


instance AbstractHole (Value term address) where
  hole = Hole

instance (Show address, Show term) => AbstractIntro (Value term address) where
  integer  = Integer . Number.Integer
  string   = String
  float    = Float . Number.Decimal
  symbol   = Symbol
  rational = Rational . Number.Ratio
  regex    = Regex

  kvPair = KVPair
  hash = Hash . map (uncurry KVPair)

  null     = Null

-- | Construct a 'Value' wrapping the value arguments (if any).
instance ( Member (Allocator address) sig
         , Member (Abstract.Boolean (Value term address)) sig
         , Member (Deref (Value term address)) sig
         , Member (Error (LoopControl (Value term address))) sig
         , Member (Error (Return (Value term address))) sig
         , Member Fresh sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader PackageInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (ValueError term address))) sig
         , Member (Resumable (BaseError (AddressError address (Value term address)))) sig
         , Member (State (Heap address address (Value term address))) sig
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

  klass n binds = do
    pure $ Class n mempty binds

  namespace name = pure . Namespace name

  scopedEnvironment v
    | Object address <- v = pure (Just address)
    | Class _ _ address <- v = pure (Just address)
    | Namespace _ address <- v = pure (Just address)
    | otherwise = pure Nothing

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

  object frameAddress = pure (Object frameAddress)

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

instance (NFData term, NFData address) => NFData1 (ValueError term address) where
  liftRnf _ x = case x of
    StringError i       -> rnf i
    BoolError   i       -> rnf i
    IndexError  i j     -> rnf i `seq` rnf j
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
              => Evaluator term address (Value term address) (ResumableC (BaseError (ValueError term address)) (Eff m)) a
              -> Evaluator term address (Value term address) m (Either (SomeError (BaseError (ValueError term address))) a)
runValueError = Evaluator . runResumable . runEvaluator

runValueErrorWith :: Carrier sig m
                  => (forall resume . BaseError (ValueError term address) resume -> Evaluator term address (Value term address) m resume)
                  -> Evaluator term address (Value term address) (ResumableWithC (BaseError (ValueError term address)) (Eff m)) a
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
