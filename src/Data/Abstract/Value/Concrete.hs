{-# LANGUAGE GADTs, RankNTypes, TypeOperators, UndecidableInstances, LambdaCase #-}
module Data.Abstract.Value.Concrete
  ( Value (..)
  , ValueError (..)
  , runFunction
  , runBoolean
  , materializeEnvironment
  , runValueError
  , runValueErrorWith
  ) where

import qualified Control.Abstract as Abstract
import Control.Abstract hiding (Boolean(..), Function(..))
import Data.Abstract.BaseError
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
import Data.Word
import Prologue

data Value address term
  = Closure PackageInfo ModuleInfo (Maybe Name) [Name] term (Environment address)
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
  | KVPair (Value address term) (Value address term)
  | Hash [Value address term]
  | Null
  | Hole
  deriving (Eq, Ord, Show)


instance Ord address => ValueRoots address (Value address term) where
  valueRoots v
    | Closure _ _ _ _ _ env <- v = Env.addresses env
    | otherwise                  = mempty


runFunction :: ( FreeVariables term
               , Member (Allocator address) effects
               , Member (Deref (Value address term)) effects
               , Member (Env address) effects
               , Member (Exc (Return address)) effects
               , Member Fresh effects
               , Member (Reader ModuleInfo) effects
               , Member (Reader PackageInfo) effects
               , Member (Reader Span) effects
               , Member (Resumable (BaseError (AddressError address (Value address term)))) effects
               , Member (Resumable (BaseError (ValueError address term))) effects
               , Member (State (Heap address (Value address term))) effects
               , Ord address
               , PureEffects effects
               )
            => (term -> Evaluator term address (Value address term) (Abstract.Function term address (Value address term) ': effects) address)
            -> Evaluator term address (Value address term) (Abstract.Function term address (Value address term) ': effects) a
            -> Evaluator term address (Value address term) effects a
runFunction eval = interpret $ \case
  Abstract.Function name params body -> do
    packageInfo <- currentPackage
    moduleInfo <- currentModule
    Closure packageInfo moduleInfo name params body <$> close (foldr Set.delete (freeVariables body) params)
  Abstract.Call op self params -> do
    case op of
      Closure packageInfo moduleInfo _ names body env -> do
        -- Evaluate the bindings and body with the closure’s package/module info in scope in order to
        -- charge them to the closure's origin.
        withCurrentPackage packageInfo . withCurrentModule moduleInfo $ do
          bindings <- foldr (\ (name, addr) rest -> Env.insert name addr <$> rest) (pure lowerBound) (zip names params)
          let fnCtx = EvalContext (Just self) (Env.push env)
          withEvalContext fnCtx (catchReturn (bindAll bindings *> runFunction eval (eval body)))
      _ -> throwValueError (CallError op) >>= box

runBoolean :: ( Member (Reader ModuleInfo) effects
              , Member (Reader Span) effects
              , Member (Resumable (BaseError (ValueError address term))) effects
              , PureEffects effects
              )
           => Evaluator term address (Value address term) (Abstract.Boolean (Value address term) ': effects) a
           -> Evaluator term address (Value address term) effects a
runBoolean = interpret $ \case
  Abstract.Boolean b          -> pure $! Boolean b
  Abstract.AsBool (Boolean b) -> pure b
  Abstract.AsBool other       -> throwValueError $! BoolError other
  Abstract.Disjunction a b    -> do
    a' <- runBoolean (Evaluator a)
    a'' <- runBoolean (asBool a')
    if a'' then pure a' else runBoolean (Evaluator b)


instance AbstractHole (Value address term) where
  hole = Hole

instance (Show address, Show term) => AbstractIntro (Value address term) where
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

materializeEnvironment :: ( Member (Deref (Value address term)) effects
                          , Member (Reader ModuleInfo) effects
                          , Member (Reader Span) effects
                          , Member (Resumable (BaseError (AddressError address (Value address term)))) effects
                          , Member (State (Heap address (Value address term))) effects
                          , Ord address
                          )
                       => Value address term
                       -> Evaluator term address (Value address term) effects (Maybe (Environment address))
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
instance ( Member (Allocator address) effects
         , Member (Abstract.Boolean (Value address term)) effects
         , Member (Deref (Value address term)) effects
         , Member (Env address) effects
         , Member (Exc (LoopControl address)) effects
         , Member (Exc (Return address)) effects
         , Member Fresh effects
         , Member (Reader ModuleInfo) effects
         , Member (Reader PackageInfo) effects
         , Member (Reader Span) effects
         , Member (Resumable (BaseError (ValueError address term))) effects
         , Member (Resumable (BaseError (AddressError address (Value address term)))) effects
         , Member (State (Heap address (Value address term))) effects
         , Member Trace effects
         , Ord address
         , Show address
         , Show term
         )
      => AbstractValue term address (Value address term) effects where
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
        specialize :: ( AbstractValue term address (Value address term) effects
                      , Member (Reader ModuleInfo) effects
                      , Member (Reader Span) effects
                      , Member (Resumable (BaseError (ValueError address term))) effects
                      )
                   => Either ArithException Number.SomeNumber
                   -> Evaluator term address (Value address term) effects (Value address term)
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
        go :: (AbstractValue term address (Value address term) effects, Member (Abstract.Boolean (Value address term)) effects, Ord a) => a -> a -> Evaluator term address (Value address term) effects (Value address term)
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
    Break value -> deref value
    -- FIXME: Figure out how to deal with this. Ruby treats this as the result of the current block iteration, while PHP specifies a breakout level and TypeScript appears to take a label.
    Continue _  -> loop x)

  castToInteger (Integer (Number.Integer i)) = pure (Integer (Number.Integer i))
  castToInteger (Float (Number.Decimal i)) = pure (Integer (Number.Integer (coefficient (normalize i))))
  castToInteger i = throwValueError (NumericError i)

-- | The type of exceptions that can be thrown when constructing values in 'Value'’s 'MonadValue' instance.
data ValueError address term resume where
  StringError            :: Value address term                       -> ValueError address term Text
  BoolError              :: Value address term                       -> ValueError address term Bool
  IndexError             :: Value address term -> Value address term -> ValueError address term (Value address term)
  NamespaceError         :: Prelude.String                           -> ValueError address term (Bindings address)
  CallError              :: Value address term                       -> ValueError address term (Value address term)
  NumericError           :: Value address term                       -> ValueError address term (Value address term)
  Numeric2Error          :: Value address term -> Value address term -> ValueError address term (Value address term)
  ComparisonError        :: Value address term -> Value address term -> ValueError address term (Value address term)
  BitwiseError           :: Value address term                       -> ValueError address term (Value address term)
  Bitwise2Error          :: Value address term -> Value address term -> ValueError address term (Value address term)
  KeyValueError          :: Value address term                       -> ValueError address term (Value address term, Value address term)
  ArrayError             :: Value address term                       -> ValueError address term [address]
  -- Indicates that we encountered an arithmetic exception inside Haskell-native number crunching.
  ArithmeticError        :: ArithException                           -> ValueError address term (Value address term)
  -- Out-of-bounds error
  BoundsError            :: [address]          -> Prelude.Integer    -> ValueError address term (Value address term)


instance (Eq address, Eq term) => Eq1 (ValueError address term) where
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

deriving instance (Show address, Show term) => Show (ValueError address term resume)
instance (Show address, Show term) => Show1 (ValueError address term) where
  liftShowsPrec _ _ = showsPrec

runValueError :: Effects effects
              => Evaluator term address (Value address term) (Resumable (BaseError (ValueError address term)) ': effects) a
              -> Evaluator term address (Value address term) effects (Either (SomeExc (BaseError (ValueError address term))) a)
runValueError = runResumable

runValueErrorWith :: Effects effects
                  => (forall resume . BaseError (ValueError address term) resume -> Evaluator term address (Value address term) effects resume)
                  -> Evaluator term address (Value address term) (Resumable (BaseError (ValueError address term)) ': effects) a
                  -> Evaluator term address (Value address term) effects a
runValueErrorWith = runResumableWith

throwValueError :: ( Member (Resumable (BaseError (ValueError address term))) effects
                   , Member (Reader ModuleInfo) effects
                   , Member (Reader Span) effects
                   )
                => ValueError address term resume
                -> Evaluator term address (Value address term) effects resume
throwValueError = throwBaseError
