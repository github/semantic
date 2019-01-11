{-# LANGUAGE LambdaCase, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Value.Abstract
( Abstract (..)
, runFunction
, runBoolean
, runWhile
) where

import Control.Abstract as Abstract
import Control.Effect.Carrier
import Control.Effect.Sum
import Data.Abstract.BaseError
import Data.Abstract.Evaluatable
import qualified Data.Map.Strict as Map
import Prologue

data Abstract = Abstract
  deriving (Eq, Ord, Show)


instance ( Member (Allocator address) sig
         , Member (Deref Abstract) sig
         , Member (Error (Return Abstract)) sig
         , Member Fresh sig
         , Member (Reader (CurrentFrame address)) sig
         , Member (Reader (CurrentScope address)) sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (State Span) sig
         , Member (State (ScopeGraph address)) sig
         , Member (Resumable (BaseError (EvalError term address Abstract))) sig
         , Member (Resumable (BaseError (ScopeError address))) sig
         , Member (Resumable (BaseError (HeapError address))) sig
         , Member (Resumable (BaseError (AddressError address Abstract))) sig
         , Member (State (Heap address address Abstract)) sig
         , Declarations term
         , Ord address
         , Show address
         , Carrier sig m
         )
      => Carrier (Abstract.Function term address Abstract :+: sig) (FunctionC term address Abstract (Eff m)) where
  ret = FunctionC . const . ret
  eff op = FunctionC (\ eval -> handleSum (eff . handleReader eval runFunctionC) (\case
    Function _ params body scope k -> runEvaluator $ do
      currentScope' <- currentScope
      currentFrame' <- currentFrame
      let frameLinks = Map.singleton Lexical (Map.singleton currentScope' currentFrame')
      frame <- newFrame scope frameLinks
      res <- withScopeAndFrame frame $ do
        for_ params $ \param -> do
          address <- lookupDeclaration (Declaration param)
          assign address Abstract
        catchReturn (runFunction (Evaluator . eval) (Evaluator (eval body)))
      Evaluator $ runFunctionC (k res) eval
    BuiltIn _ _ k -> runFunctionC (k Abstract) eval
    Bind _ _ k -> runFunctionC (k Abstract) eval
    Call _ _ k -> runFunctionC (k Abstract) eval) op)


instance (Carrier sig m, Alternative m) => Carrier (Boolean Abstract :+: sig) (BooleanC Abstract m) where
  ret = BooleanC . ret
  eff = BooleanC . handleSum (eff . handleCoercible) (\case
    Boolean _ k -> runBooleanC (k Abstract)
    AsBool  _ k -> runBooleanC (k True) <|> runBooleanC (k False))


instance ( Member (Abstract.Boolean Abstract) sig
         , Carrier sig m
         , Alternative m
         , Monad m
         )
      => Carrier (While Abstract :+: sig) (WhileC Abstract m) where
  ret = WhileC . ret
  eff = WhileC . handleSum
    (eff . handleCoercible)
    (\ (Abstract.While cond body k) -> do
      cond' <- runWhileC cond
      ifthenelse cond' (runWhileC body *> empty) (runWhileC (k Abstract)))


instance Carrier sig m
      => Carrier (Unit Abstract :+: sig) (UnitC Abstract m) where
  ret = UnitC . ret
  eff = UnitC . handleSum
    (eff . handleCoercible)
    (\ (Abstract.Unit k) -> runUnitC (k Abstract))

instance Carrier sig m
      => Carrier (Abstract.String Abstract :+: sig) (StringC Abstract m) where
  ret = StringC . ret
  eff = StringC . handleSum (eff . handleCoercible) (\case
    Abstract.String _ k -> runStringC (k Abstract)
    AsString        _ k -> runStringC (k ""))

instance Carrier sig m
      => Carrier (Numeric Abstract :+: sig) (NumericC Abstract m) where
  ret = NumericC . ret
  eff = NumericC . handleSum (eff . handleCoercible) (\case
    Integer _ k -> runNumericC (k Abstract)
    Float _ k -> runNumericC (k Abstract)
    Rational _ k -> runNumericC (k Abstract)
    LiftNumeric _ _ k -> runNumericC (k Abstract)
    LiftNumeric2 _ _ _ k -> runNumericC (k Abstract))

instance Carrier sig m
      => Carrier (Bitwise Abstract :+: sig) (BitwiseC Abstract m) where
  ret = BitwiseC . ret
  eff = BitwiseC . handleSum (eff . handleCoercible) (\case
    CastToInteger _ k -> runBitwiseC (k Abstract)
    LiftBitwise _ _ k -> runBitwiseC (k Abstract)
    LiftBitwise2 _ _ _ k -> runBitwiseC (k Abstract)
    UnsignedRShift _ _ k -> runBitwiseC (k Abstract))

instance Carrier sig m
      => Carrier (Object address Abstract :+: sig) (ObjectC address Abstract m) where
  ret = ObjectC . ret
  eff = ObjectC . handleSum (eff . handleCoercible) (\case
    Object _ k -> runObjectC (k Abstract)
    ScopedEnvironment _ k -> runObjectC (k Nothing)
    Klass _ _ k -> runObjectC (k Abstract))

instance Carrier sig m
      => Carrier (Array Abstract :+: sig) (ArrayC Abstract m) where
  ret = ArrayC . ret
  eff = ArrayC . handleSum (eff . handleCoercible) (\case
    Array _ k -> runArrayC (k Abstract)
    AsArray _ k -> runArrayC (k []))

instance Carrier sig m
      => Carrier (Hash Abstract :+: sig) (HashC Abstract m) where
  ret = HashC . ret
  eff = HashC . handleSum (eff . handleCoercible) (\case
    Hash _ k -> runHashC (k Abstract)
    KvPair _ _ k -> runHashC (k Abstract))


instance Ord address => ValueRoots address Abstract where
  valueRoots = mempty

instance AbstractHole Abstract where
  hole = Abstract

instance AbstractIntro Abstract where
  hash _     = Abstract
  kvPair _ _ = Abstract
  null       = Abstract

instance AbstractValue term address Abstract m where
  tuple _ = pure Abstract

  namespace _ _ = pure Abstract

  asPair _ = pure (Abstract, Abstract)

  index _ _ = pure Abstract

  liftComparison _ _ _ = pure Abstract
