{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Value.Abstract
( Abstract (..)
, runFunction
, runBoolean
, runWhile
) where

import Control.Abstract as Abstract
import Control.Effect.Carrier
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
      => Carrier (Abstract.Function term address Abstract :+: sig) (FunctionC term address Abstract m) where
  eff (R other) = FunctionC . eff . R . handleCoercible $ other
  eff (L op) = runEvaluator $ do
    eval <- Evaluator . FunctionC $ ask
    case op of
      Function _ params body scope k -> do
        currentScope' <- currentScope
        currentFrame' <- currentFrame
        let frameLinks = Map.singleton Lexical (Map.singleton currentScope' currentFrame')
        frame <- newFrame scope frameLinks
        res <- withScopeAndFrame frame $ do
          for_ params $ \param -> do
            slot <- lookupSlot (Declaration param)
            assign slot Abstract
          catchReturn (Evaluator (eval body))
        Evaluator (k res)
      BuiltIn _ _ k -> Evaluator (k Abstract)
      Bind _ _ k -> Evaluator (k Abstract)
      Call _ _ k -> Evaluator (k Abstract)


instance (Carrier sig m, Alternative m) => Carrier (Boolean Abstract :+: sig) (BooleanC Abstract m) where
  eff (L (Boolean _ k)) = k Abstract
  eff (L (AsBool  _ k)) = k True <|> k False
  eff (R other)         = BooleanC . eff . handleCoercible $ other


instance ( Member (Abstract.Boolean Abstract) sig
         , Carrier sig m
         , Alternative m
         )
      => Carrier (While Abstract :+: sig) (WhileC Abstract m) where
  eff (R other) = WhileC . eff . handleCoercible $ other
  eff (L (Abstract.While cond body k)) = do
    cond' <- cond
    ifthenelse cond' (body *> empty) (k Abstract)

instance Carrier sig m
      => Carrier (Unit Abstract :+: sig) (UnitC Abstract m) where
  eff (R other) = UnitC . eff . handleCoercible $ other
  eff (L (Abstract.Unit k)) = k Abstract

instance Carrier sig m
      => Carrier (Abstract.String Abstract :+: sig) (StringC Abstract m) where
  eff (R other) = StringC . eff . handleCoercible $ other
  eff (L op) = case op of
    Abstract.String _ k -> k Abstract
    AsString        _ k -> k ""

instance Carrier sig m
      => Carrier (Numeric Abstract :+: sig) (NumericC Abstract m) where
  eff (R other) = NumericC . eff . handleCoercible $ other
  eff (L op) = case op of
    Integer _ k -> k Abstract
    Float _ k -> k Abstract
    Rational _ k -> k Abstract
    LiftNumeric _ _ k -> k Abstract
    LiftNumeric2 _ _ _ k -> k Abstract

instance Carrier sig m
      => Carrier (Bitwise Abstract :+: sig) (BitwiseC Abstract m) where
  eff (R other) = BitwiseC . eff . handleCoercible $ other
  eff (L op) = case op of
    CastToInteger _ k -> k Abstract
    LiftBitwise _ _ k -> k Abstract
    LiftBitwise2 _ _ _ k -> k Abstract
    UnsignedRShift _ _ k -> k Abstract

instance Carrier sig m
      => Carrier (Object address Abstract :+: sig) (ObjectC address Abstract m) where
  eff (R other) = ObjectC . eff . handleCoercible $ other
  eff (L op) = case op of
    Object _ k -> k Abstract
    ScopedEnvironment _ k -> k Nothing
    Klass _ _ k -> k Abstract

instance Carrier sig m
      => Carrier (Array Abstract :+: sig) (ArrayC Abstract m) where
  eff (R other) = ArrayC . eff . handleCoercible $ other
  eff (L op) = case op of
    Array _ k -> k Abstract
    AsArray _ k -> k []

instance Carrier sig m
      => Carrier (Hash Abstract :+: sig) (HashC Abstract m) where
  eff (R other) = HashC . eff . handleCoercible $ other
  eff (L op) = case op of
    Hash _ k -> k Abstract
    KvPair _ _ k -> k Abstract


instance Ord address => ValueRoots address Abstract where
  valueRoots = mempty

instance AbstractHole Abstract where
  hole = Abstract

instance AbstractIntro Abstract where
  null       = Abstract

instance Applicative m => AbstractValue term address Abstract m where
  tuple _ = pure Abstract

  namespace _ _ = pure Abstract

  asPair _ = pure (Abstract, Abstract)

  index _ _ = pure Abstract

  liftComparison _ _ _ = pure Abstract
