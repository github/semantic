{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.Value.Abstract
( Abstract (..)
, runFunction
, runBoolean
, runWhile
) where

import           Control.Abstract as Abstract
import           Control.Algebra
import           Data.Abstract.BaseError
import           Data.Abstract.Evaluatable
import           Data.Foldable
import qualified Data.Map.Strict as Map

data Abstract = Abstract
  deriving (Eq, Ord, Show)


instance ( Has (Allocator address) sig m
         , Has (Deref Abstract) sig m
         , Has (Error (Return Abstract)) sig m
         , Has Fresh sig m
         , Has (Reader (CurrentFrame address)) sig m
         , Has (Reader (CurrentScope address)) sig m
         , Has (Reader ModuleInfo) sig m
         , Has (Reader Span) sig m
         , Has (State Span) sig m
         , Has (State (ScopeGraph address)) sig m
         , Has (Resumable (BaseError (EvalError term address Abstract))) sig m
         , Has (Resumable (BaseError (ScopeError address))) sig m
         , Has (Resumable (BaseError (HeapError address))) sig m
         , Has (Resumable (BaseError (AddressError address Abstract))) sig m
         , Has (State (Heap address address Abstract)) sig m
         , Declarations term
         , Ord address
         , Show address
         , Algebra sig m
         )
      => Algebra (Abstract.Function term address Abstract :+: sig) (FunctionC term address Abstract m) where
  alg (R other) = FunctionC . alg . R . handleCoercible $ other
  alg (L op) = runEvaluator $ do
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


instance (Algebra sig m, Alternative m) => Algebra (Boolean Abstract :+: sig) (BooleanC Abstract m) where
  alg (L (Boolean _ k)) = k Abstract
  alg (L (AsBool  _ k)) = k True <|> k False
  alg (R other)         = BooleanC . alg . handleCoercible $ other


instance ( Has (Abstract.Boolean Abstract) sig m
         , Algebra sig m
         , Alternative m
         )
      => Algebra (While Abstract :+: sig) (WhileC Abstract m) where
  alg (R other) = WhileC . alg . handleCoercible $ other
  alg (L (Abstract.While cond body k)) = do
    cond' <- cond
    ifthenelse cond' (body *> empty) (k Abstract)

instance Algebra sig m
      => Algebra (Unit Abstract :+: sig) (UnitC Abstract m) where
  alg (R other)             = UnitC . alg . handleCoercible $ other
  alg (L (Abstract.Unit k)) = k Abstract

instance Algebra sig m
      => Algebra (Abstract.String Abstract :+: sig) (StringC Abstract m) where
  alg (R other) = StringC . alg . handleCoercible $ other
  alg (L op) = case op of
    Abstract.String _ k -> k Abstract
    AsString        _ k -> k ""

instance Algebra sig m
      => Algebra (Numeric Abstract :+: sig) (NumericC Abstract m) where
  alg (R other) = NumericC . alg . handleCoercible $ other
  alg (L op) = case op of
    Integer _ k          -> k Abstract
    Float _ k            -> k Abstract
    Rational _ k         -> k Abstract
    LiftNumeric _ _ k    -> k Abstract
    LiftNumeric2 _ _ _ k -> k Abstract

instance Algebra sig m
      => Algebra (Bitwise Abstract :+: sig) (BitwiseC Abstract m) where
  alg (R other) = BitwiseC . alg . handleCoercible $ other
  alg (L op) = case op of
    CastToInteger _ k    -> k Abstract
    LiftBitwise _ _ k    -> k Abstract
    LiftBitwise2 _ _ _ k -> k Abstract
    UnsignedRShift _ _ k -> k Abstract

instance Algebra sig m
      => Algebra (Object address Abstract :+: sig) (ObjectC address Abstract m) where
  alg (R other) = ObjectC . alg . handleCoercible $ other
  alg (L op) = case op of
    Object _ k            -> k Abstract
    ScopedEnvironment _ k -> k Nothing
    Klass _ _ k           -> k Abstract

instance Algebra sig m
      => Algebra (Array Abstract :+: sig) (ArrayC Abstract m) where
  alg (R other) = ArrayC . alg . handleCoercible $ other
  alg (L op) = case op of
    Array _ k   -> k Abstract
    AsArray _ k -> k []

instance Algebra sig m
      => Algebra (Hash Abstract :+: sig) (HashC Abstract m) where
  alg (R other) = HashC . alg . handleCoercible $ other
  alg (L op) = case op of
    Hash _ k     -> k Abstract
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
