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
import Data.Abstract.Environment as Env
import Prologue
import qualified Data.Map.Strict as Map
import Data.Abstract.Ref

data Abstract = Abstract
  deriving (Eq, Ord, Show)


instance ( Member (Allocator address) sig
         , Member (Deref Abstract) sig
         , Member (Error (Return address Abstract)) sig
         , Member Fresh sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (State Span) sig
         , Member (State (ScopeGraph address)) sig
         , Member (Resumable (BaseError (ScopeError address))) sig
         , Member (Resumable (BaseError (HeapError address))) sig
         , Member (Resumable (BaseError (AddressError address Abstract))) sig
         , Member (State (Heap address address Abstract)) sig
         , Ord address
         , Carrier sig m
         )
      => Carrier (Abstract.Function term address Abstract :+: sig) (FunctionC term address Abstract (Eff m)) where
  ret = FunctionC . const . ret
  eff op = FunctionC (\ eval -> handleSum (eff . handleReader eval runFunctionC) (\case
    Function name params body k -> runEvaluator $ do
      functionSpan <- ask @Span -- TODO: This might be wrong
      declare (Declaration name) functionSpan Nothing
      (Evaluator . flip runFunctionC eval . k =<<) . withLexicalScopeAndFrame $ do
        -- TODO: Use scope graph and heap graph
        for_ params $ \name -> do
          span <- get @Span -- TODO: This span is probably wrong
          declare (Declaration name) span Nothing
          address <- lookupDeclaration (Declaration name)
          -- assign tvar values to names in the frame of the function?
          assign address Abstract
        catchReturn (runFunction (Evaluator . eval) (Evaluator (eval body)))
    BuiltIn _ _ k -> runFunctionC (k (Rval Abstract)) eval
    Call _ _ params k -> runEvaluator $ do
      rvalBox Abstract >>= Evaluator . flip runFunctionC eval . k) op)


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
      => Carrier (While address Abstract :+: sig) (WhileC address Abstract m) where
  ret = WhileC . ret
  eff = WhileC . handleSum
    (eff . handleCoercible)
    (\ (Abstract.While cond body k) -> do
      cond' <- runWhileC cond
      ifthenelse cond' (runWhileC body *> empty) (runWhileC (k $ Rval unit)))


instance Ord address => ValueRoots address Abstract where
  valueRoots = mempty

instance AbstractHole Abstract where
  hole = Abstract

instance AbstractIntro Abstract where
  unit       = Abstract
  integer _  = Abstract
  string _   = Abstract
  float _    = Abstract
  symbol _   = Abstract
  regex _    = Abstract
  rational _ = Abstract
  hash _     = Abstract
  kvPair _ _ = Abstract
  null       = Abstract

instance ( Member (Allocator address) sig
         , Member (Deref Abstract) sig
         , Member Fresh sig
         , Member NonDet sig
         , Member (State (Heap address address Abstract)) sig
         , Ord address
         , Carrier sig m
         )
      => AbstractValue term address Abstract m where
  array _ = pure Abstract

  tuple _ = pure Abstract

  klass _ _ _ = pure Abstract
  namespace _ _ _ = pure Abstract

  scopedEnvironment _ = pure lowerBound

  asString _ = pure ""
  asPair _ = pure (Abstract, Abstract)
  asArray _ = pure mempty

  index _ _ = pure Abstract

  liftNumeric _ _ = pure Abstract
  liftNumeric2 _ _ _ = pure Abstract

  liftBitwise _ _ = pure Abstract
  liftBitwise2 _ _ _ = pure Abstract

  unsignedRShift _ _ = pure Abstract

  liftComparison _ _ _ = pure Abstract

  castToInteger _ = pure Abstract
