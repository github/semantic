{-# LANGUAGE GADTs, LambdaCase, TypeOperators, UndecidableInstances #-}
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

data Abstract = Abstract
  deriving (Eq, Ord, Show)


instance ( Member (Allocator address) sig
         , Member (Deref Abstract) sig
         , Member (Env address) sig
         , Member (Error (Return address)) sig
         , Member Fresh sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (AddressError address Abstract))) sig
         , Member (State (Heap address Abstract)) sig
         , Ord address
         , Carrier sig m
         )
      => Carrier (Abstract.Function term address Abstract :+: sig) (FunctionC term address Abstract (Evaluator term address Abstract m)) where
  ret = FunctionC . const . ret
  eff op = FunctionC (\ eval -> (alg eval \/ eff . handleReader eval runFunctionC) op)
    where alg eval = \case
            Function _ params body k -> do
              env <- foldr (\ name rest -> do
                addr <- alloc name
                assign addr Abstract
                Env.insert name addr <$> rest) (pure lowerBound) params
              addr <- locally (bindAll env *> catchReturn (runFunction eval (eval body)))
              deref addr >>= flip runFunctionC eval . k
            BuiltIn _ k -> runFunctionC (k Abstract) eval
            Call _ _ params k -> do
              traverse_ deref params
              box Abstract >>= flip runFunctionC eval . k


instance (Carrier sig m, Alternative m, Monad m) => Carrier (Boolean Abstract :+: sig) (BooleanC Abstract m) where
  ret = BooleanC . ret
  eff = BooleanC . (alg \/ eff . handleCoercible)
    where alg (Boolean _ k) = runBooleanC (k Abstract)
          alg (AsBool _ k) = runBooleanC (k True) <|> runBooleanC (k False)
          alg (Disjunction a b k) = (runBooleanC a <|> runBooleanC b) >>= runBooleanC . k


instance ( Member (Abstract.Boolean Abstract) sig
         , Carrier sig m
         , Alternative m
         , Monad m
         )
      => Carrier (While Abstract :+: sig) (WhileC Abstract m) where
  ret = WhileC . ret
  eff = WhileC . (alg \/ eff . handleCoercible)
    where alg (Abstract.While cond body k) = do
            cond' <- runWhileC cond
            ifthenelse cond' (runWhileC body *> empty) (runWhileC (k unit))


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
         , Member (State (Heap address Abstract)) sig
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

  index _ _ = box Abstract

  liftNumeric _ _ = pure Abstract
  liftNumeric2 _ _ _ = pure Abstract

  liftBitwise _ _ = pure Abstract
  liftBitwise2 _ _ _ = pure Abstract

  unsignedRShift _ _ = pure Abstract

  liftComparison _ _ _ = pure Abstract

  castToInteger _ = pure Abstract
