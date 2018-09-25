{-# LANGUAGE GADTs, LambdaCase, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Value.Abstract
( Abstract (..)
, runFunction
, runBoolean
) where

import Control.Abstract as Abstract
import Data.Abstract.BaseError
import Data.Abstract.Environment as Env
import Prologue

data Abstract = Abstract
  deriving (Eq, Ord, Show)


runFunction :: ( Member (Allocator address) effects
               , Member (Deref Abstract) effects
               , Member (Env address) effects
               , Member (Exc (Return address)) effects
               , Member Fresh effects
               , Member (Reader ModuleInfo) effects
               , Member (Reader Span) effects
               , Member (Resumable (BaseError (AddressError address Abstract))) effects
               , Member (State (Heap address address Abstract)) effects
               , Ord address
               , PureEffects effects
               )
            => Evaluator address Abstract (Function address Abstract ': effects) a
            -> Evaluator address Abstract effects a
runFunction = interpret $ \case
  Function _ params _ body -> do
    env <- foldr (\ name rest -> do
      addr <- alloc name
      assign addr Abstract
      Env.insert name addr <$> rest) (pure lowerBound) params
    addr <- locally (bindAll env *> catchReturn (runFunction (Evaluator body)))
    deref addr
  Call _ _ params -> do
    traverse_ deref params
    box Abstract

runBoolean :: ( Member NonDet effects
              , PureEffects effects
              )
           => Evaluator address Abstract (Boolean Abstract ': effects) a
           -> Evaluator address Abstract effects a
runBoolean = interpret $ \case
  Boolean _       -> pure Abstract
  AsBool  _       -> pure True <|> pure False
  Disjunction a b -> runBoolean (Evaluator (a <|> b))


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

instance ( Member (Allocator address) effects
         , Member (Deref Abstract) effects
         , Member Fresh effects
         , Member NonDet effects
         , Member (State (Heap address address Abstract)) effects
         , Ord address
         )
      => AbstractValue address Abstract effects where
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

  loop f = f empty

  castToInteger _ = pure Abstract
