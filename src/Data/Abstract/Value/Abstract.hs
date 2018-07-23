{-# LANGUAGE GADTs, UndecidableInstances #-}
module Data.Abstract.Value.Abstract ( Abstract (..) ) where

import Control.Abstract
import Data.Abstract.Environment as Env
import Prologue

data Abstract = Abstract
  deriving (Eq, Ord, Show)


instance Ord address => ValueRoots address Abstract where
  valueRoots = mempty

instance AbstractHole Abstract where
  hole = Abstract

instance AbstractIntro Abstract where
  unit       = Abstract
  integer _  = Abstract
  boolean _  = Abstract
  string _   = Abstract
  float _    = Abstract
  symbol _   = Abstract
  rational _ = Abstract
  hash _     = Abstract
  kvPair _ _ = Abstract
  null       = Abstract

instance ( Member (Allocator address Abstract) effects
         , Member (Env address) effects
         , Member (Exc (Return address)) effects
         , Member Fresh effects
         )
      => AbstractFunction address Abstract effects where
  function names _ body = do
    env <- foldr (\ name rest -> do
      addr <- alloc name
      assign addr Abstract
      Env.insert name addr <$> rest) (pure lowerBound) names
    addr <- locally (bindAll env *> catchReturn body)
    deref addr

  call Abstract params = do
    traverse_ deref params
    box Abstract

instance ( Member (Allocator address Abstract) effects
         , Member (Env address) effects
         , Member (Exc (Return address)) effects
         , Member NonDet effects
         , Member Fresh effects
         )
      => AbstractValue address Abstract effects where
  array _ = pure Abstract

  tuple _ = pure Abstract

  klass _ _ _ = pure Abstract
  namespace _ _ = pure Abstract

  scopedEnvironment _ = pure lowerBound

  asString _ = pure ""
  asPair _ = pure (Abstract, Abstract)

  index _ _ = box Abstract

  ifthenelse _ if' else' = if' <|> else'
  disjunction = (<|>)

  liftNumeric _ _ = pure Abstract
  liftNumeric2 _ _ _ = pure Abstract

  liftBitwise _ _ = pure Abstract
  liftBitwise2 _ _ _ = pure Abstract

  liftComparison _ _ _ = pure Abstract

  loop f = f empty
