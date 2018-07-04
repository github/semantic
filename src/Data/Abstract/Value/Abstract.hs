{-# LANGUAGE GADTs, UndecidableInstances #-}
module Data.Abstract.Value.Abstract where

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
         , Member (Return address) effects
         )
      => AbstractFunction address Abstract effects where
  closure names _ body = do
    env <- foldr (\ name rest -> do
      addr <- alloc name
      assign addr Abstract
      Env.insert name addr <$> rest) (pure lowerBound) names
    addr <- locally (bindAll env *> body `catchReturn` \ (Return ptr) -> pure ptr)
    deref addr

  call Abstract params = do
    traverse_ (>>= deref) params
    box Abstract
