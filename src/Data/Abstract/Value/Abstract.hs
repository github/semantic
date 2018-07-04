module Data.Abstract.Value.Abstract where

import Control.Abstract

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
