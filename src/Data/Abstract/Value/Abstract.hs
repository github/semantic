module Data.Abstract.Value.Abstract where

import Control.Abstract

data Abstract = Abstract
  deriving (Eq, Ord, Show)


instance Ord address => ValueRoots address Abstract where
  valueRoots = mempty

instance AbstractHole Abstract where
  hole = Abstract
