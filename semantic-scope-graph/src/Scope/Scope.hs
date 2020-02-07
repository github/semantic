{-# LANGUAGE OverloadedStrings #-}
module Scope.Scope
  ( Scope (..)
  , Reference (..)
  , ReferenceInfo (..)
  , Domain (..)
  ) where

import Data.Hole
import Data.Map.Strict (Map)
import Data.Semilattice.Lower
import Data.Sequence (Seq)
import Scope.Info
import Scope.Path
import Scope.Reference
import Scope.Types

-- Offsets and frame addresses in the heap should be addresses?
data Scope address = Scope
  { edges        :: Map EdgeLabel [address]
  , references   :: Map Reference ([ReferenceInfo], Path address)
  , declarations :: Seq (Info address)
  , domain       :: Domain
  } deriving (Eq, Show, Ord)

instance Lower (Scope scopeAddress) where
  lowerBound = Scope mempty mempty mempty Standard

instance AbstractHole (Scope scopeAddress) where
  hole = lowerBound
