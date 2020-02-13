{-# LANGUAGE OverloadedStrings #-}
-- This is meant to be imported qualified.
module Scope.Scope
  ( Scope (..)
  , Reference (..)
  , ReferenceInfo (..)
  , Domain (..)
  , fold
  ) where

import           Data.Hole
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semilattice.Lower
import           Data.Sequence (Seq)
import           Scope.Info
import           Scope.Path
import           Scope.Reference
import           Scope.Types

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

fold ::
  Monoid m
  => (EdgeLabel -> [addr] -> m)
  -> (Reference -> [ReferenceInfo] -> Path addr -> m)
  -> (Info addr -> m)
  -> Scope addr
  -> m
fold onEdge onRef onInfo s
  = Map.foldMapWithKey onEdge (edges s)
  <> Map.foldMapWithKey (uncurry . onRef) (references s)
  <> foldMap onInfo (declarations s)

