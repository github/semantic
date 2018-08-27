{-# LANGUAGE GADTs, RankNTypes, TypeOperators #-}
module Data.Abstract.Address.Hole
( Hole(..)
, toMaybe
, runAllocator
, handleAllocator
, runDeref
, handleDeref
) where

import Control.Abstract
import Prologue

data Hole context a = Partial context | Total a
  deriving (Foldable, Functor, Eq, Ord, Show, Traversable)

instance Lower context => AbstractHole (Hole context a) where
  hole = Partial lowerBound

toMaybe :: Hole context a -> Maybe a
toMaybe (Partial _) = Nothing
toMaybe (Total a)   = Just a


relocate :: Evaluator address1 value effects a -> Evaluator address2 value effects a
relocate = raiseEff . lowerEff


runAllocator :: PureEffects effects
             => (forall x. Allocator address (Eff (Allocator address ': effects)) x -> Evaluator address value effects x)
             -> Evaluator (Hole context address) value (Allocator (Hole context address) ': effects) a
             -> Evaluator (Hole context address) value effects a
runAllocator handler = interpret (handleAllocator handler)

handleAllocator :: (forall x. Allocator address (Eff (Allocator address ': effects)) x -> Evaluator address value effects x)
                -> Allocator (Hole context address) (Eff (Allocator (Hole context address) ': effects)) a
                -> Evaluator (Hole context address) value effects a
handleAllocator handler (Alloc name) = relocate (Total <$> handler (Alloc name))

runDeref :: PureEffects effects
         => (forall x. Deref value (Eff (Deref value ': effects)) x -> Evaluator address value effects x)
         -> Evaluator (Hole context address) value (Deref value ': effects) a
         -> Evaluator (Hole context address) value effects a
runDeref handler = interpret (handleDeref handler)

handleDeref :: (forall x. Deref value (Eff (Deref value ': effects)) x -> Evaluator address value effects x)
            -> Deref value (Eff (Deref value ': effects)) a
            -> Evaluator (Hole context address) value effects a
handleDeref handler (DerefCell        cell) = relocate (handler (DerefCell        cell))
handleDeref handler (AssignCell value cell) = relocate (handler (AssignCell value cell))
