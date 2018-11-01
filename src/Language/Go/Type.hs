{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.Go.Type where

import Prologue

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm
import Proto3.Suite.Class

-- | A Bidirectional channel in Go (e.g. `chan`).
newtype BidirectionalChannel a = BidirectionalChannel { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)

instance Eq1 BidirectionalChannel where liftEq = genericLiftEq
instance Ord1 BidirectionalChannel where liftCompare = genericLiftCompare
instance Show1 BidirectionalChannel where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for BidirectionalChannel
instance Evaluatable BidirectionalChannel

-- | A Receive channel in Go (e.g. `<-chan`).
newtype ReceiveChannel a = ReceiveChannel { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)

instance Eq1 ReceiveChannel where liftEq = genericLiftEq
instance Ord1 ReceiveChannel where liftCompare = genericLiftCompare
instance Show1 ReceiveChannel where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for ReceiveChannel
instance Evaluatable ReceiveChannel

-- | A Send channel in Go (e.g. `chan<-`).
newtype SendChannel a = SendChannel { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)

instance Eq1 SendChannel where liftEq = genericLiftEq
instance Ord1 SendChannel where liftCompare = genericLiftCompare
instance Show1 SendChannel where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for SendChannel
instance Evaluatable SendChannel
