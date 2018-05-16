{-# LANGUAGE DeriveAnyClass #-}
module Language.Go.Type where

import Prologue
import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm

-- | A Bidirectional channel in Go (e.g. `chan`).
newtype BidirectionalChannel a = BidirectionalChannel a
  deriving (Diffable, Eq, FreeVariables1, Declarations1, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable)

instance Eq1 BidirectionalChannel where liftEq = genericLiftEq
instance Ord1 BidirectionalChannel where liftCompare = genericLiftCompare
instance Show1 BidirectionalChannel where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 BidirectionalChannel

-- TODO: Implement Eval instance for BidirectionalChannel
instance Evaluatable BidirectionalChannel

-- | A Receive channel in Go (e.g. `<-chan`).
newtype ReceiveChannel a = ReceiveChannel a
  deriving (Diffable, Eq, FreeVariables1, Declarations1, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable)

instance Eq1 ReceiveChannel where liftEq = genericLiftEq
instance Ord1 ReceiveChannel where liftCompare = genericLiftCompare
instance Show1 ReceiveChannel where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 ReceiveChannel

-- TODO: Implement Eval instance for ReceiveChannel
instance Evaluatable ReceiveChannel

-- | A Send channel in Go (e.g. `chan<-`).
newtype SendChannel a = SendChannel a
  deriving (Diffable, Eq, FreeVariables1, Declarations1, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable)

instance Eq1 SendChannel where liftEq = genericLiftEq
instance Ord1 SendChannel where liftCompare = genericLiftCompare
instance Show1 SendChannel where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 SendChannel

-- TODO: Implement Eval instance for SendChannel
instance Evaluatable SendChannel
