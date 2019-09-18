{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.Go.Type where

import Prologue

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm
import Tags.Taggable (Taggable)

-- | A Bidirectional channel in Go (e.g. `chan`).
newtype BidirectionalChannel a = BidirectionalChannel { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically BidirectionalChannel

-- TODO: Implement Eval instance for BidirectionalChannel
instance Evaluatable BidirectionalChannel

-- | A Receive channel in Go (e.g. `<-chan`).
newtype ReceiveChannel a = ReceiveChannel { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ReceiveChannel

-- TODO: Implement Eval instance for ReceiveChannel
instance Evaluatable ReceiveChannel

-- | A Send channel in Go (e.g. `chan<-`).
newtype SendChannel a = SendChannel { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically SendChannel

-- TODO: Implement Eval instance for SendChannel
instance Evaluatable SendChannel
