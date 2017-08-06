{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Data.Error where

import Data.Span
import GHC.Stack

data Error grammar = HasCallStack => Error { errorSpan :: Span, errorExpected :: [grammar], errorActual :: Maybe grammar }

deriving instance Eq grammar => Eq (Error grammar)
deriving instance Foldable Error
deriving instance Functor Error
deriving instance Show grammar => Show (Error grammar)
deriving instance Traversable Error

errorCallStack :: Error grammar -> CallStack
errorCallStack Error{} = callStack
