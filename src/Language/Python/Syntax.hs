{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses #-}
module Language.Python.Syntax where

import Data.Abstract.Evaluatable
import Data.Abstract.FreeVariables
import Data.Align.Generic
import Data.Functor.Classes.Generic
import Data.Mergeable
import Diffing.Algorithm
import GHC.Generics

-- | Ellipsis (used in splice expressions and alternatively can be used as a fill in expression, like `undefined` in Haskell)
data Ellipsis a = Ellipsis
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Ellipsis where liftEq = genericLiftEq
instance Ord1 Ellipsis where liftCompare = genericLiftCompare
instance Show1 Ellipsis where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Ellipsis
instance Evaluatable Ellipsis


data Redirect a = Redirect !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Redirect where liftEq = genericLiftEq
instance Ord1 Redirect where liftCompare = genericLiftCompare
instance Show1 Redirect where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Redirect
instance Evaluatable Redirect
