{-# LANGUAGE DeriveAnyClass #-}
module Language.Haskell.Syntax where

import           Data.Abstract.Evaluatable
import           Data.JSON.Fields
import           Diffing.Algorithm
import           Prelude
import           Prologue

data Module a = Module { moduleIdentifier :: !a
                       , moduleExports    :: ![a]
                       , moduleStatements :: !a
                       }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, Mergeable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module where
