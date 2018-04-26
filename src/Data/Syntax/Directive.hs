{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances #-}
module Data.Syntax.Directive where

import           Data.Abstract.Evaluatable
import           Data.Abstract.Module (ModuleInfo(..))
import qualified Data.ByteString.Char8 as BC
import           Diffing.Algorithm
import           Prologue

data FileDirective a = FileDirective
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 FileDirective where liftEq = genericLiftEq
instance Ord1 FileDirective where liftCompare = genericLiftCompare
instance Show1 FileDirective where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FileDirective where
  eval FileDirective{} = currentModule >>= string . BC.pack . modulePath
