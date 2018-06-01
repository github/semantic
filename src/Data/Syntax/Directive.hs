{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances #-}
module Data.Syntax.Directive where

import           Data.Abstract.Evaluatable
import           Data.Abstract.Module (ModuleInfo(..))
import qualified Data.ByteString.Char8 as BC
import           Data.JSON.Fields
import           Data.Span
import           Diffing.Algorithm
import           Prologue

-- A file directive like the Ruby constant `__FILE__`.
data File a = File
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, Mergeable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 File where liftEq = genericLiftEq
instance Ord1 File where liftCompare = genericLiftCompare
instance Show1 File where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable File where
  eval File = Rval . string . BC.pack . modulePath <$> currentModule


-- A line directive like the Ruby constant `__LINE__`.
data Line a = Line
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, Mergeable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 Line where liftEq = genericLiftEq
instance Ord1 Line where liftCompare = genericLiftCompare
instance Show1 Line where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Line where
  eval Line = Rval . integer . fromIntegral . posLine . spanStart <$> currentSpan
