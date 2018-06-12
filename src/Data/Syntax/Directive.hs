{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances #-}
module Data.Syntax.Directive where

import           Data.Abstract.Evaluatable
import           Data.Abstract.Module (ModuleInfo(..))
import qualified Data.Text as T
import           Data.JSON.Fields
import           Data.Span
import           Diffing.Algorithm
import           Prologue

-- A file directive like the Ruby constant `__FILE__`.
data File a = File
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 File where liftEq = genericLiftEq
instance Ord1 File where liftCompare = genericLiftCompare
instance Show1 File where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable File where
  eval File = rvalBox =<< (string . T.pack . modulePath <$> currentModule)


-- A line directive like the Ruby constant `__LINE__`.
data Line a = Line
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Line where liftEq = genericLiftEq
instance Ord1 Line where liftCompare = genericLiftCompare
instance Show1 Line where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Line where
  eval Line = rvalBox =<< (integer . fromIntegral . posLine . spanStart <$> currentSpan)
