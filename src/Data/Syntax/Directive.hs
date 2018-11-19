{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Syntax.Directive where

import Prologue

import qualified Data.Text as T
import           Proto3.Suite.Class

import Data.Abstract.Evaluatable
import Data.Abstract.Module (ModuleInfo (..))
import Data.JSON.Fields
import Data.Span
import Diffing.Algorithm
import Reprinting.Tokenize

-- A file directive like the Ruby constant `__FILE__`.
data File a = File
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 File where liftEq = genericLiftEq
instance Ord1 File where liftCompare = genericLiftCompare
instance Show1 File where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable File where
  eval _ File = rvalBox =<< (string . T.pack . modulePath <$> currentModule)

-- We may need a separate token class for these given additional languages
instance Tokenize File where
  tokenize _ = yield . Run $ "__FILE__"


-- A line directive like the Ruby constant `__LINE__`.
data Line a = Line
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Line where liftEq = genericLiftEq
instance Ord1 Line where liftCompare = genericLiftCompare
instance Show1 Line where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Line where
  eval _ Line = rvalBox =<< (integer . fromIntegral . posLine . spanStart <$> currentSpan)

-- PT TODO: proper token for this
instance Tokenize Line where
  tokenize _ = yield . Run $ "__FILE__"
