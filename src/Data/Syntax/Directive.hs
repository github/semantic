{-# LANGUAGE DeriveAnyClass, DerivingVia, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances #-}
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
  deriving (Eq1, Show1, Ord1) via Generically File

instance Evaluatable File where
  eval _ _ File = currentModule >>= string . T.pack . modulePath

-- We may need a separate token class for these given additional languages
instance Tokenize File where
  tokenize _ = yield . Run $ "__FILE__"


-- A line directive like the Ruby constant `__LINE__`.
data Line a = Line
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Line

instance Evaluatable Line where
  eval _ _ Line = currentSpan >>= integer . fromIntegral . posLine . spanStart

-- PT TODO: proper token for this
instance Tokenize Line where
  tokenize _ = yield . Run $ "__FILE__"
