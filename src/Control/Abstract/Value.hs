{-# LANGUAGE MultiParamTypeClasses, Rank2Types, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Value
( MonadValue(..)
, Comparator(..)
, while
, doWhile
, forLoop
, toBool
) where

import Control.Abstract.Addressable
import Control.Abstract.Analysis
import qualified Data.Abstract.Environment as Env
import Data.Abstract.FreeVariables
import Data.Abstract.Number as Number
import Data.Abstract.Type as Type
import Data.Abstract.Value as Value
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import Prelude hiding (fail)
import Prologue
