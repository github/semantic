module Prologue
( module X
, lookup
, (&&&)
, (***)
, hylo, cata, para, ana
, module Data.Hashable
) where

import Protolude as X
import Data.List (lookup)

import Control.Comonad.Trans.Cofree as X
import Control.Monad.Trans.Free as X
import Control.Comonad as X

import Control.Arrow ((&&&), (***))

import Data.Functor.Foldable (hylo, cata, para, ana)

import Data.Hashable
