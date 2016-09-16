module Prologue
( module X
, lookup
, traceShowId
, (&&&)
, (***)
, hylo, cata, para
, module Data.Hashable
) where

import Protolude as X
import Data.List (lookup)

import Control.Comonad.Trans.Cofree as X
import Control.Monad.Trans.Free as X
import Control.Comonad as X

import qualified GHC.Show as P
import qualified Debug.Trace as T

import Control.Arrow ((&&&), (***))

import Data.Functor.Foldable (hylo, cata, para)

import Data.Hashable

{-# WARNING traceShowId "'traceShowId' remains in code" #-}
traceShowId :: P.Show a => a -> a
traceShowId a = T.trace (P.show a) a
