module Prologue
( module X
, lookup
, traceShowId
, FilePath
) where

import Protolude as X
import Data.List (lookup)
import System.IO (FilePath)

import qualified GHC.Show as P
import qualified Debug.Trace as T

{-# WARNING traceShowId "'traceShowId' remains in code" #-}
traceShowId :: P.Show a => a -> a
traceShowId a = T.trace (P.show a) a
