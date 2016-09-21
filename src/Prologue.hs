module Prologue
( module X
, lookup
, last
) where

import Protolude as X
import Data.List (lookup, last)

import Control.Comonad.Trans.Cofree as X
import Control.Monad.Trans.Free as X
import Control.Comonad as X
