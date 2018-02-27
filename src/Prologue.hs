module Prologue (
  module X
, ) where

import Data.Union as X
import Data.Function as X (fix, on, (&))
import Data.Functor.Foldable as X (Base, Recursive(..))
import Data.Semigroup as X
import Control.Applicative as X
import Data.Maybe as X
import Data.Monoid as X (Alt(..))
import Data.Pointed as X

-- Data Types
import Data.Map as X (Map)
import Data.Set as X (Set)
import Data.Sequence as X (Seq)
import Data.IntMap as X (IntMap)
import Data.IntSet as X (IntSet)


import Data.Proxy as X ( Proxy(..) )

-- Generics
import GHC.Generics as X (
    Generic(..)
  , Generic1
  , Rep
  , D
  , C
  , Rep1
  , from1
  , K1(..)
  , M1(..)
  , U1(..)
  , V1
  , D1
  , C1
  , S1
  , (:+:)(..)
  , (:*:)(..)
  , (:.:)(..)
  , Rec0
  , Constructor(..)
  , Datatype(..)
  , Selector(..)
  , Fixity(..)
  , Associativity(..)
  , Meta(..)
  , FixityI(..)
  , URec
  )
