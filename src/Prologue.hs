module Prologue (
  module X
, ) where

import Data.Union as X
import Data.Function as X (fix, on, (&))
import Data.Functor.Foldable as X (Base, Recursive(..), Corecursive(..))
import Data.Functor.Classes as X
import Data.Semigroup as X
import Control.Applicative as X
import Data.Maybe as X
import Data.Monoid as X (Alt(..))
import Data.Pointed as X

import Control.Monad as X hiding (fail, return)
import Control.Monad.Fail as X (MonadFail(..))
import Control.Monad.Except as X (MonadError(..))

import GHC.Stack as X

import Data.Ix as X (Ix(..))

import Data.Hashable as X (
    Hashable
  , hash
  , hashWithSalt
  , hashUsing
  )

import Data.These as X

-- Data Types
import Data.Map as X (Map)
import Data.Set as X (Set)
import Data.Sequence as X (Seq)
import Data.IntMap as X (IntMap)
import Data.IntSet as X (IntSet)

import Data.Functor.Classes.Generic as X

import Data.Proxy as X ( Proxy(..) )

import Data.Foldable as X hiding (product , sum)
import Data.Traversable as X
import Control.Arrow as X ((&&&), (***))

import Data.Functor.Both (Both)

import Data.List.NonEmpty as X (
    NonEmpty(..)
  , nonEmpty
  )

import Data.Algebra as X
import Data.Bifunctor as X (Bifunctor(..))
import Data.Bifunctor.Join as X

import Data.ByteString as X (ByteString)

import Data.Align.Generic as X (GAlign)
import Data.Mergeable as X (Mergeable)

import Data.Bifoldable as X
import Data.Bitraversable as X

-- Generics
import GHC.Generics as X
