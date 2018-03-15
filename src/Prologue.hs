{-# LANGUAGE UndecidableInstances #-}
module Prologue
  ( module X
  , maybeM
  , maybeFail
  ) where


import Data.Bifunctor.Join as X
import Data.ByteString as X (ByteString)
import Data.Functor.Both as X (Both, runBothWith, both)
import Data.IntMap as X (IntMap)
import Data.IntSet as X (IntSet)
import Data.Ix as X (Ix(..))
import Data.Map as X (Map)
import Data.Maybe as X
import Data.Sequence as X (Seq)
import Data.Set as X (Set)
import Data.Text as X (Text)
import Data.These as X
import Data.Union as X
import Data.List.NonEmpty as X (
    NonEmpty(..)
  , nonEmpty
  , some1
  )

import Debug.Trace as X

import Control.Exception as X hiding (
    evaluate
  , throw
  , throwIO
  , throwTo
  , assert
  , Handler(..)
  )

-- Typeclasses
import Control.Applicative as X
import Control.Arrow as X ((&&&), (***))
import Control.Monad as X hiding (fail, return, unless, when)
import Control.Monad.Except as X (MonadError(..))
import Control.Monad.Fail as X (MonadFail(..))
import Data.Algebra as X
import Data.Align.Generic as X (GAlign)
import Data.Bifoldable as X
import Data.Bifunctor as X (Bifunctor(..))
import Data.Bitraversable as X
import Data.Foldable as X hiding (product , sum)
import Data.Functor as X (void)
import Data.Function as X (fix, on, (&))
import Data.Functor.Classes as X
import Data.Functor.Classes.Generic as X
import Data.Functor.Foldable as X (Base, Recursive(..), Corecursive(..))
import Data.Mergeable as X (Mergeable)
import Data.Monoid as X (Monoid(..), First(..), Last(..))
import Data.Proxy as X (Proxy(..))
import Data.Semigroup as X (Semigroup(..))
import Data.Traversable as X
import Data.Typeable as X (Typeable)
import Data.Hashable as X (
    Hashable
  , hash
  , hashWithSalt
  , hashUsing
  )

-- Generics
import GHC.Generics as X hiding (moduleName)
import GHC.Stack as X

-- Extract the 'Just' of a Maybe in an Applicative context or, given Nothing, run the provided action.
maybeM :: Applicative f => f a -> Maybe a -> f a
maybeM f = maybe f pure

-- Either extract the 'Just' of a Maybe or invoke `fail` with the provided string.
maybeFail :: MonadFail m => String -> Maybe a -> m a
maybeFail s = maybeFail (X.fail s)
