{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Analysis.Exception
( Exception(..)
, ExcSet(..)
, var
, exc
  -- * Exception tracing analysis
, ExcC(..)
) where

import           Analysis.Name
import           Control.Applicative (Alternative)
import qualified Data.Set as Set

newtype Exception = Exception { exceptionName :: String }
  deriving (Eq, Ord, Show)

-- | Sets whose elements are each a variable or an exception.
newtype ExcSet = ExcSet { values :: Set.Set (Either Name Exception) }
  deriving (Eq, Monoid, Semigroup, Ord, Show)

var :: Name -> ExcSet
var = ExcSet . Set.singleton . Left

exc :: Exception -> ExcSet
exc = ExcSet . Set.singleton . Right


newtype ExcC m a = ExcC { runExcC :: m a }
  deriving (Alternative, Applicative, Functor, Monad)
