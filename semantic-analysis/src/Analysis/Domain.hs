{-# LANGUAGE QuantifiedConstraints, StandaloneDeriving #-}
module Analysis.Domain
( Domain(..)
) where

import Data.Text (Text)
import Syntax.Scope

data Domain name f a
  = Unit
  | Bool Bool
  | String Text
  | Record [(name, f a)]
  | Lam name (Scope () f a)

deriving instance (Eq   name, Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Domain name f a)
deriving instance (Ord  name, Ord  a, forall a . Eq   a => Eq   (f a)
                                    , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Domain name f a)
deriving instance (Show name, Show a, forall a . Show a => Show (f a))          => Show (Domain name f a)
