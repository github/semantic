{-# LANGUAGE GeneralizedNewtypeDeriving, QuantifiedConstraints, StandaloneDeriving #-}
module Analysis.Domain
( Domain(..)
) where

import Data.String (IsString)
import Data.Text (Text)
import Syntax.Scope

data Domain f a
  = Unit
  | Bool Bool
  | String Text
  | Record [(Name, f a)]
  | Lam Name (Scope () f a)

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Domain f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Domain f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (Domain f a)


-- | User-specified and -relevant names.
newtype Name = Name { unName :: Text }
  deriving (Eq, IsString, Ord, Show)
