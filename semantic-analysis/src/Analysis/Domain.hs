{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, QuantifiedConstraints, StandaloneDeriving #-}
module Analysis.Domain
( unit
, Domain(..)
) where

import Control.Effect.Carrier
import Data.String (IsString)
import Data.Text (Text)
import Syntax.Scope

unit :: (Carrier sig m, Member Domain sig) => m a
unit = send Unit


data Domain f a
  = Unit
  | Bool Bool
  | String Text
  | Record [(Name, f a)]
  | Lam (Maybe Name) (Scope () f a)

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Domain f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Domain f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (Domain f a)


-- | User-specified and -relevant names.
newtype Name = Name { unName :: Text }
  deriving (Eq, IsString, Ord, Show)
