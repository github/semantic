{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
module Analysis.Intro
( Intro(..)
, unit
, bool
, string
, lam
, record
) where

import Analysis.Functor.Named
import Control.Algebra
import Data.Text (Text)
import GHC.Generics (Generic1)
import Syntax.Foldable
import Syntax.Module
import Syntax.Scope
import Syntax.Traversable

data Intro t a
  = Unit
  | Bool Bool
  | String Text
  | Lam (Named (Scope () t a))
  | Record [(Name, t a)]
  deriving (Foldable, Functor, Generic1, Traversable)

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Intro f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Intro f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (Intro f a)

instance HFunctor Intro
instance HFoldable Intro
instance HTraversable Intro

instance RightModule Intro where
  Unit     >>=* _ = Unit
  Bool b   >>=* _ = Bool b
  String s >>=* _ = String s
  Lam b    >>=* f = Lam ((>>=* f) <$> b)
  Record t >>=* f = Record (map (fmap (>>= f)) t)


unit :: Has Intro sig m => m a
unit = send Unit

bool :: Has Intro sig m => Bool -> m a
bool = send . Bool

string :: Has Intro sig m => Text -> m a
string = send . String


lam :: (Eq a, Has Intro sig m) => Named a -> m a -> m a
lam (Named u n) b = send (Lam (Named u (abstract1 n b)))


record :: Has Intro sig m => [(Name, m a)] -> m a
record = send . Record
