{-# LANGUAGE DataKinds, DeriveGeneric, DeriveTraversable, FlexibleContexts, GeneralizedNewtypeDeriving, QuantifiedConstraints, StandaloneDeriving #-}
module Analysis.Intro
( unit
, bool
, string
, record
, lam
, lamFin
, lams
, unlam
, Intro(..)
, Name(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic1)
import Syntax.Fin
import Syntax.Module
import Syntax.Scope
import Syntax.Term
import Syntax.Var

unit :: (Carrier sig m, Member Intro sig) => m a
unit = send Unit

bool :: (Carrier sig m, Member Intro sig) => Bool -> m a
bool = send . Bool

string :: (Carrier sig m, Member Intro sig) => Text -> m a
string = send . String

record :: (Carrier sig m, Member Intro sig) => [(Name, m a)] -> m a
record fs = send (Record fs)

lam :: (Eq a, Carrier sig m, Member Intro sig) => Maybe Name -> a -> m a -> m a
lam u n b = send (Lam u (abstract1 n b))

lamFin :: (Carrier sig m, Member Intro sig) => Maybe Name -> m (Var (Fin ('S n)) a) -> m (Var (Fin n) a)
lamFin u b = send (Lam u (toScopeFin b))

lams :: (Eq a, Foldable t, Carrier sig m, Member Intro sig) => t (Maybe Name, a) -> m a -> m a
lams names body = foldr (uncurry lam) body names

unlam :: (Alternative m, Member Intro sig, RightModule sig) => a -> Term sig a -> m (Maybe Name, a, Term sig a)
unlam n (Alg sig) | Just (Lam n' b) <- prj sig = pure (n', n, instantiate1 (pure n) b)
unlam _ _                                      = empty


data Intro f a
  = Unit
  | Bool Bool
  | String Text
  | Record [(Name, f a)]
  | Lam (Maybe Name) (Scope () f a)
  deriving (Foldable, Functor, Generic1, Traversable)

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Intro f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Intro f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (Intro f a)

instance HFunctor Intro

instance RightModule Intro where
  Unit       >>=* _ = Unit
  Bool b     >>=* _ = Bool b
  String s   >>=* _ = String s
  Record fs  >>=* f = Record (map (fmap (>>= f)) fs)
  Lam n b    >>=* f = Lam n (b >>=* f)


-- | User-specified and -relevant names.
newtype Name = Name { unName :: Text }
  deriving (Eq, IsString, Ord, Show)
