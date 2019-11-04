{-# LANGUAGE DeriveTraversable, FlexibleContexts, GeneralizedNewtypeDeriving, QuantifiedConstraints, StandaloneDeriving #-}
module Analysis.Intro
( unit
, bool
, string
, record
, lam
, lams
, unlam
, Intro(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Data.String (IsString)
import Data.Text (Text)
import Syntax.Module
import Syntax.Scope
import Syntax.Term

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
  deriving (Foldable, Functor, Traversable)

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Intro f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Intro f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (Intro f a)


-- | User-specified and -relevant names.
newtype Name = Name { unName :: Text }
  deriving (Eq, IsString, Ord, Show)
