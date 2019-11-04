{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, QuantifiedConstraints, StandaloneDeriving #-}
module Analysis.Domain
( unit
, bool
, string
, record
, lam
, lams
, unlam
, Domain(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Data.String (IsString)
import Data.Text (Text)
import Syntax.Module
import Syntax.Scope
import Syntax.Term

unit :: (Carrier sig m, Member Domain sig) => m a
unit = send Unit

bool :: (Carrier sig m, Member Domain sig) => Bool -> m a
bool = send . Bool

string :: (Carrier sig m, Member Domain sig) => Text -> m a
string = send . String

record :: (Carrier sig m, Member Domain sig) => [(Name, m a)] -> m a
record fs = send (Record fs)

lam :: (Eq a, Carrier sig m, Member Domain sig) => Maybe Name -> a -> m a -> m a
lam u n b = send (Lam u (abstract1 n b))

lams :: (Eq a, Foldable t, Carrier sig m, Member Domain sig) => t (Maybe Name, a) -> m a -> m a
lams names body = foldr (uncurry lam) body names

unlam :: (Alternative m, Member Domain sig, RightModule sig) => a -> Term sig a -> m (Maybe Name, a, Term sig a)
unlam n (Alg sig) | Just (Lam n' b) <- prj sig = pure (n', n, instantiate1 (pure n) b)
unlam _ _                                      = empty


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
