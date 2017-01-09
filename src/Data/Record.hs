{-# LANGUAGE DataKinds, GADTs, KindSignatures, MultiParamTypeClasses, TypeOperators, ConstraintKinds #-}
module Data.Record where

import Category
import Data.Aeson
import Data.Aeson.Types
import Data.Functor.Listable
import GHC.Show
import Prologue
import Range
import SourceSpan

-- | A type alias for HasField constraints commonly used throughout semantic-diff.
type DefaultFields fields = (HasField fields Category, HasField fields Range, HasField fields SourceSpan)

-- | A type-safe, extensible record structure.
-- |
-- | This is heavily inspired by Aaron Levin’s [Extensible Effects in the van Laarhoven Free Monad](http://aaronlevin.ca/post/136494428283/extensible-effects-in-the-van-laarhoven-free-monad).
data Record :: [*] -> * where
  RNil :: Record '[]
  RCons :: h -> Record t -> Record (h ': t)

infixr 0 .:

-- | Infix synonym for `RCons`: `a .: b .: RNil == RCons a (RCons b RNil)`.
(.:) :: h -> Record t -> Record (h ': t)
(.:) = RCons

-- | Get the first element of a non-empty record.
rhead :: Record (head ': tail) -> head
rhead (RCons head _) = head

-- | Get the first element of a non-empty record.
rtail :: Record (head ': tail) -> Record tail
rtail (RCons _ tail) = tail


-- Classes

-- | HasField enables indexing a Record by (phantom) type tags.
class HasField (fields :: [*]) (field :: *) where
  getField :: Record fields -> field
  setField :: Record fields -> field -> Record fields


-- Instances

-- OVERLAPPABLE is required for the HasField instances so that we can handle the two cases: either the head of the non-empty h-list is the requested field, or it isn’t. The third possible case (the h-list is empty) is rejected at compile-time.

instance {-# OVERLAPPABLE #-} HasField fields field => HasField (notIt ': fields) field where
  getField (RCons _ t) = getField t
  setField (RCons h t) f = RCons h (setField t f)

instance {-# OVERLAPPABLE #-} HasField (field ': fields) field where
  getField (RCons h _) = h
  setField (RCons _ t) f = RCons f t


instance (Show h, Show (Record t)) => Show (Record (h ': t)) where
  showsPrec n (RCons h t) = showParen (n > 0) $ showsPrec 1 h . (" .: " <>) . shows t

instance Show (Record '[]) where
  showsPrec n RNil = showParen (n > 0) ("RNil" <>)

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (Record (a ': b ': c ': d ': '[])) where
  toJSON (RCons a (RCons b (RCons c (RCons d RNil)))) = toJSONList [toJSON a, toJSON b, toJSON c, toJSON d]

instance ToJSON (Record '[]) where
  toJSON _ = emptyArray


instance (Eq h, Eq (Record t)) => Eq (Record (h ': t)) where
  RCons h1 t1 == RCons h2 t2 = h1 == h2 && t1 == t2

instance Eq (Record '[]) where
  _ == _ = True


instance (Ord h, Ord (Record t)) => Ord (Record (h ': t)) where
  RCons h1 t1 `compare` RCons h2 t2 = let h = h1 `compare` h2 in
    if h == EQ then t1 `compare` t2 else h

instance Ord (Record '[]) where
  _ `compare` _ = EQ


instance (Listable head, Listable (Record tail)) => Listable (Record (head ': tail)) where
  tiers = cons2 RCons

instance Listable (Record '[]) where
  tiers = cons0 RNil
