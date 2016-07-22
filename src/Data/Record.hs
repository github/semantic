{-# LANGUAGE DataKinds, GADTs, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators #-}
module Data.Record
( Record(RNil)
, (.:)
, HasField(..)
, maybeGetField
, updateField
) where

import GHC.Show
import Prologue
import Test.QuickCheck

-- | A type-safe, extensible record structure.
-- |
-- | This is heavily inspired by Aaron Levin’s [Extensible Effects in the van Laarhoven Free Monad](http://aaronlevin.ca/post/136494428283/extensible-effects-in-the-van-laarhoven-free-monad).
data Record :: [*] -> * where
  RNil :: Record '[]
  RCons :: Typeable h => h -> Record t -> Record (h ': t)
  deriving Typeable

infixr 0 .:

-- | Infix synonym for `RCons`: `a .: b .: RNil == RCons a (RCons b RNil)`.
(.:) :: Typeable h => h -> Record t -> Record (h ': t)
(.:) = RCons

-- | Return 'Just' a 'field', if it exists in a record. Otherwise, return 'Nothing'.
maybeGetField :: Typeable field => Record fields -> Maybe field
maybeGetField (RCons h t) = cast h <|> maybeGetField t
maybeGetField RNil = Nothing

-- | Update (replace the value of) 'field' in a record, if it exists. Otherwise, return the record unchanged.
updateField :: forall field fields. Typeable field => Record fields -> field -> Record fields
updateField record a = case record of
  RNil -> RNil
  cons@(RCons _ _) -> updateRCons cons a

-- | Update (replace the value of) 'field' in a non-empty record, if it exists. Otherwise return the record unchanged.
updateRCons :: forall h t field. Typeable field => Record (h ': t) -> field -> Record (h ': t)
updateRCons (RCons h t) a = case eqT :: Maybe (h :~: field) of
  Just Refl -> RCons a t
  Nothing -> RCons h (updateField t a)


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


instance (Eq h, Eq (Record t)) => Eq (Record (h ': t)) where
  RCons h1 t1 == RCons h2 t2 = h1 == h2 && t1 == t2

instance Eq (Record '[]) where
  _ == _ = True


instance (Ord h, Ord (Record t)) => Ord (Record (h ': t)) where
  RCons h1 t1 `compare` RCons h2 t2 = let h = h1 `compare` h2 in
    if h == EQ then t1 `compare` t2 else h

instance Ord (Record '[]) where
  _ `compare` _ = EQ


instance (Typeable field, Arbitrary field, Arbitrary (Record fields)) => Arbitrary (Record (field ': fields)) where
  arbitrary = RCons <$> arbitrary <*> arbitrary

  shrink (RCons h t) = RCons <$> shrink h <*> shrink t

instance Arbitrary (Record '[]) where
  arbitrary = pure RNil

  shrink _ = []
