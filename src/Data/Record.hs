{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
module Data.Record where

import Prologue

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


-- Families

type family ValueOf field


-- Classes

-- | HasField enables indexing a Record by (phantom) type tags.
class HasField (fields :: [*]) (field :: *) where
  getField :: Record fields -> field

class SetField (fields :: [*]) (field :: *) where
  setField :: Record fields -> field -> Record fields

class IsField field where
  getValue :: field -> ValueOf field
  setValue :: ValueOf field -> field


-- Instances

-- OVERLAPPABLE is required for the HasField instances so that we can handle the two cases: either the head of the non-empty h-list is the requested field, or it isn’t. The third possible case (the h-list is empty) is rejected at compile-time.

instance {-# OVERLAPPABLE #-} HasField fields field => HasField (notIt ': fields) field where
  getField (RCons _ t) = getField t

instance {-# OVERLAPPABLE #-} HasField (field ': fields) field where
  getField (RCons h _) = h

instance {-# OVERLAPPABLE #-} SetField fields field => SetField (notIt ': fields) field where
  setField (RCons h t) f = RCons h (setField t f)

instance {-# OVERLAPPABLE #-} SetField (field ': fields) field where
  setField (RCons _ t) f = RCons f t


instance (Show h, Show (Record t)) => Show (Record (h ': t)) where
  showsPrec n (RCons h t) = showsPrec n h . (" : " <>) . showsPrec n t

instance Show (Record '[]) where
  showsPrec _ RNil = ("'[]" <>)
