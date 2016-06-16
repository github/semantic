{-# LANGUAGE DataKinds, GADTs, KindSignatures, MultiParamTypeClasses, TypeOperators #-}
module Data.Record where

import Data.Tagged
import Prologue

-- | A type-safe, extensible record structure.
data Record :: [*] -> * where
  RNil :: Record '[]
  RCons :: h -> Record t -> Record (h ': t)


infix 9 :=>

-- | A phantom type tag constructor.
type a :=> b = Tagged a b

-- | Smart constructor for type-tagged data fields.
-- |
-- | This has type a :=> b. When you require a to be some concrete type (and you usually will), it should be provided by context, whether using ascription, a type signature for the binding, `asTypeOf`, or some other way to allow the specific type to be inferred.
field :: b -> a :=> b
field = Tagged


-- Classes

-- | HasField enables indexing a Record by (phantom) type tags.
class HasField (fields :: [*]) (field :: *) where
  getField :: Record fields -> field


-- Instances

instance {-# OVERLAPPABLE #-} HasField fields field => HasField (notIt ': fields) field where
  getField (RCons _ t) = getField t

instance {-# OVERLAPPABLE #-} HasField (field ': fields) field where
  getField (RCons h _) = h
