{-# LANGUAGE DataKinds, GADTs, KindSignatures, MultiParamTypeClasses, TypeOperators #-}
module Data.Record where

import Data.Tagged
import Prologue

data Record :: [*] -> * where
  RNil :: Record '[]
  RCons :: h -> Record t -> Record (h ': t)

class HasField (fields :: [*]) (field :: *) where
  getField :: Record fields -> field


infix 9 :=>

-- | A phantom type tag constructor.
type a :=> b = Tagged a b

-- | Smart constructor for type-tagged data fields.
-- |
-- | This has type a :=> b. When you require a to be some concrete type (and you usually will), it should be provided by context, whether using ascription, a type signature for the binding, `asTypeOf`, or some other way to allow the specific type to be inferred.
field :: b -> a :=> b
field = Tagged
