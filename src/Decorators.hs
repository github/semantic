{-# LANGUAGE DataKinds, TypeOperators #-}
module Decorators
( ConstructorLabel(..)
, constructorLabelWithSource
, constructorLabel
) where

import Data.Aeson
import Data.Functor.Classes (Show1 (liftShowsPrec))
import Data.Union
import Data.String
import GHC.Generics
import Prologue
import Renderer.JSON
import Term
import Text.Show


-- | Compute a 'ByteString' label for a 'Show1'able 'Term'.
--
--   This uses 'liftShowsPrec' to produce the 'ByteString', with the effect that
--   constant fields will be included and parametric fields will not be.
constructorLabelWithSource :: Show1 f => TermF f a b -> ByteString
constructorLabelWithSource (_ :< f) = toS (liftShowsPrec (const (const identity)) (const identity) 0 f "")

-- | Compute a 'ByteString' label for a 'Union' of syntax 'Term's.
constructorLabel :: ConstructorName f => TermF f a b -> ByteString
constructorLabel (_ :< f) = toS (constructorName f)


newtype ConstructorLabel = ConstructorLabel ByteString

instance Show ConstructorLabel where
  showsPrec _ (ConstructorLabel s) = showString (toS s)

instance ToJSONFields ConstructorLabel where
  toJSONFields (ConstructorLabel s) = [ "category" .= (toS s :: Text) ]


class ConstructorName f where
  constructorName :: f a -> String

instance (Generic1 f, ConstructorName (Rep1 f), ConstructorName (Union fs)) => ConstructorName (Union (f ': fs)) where
  constructorName union = case decompose union of
    Left rest -> constructorName rest
    Right f -> constructorName (from1 f)

instance ConstructorName (Union '[]) where
  constructorName _ = ""

instance ConstructorName f => ConstructorName (M1 D c f) where
  constructorName = constructorName . unM1

instance Constructor c => ConstructorName (M1 C c f) where
  constructorName = conName

instance (ConstructorName f, ConstructorName g) => ConstructorName (f :+: g) where
  constructorName (L1 l) = constructorName l
  constructorName (R1 r) = constructorName r
