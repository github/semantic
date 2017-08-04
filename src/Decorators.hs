{-# LANGUAGE DataKinds, TypeOperators #-}
module Decorators
( ConstructorLabel(..)
, constructorNameAndConstantFields
, constructorLabel
) where

import Data.Aeson
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Functor.Classes (Show1 (liftShowsPrec))
import Data.Text.Encoding (decodeUtf8)
import Data.Union
import GHC.Generics
import Renderer.JSON
import Term

-- | Compute a 'ByteString' label for a 'Show1'able 'Term'.
--
--   This uses 'liftShowsPrec' to produce the 'ByteString', with the effect that
--   constant fields will be included and parametric fields will not be.
constructorNameAndConstantFields :: Show1 f => TermF f a b -> ByteString
constructorNameAndConstantFields (_ :< f) = pack (liftShowsPrec (const (const id)) (const id) 0 f "")

-- | Compute a 'ConstructorLabel' label for a 'Union' of syntax 'Term's.
constructorLabel :: ConstructorName f => TermF f a b -> ConstructorLabel
constructorLabel (_ :< f) = ConstructorLabel $ pack (constructorName f)


newtype ConstructorLabel = ConstructorLabel ByteString

instance Show ConstructorLabel where
  showsPrec _ (ConstructorLabel s) = showString (unpack s)

instance ToJSONFields ConstructorLabel where
  toJSONFields (ConstructorLabel s) = [ "category" .= decodeUtf8 s ]


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

instance (ConstructorName f, ConstructorName g) => ConstructorName (f :+: g) where
  constructorName (L1 l) = constructorName l
  constructorName (R1 r) = constructorName r

instance Constructor c => ConstructorName (M1 C c f) where
  constructorName x = case conName x of
                        ":" -> ""
                        n -> n
