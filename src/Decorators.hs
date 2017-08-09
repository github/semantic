{-# LANGUAGE DataKinds, TypeOperators, UndecidableInstances #-}
module Decorators
( ConstructorLabel(..)
, constructorNameAndConstantFields
, constructorLabel
) where

import Data.Aeson
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Functor.Classes (Show1 (liftShowsPrec))
import Data.Proxy
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
constructorLabel :: Apply1 ConstructorName fs => TermF (Union fs) a b -> ConstructorLabel
constructorLabel (_ :< u) = ConstructorLabel $ pack (apply1 (Proxy :: Proxy ConstructorName) constructorName u)


newtype ConstructorLabel = ConstructorLabel ByteString

instance Show ConstructorLabel where
  showsPrec _ (ConstructorLabel s) = showString (unpack s)

instance ToJSONFields ConstructorLabel where
  toJSONFields (ConstructorLabel s) = [ "category" .= decodeUtf8 s ]


class ConstructorName f where
  constructorName :: f a -> String

instance (Generic1 f, GConstructorName (Rep1 f)) => ConstructorName f where
  constructorName = gconstructorName . from1


class GConstructorName f where
  gconstructorName :: f a -> String

instance GConstructorName f => GConstructorName (M1 D c f) where
  gconstructorName = gconstructorName . unM1

instance (GConstructorName f, GConstructorName g) => GConstructorName (f :+: g) where
  gconstructorName (L1 l) = gconstructorName l
  gconstructorName (R1 r) = gconstructorName r

instance Constructor c => GConstructorName (M1 C c f) where
  gconstructorName x = case conName x of
                        ":" -> ""
                        n -> n
