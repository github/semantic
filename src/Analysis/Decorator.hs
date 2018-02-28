{-# LANGUAGE DataKinds, TypeOperators #-}
module Analysis.Decorator
( decoratorWithAlgebra
, constructorNameAndConstantFields
) where

import Prologue
import Data.Aeson
import Data.ByteString.Char8 (ByteString, pack)
import Data.JSON.Fields
import Data.Record
import Data.Term
import Data.Text.Encoding (decodeUtf8)

-- | Lift an algebra into a decorator for terms annotated with records.
decoratorWithAlgebra :: Functor syntax
                     => RAlgebra (TermF syntax (Record fs)) (Term syntax (Record fs)) a -- ^ An R-algebra on terms.
                     -> Term syntax (Record fs)                                         -- ^ A term to decorate with values produced by the R-algebra.
                     -> Term syntax (Record (a ': fs))                                  -- ^ A term decorated with values produced by the R-algebra.
decoratorWithAlgebra alg = para $ \ c@(In a f) -> termIn (alg (fmap (second (rhead . termAnnotation)) c) :. a) (fmap snd f)


newtype Identifier = Identifier ByteString
  deriving (Eq, Show)

instance ToJSONFields Identifier where
  toJSONFields (Identifier i) = [ "identifier" .= decodeUtf8 i ]

-- | Compute a 'ByteString' label for a 'Show1'able 'Term'.
--
--   This uses 'liftShowsPrec' to produce the 'ByteString', with the effect that
--   constant fields will be included and parametric fields will not be.
constructorNameAndConstantFields :: Show1 f => TermF f a b -> ByteString
constructorNameAndConstantFields (In _ f) = pack (liftShowsPrec (const (const id)) (const id) 0 f "")
