{-# LANGUAGE DataKinds, TypeOperators #-}
module Analysis.Decorator
( decoratorWithAlgebra
) where

import Prologue
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
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
