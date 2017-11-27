{-# LANGUAGE DataKinds, TypeOperators #-}
module Analysis.Decorator
( decoratorWithAlgebra
, syntaxIdentifierAlgebra
, constructorNameAndConstantFields
) where

import Data.Aeson
import Data.Algebra
import Data.Bifunctor (second)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Foldable (asum)
import Data.Functor.Classes (Show1 (liftShowsPrec))
import Data.Functor.Foldable
import Data.JSON.Fields
import Data.Record
import Data.Term
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Syntax as S

-- | Lift an algebra into a decorator for terms annotated with records.
decoratorWithAlgebra :: Functor syntax
                     => RAlgebra (Term syntax (Record fs)) a -- ^ An R-algebra on terms.
                     -> Term syntax (Record fs)              -- ^ A term to decorate with values produced by the R-algebra.
                     -> Term syntax (Record (a ': fs))       -- ^ A term decorated with values produced by the R-algebra.
decoratorWithAlgebra alg = para $ \ c@(In a f) -> termIn (alg (fmap (second (rhead . termAnnotation)) c) :. a) (fmap snd f)


newtype Identifier = Identifier ByteString
  deriving (Eq, Show)

instance ToJSONFields Identifier where
  toJSONFields (Identifier i) = [ "identifier" .= decodeUtf8 i ]

syntaxIdentifierAlgebra :: RAlgebra (Term S.Syntax a) (Maybe Identifier)
syntaxIdentifierAlgebra (In _ syntax) = case syntax of
  S.Assignment f _ -> identifier f
  S.Class f _ _ -> identifier f
  S.Export f _ -> f >>= identifier
  S.Function f _ _ -> identifier f
  S.FunctionCall f _ _ -> identifier f
  S.Import f _ -> identifier f
  S.Method _ f _ _ _ -> identifier f
  S.MethodCall _ f _ _ -> identifier f
  S.Module f _ -> identifier f
  S.OperatorAssignment f _ -> identifier f
  S.SubscriptAccess f _  -> identifier f
  S.TypeDecl f _ -> identifier f
  S.VarAssignment f _ -> asum $ identifier <$> f
  _ -> Nothing
  where identifier = fmap (Identifier . encodeUtf8) . S.extractLeafValue . termOut . fst

-- | Compute a 'ByteString' label for a 'Show1'able 'Term'.
--
--   This uses 'liftShowsPrec' to produce the 'ByteString', with the effect that
--   constant fields will be included and parametric fields will not be.
constructorNameAndConstantFields :: Show1 f => TermF f a b -> ByteString
constructorNameAndConstantFields (In _ f) = pack (liftShowsPrec (const (const id)) (const id) 0 f "")
