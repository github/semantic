{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeOperators, UndecidableInstances #-}
module Data.Syntax.Algebra
( FAlgebra
, RAlgebra
, fToR
, decoratorWithAlgebra
, identifierAlgebra
, cyclomaticComplexityAlgebra
, ConstructorName(..)
, ConstructorLabel(..)
, constructorNameAndConstantFields
, constructorLabel
) where

import Data.Aeson
import Data.Bifunctor (second)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Functor.Classes (Show1 (liftShowsPrec))
import Data.Functor.Foldable
import Data.JSON.Fields
import Data.Record
import Data.Proxy
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Statement as Statement
import Data.Term
import Data.Text.Encoding (decodeUtf8)
import Data.Union
import GHC.Generics

-- | An F-algebra on some carrier functor 'f'.
type FAlgebra f a = f a -> a

-- | An R-algebra on some carrier functor 'f' of its fixpoint type 't'.
type RAlgebra f t a = f (t, a) -> a

-- | Promote an FAlgebra into an RAlgebra (by dropping the original parameter).
fToR :: Functor (Base t) => FAlgebra (Base t) a -> RAlgebra (Base t) t a
fToR f = f . fmap snd

-- | Lift an algebra into a decorator for terms annotated with records.
decoratorWithAlgebra :: Functor f
                     => RAlgebra (Base (Term f (Record fs))) (Term f (Record fs)) a -- ^ An R-algebra on terms.
                     -> Term f (Record fs) -- ^ A term to decorate with values produced by the R-algebra.
                     -> Term f (Record (a ': fs)) -- ^ A term decorated with values produced by the R-algebra.
decoratorWithAlgebra alg = para $ \ c@(In a f) -> termIn (alg (fmap (second (rhead . extract)) c) :. a) (fmap snd f)


newtype Identifier = Identifier ByteString
  deriving (Eq, Show)

instance ToJSONFields Identifier where
  toJSONFields (Identifier i) = [ "identifier" .= decodeUtf8 i ]

-- | Produce the identifier for a given term, if any.
--
--   Identifier syntax is labelled, as well as declaration syntax identified by these, but other uses of these identifiers are not, e.g. the declaration of a class or method or binding of a variable will be labelled, but a function call will not.
identifierAlgebra :: (Syntax.Identifier :< fs, Declaration.Method :< fs, Declaration.Class :< fs, Apply Foldable fs, Apply Functor fs) => FAlgebra (Base (Term (Union fs) a)) (Maybe Identifier)
identifierAlgebra (In _ union) = case union of
  _ | Just (Syntax.Identifier s) <- prj union -> Just (Identifier s)
  _ | Just Declaration.Class{..} <- prj union -> classIdentifier
  _ | Just Declaration.Method{..} <- prj union -> methodName
  _ -> Nothing


-- | The cyclomatic complexity of a (sub)term.
newtype CyclomaticComplexity = CyclomaticComplexity Int
  deriving (Enum, Eq, Num, Ord, Show)

-- | Compute the cyclomatic complexity of a (sub)term, measured as the number places where control exits scope, e.g. returns and yields.
--
--   TODO: Explicit returns at the end of methods should only count once.
--   TODO: Anonymous functions should not increase parent scope’s complexity.
--   TODO: Inner functions should not increase parent scope’s complexity.
cyclomaticComplexityAlgebra :: (Declaration.Method :< fs, Statement.Return :< fs, Statement.Yield :< fs, Apply Foldable fs, Apply Functor fs) => FAlgebra (Base (Term (Union fs) a)) CyclomaticComplexity
cyclomaticComplexityAlgebra (In _ union) = case union of
  _ | Just Declaration.Method{} <- prj union -> succ (sum union)
  _ | Just Statement.Return{} <- prj union -> succ (sum union)
  _ | Just Statement.Yield{} <- prj union -> succ (sum union)
  _ -> sum union

-- | Compute a 'ByteString' label for a 'Show1'able 'Term'.
--
--   This uses 'liftShowsPrec' to produce the 'ByteString', with the effect that
--   constant fields will be included and parametric fields will not be.
constructorNameAndConstantFields :: Show1 f => TermF f a b -> ByteString
constructorNameAndConstantFields (In _ f) = pack (liftShowsPrec (const (const id)) (const id) 0 f "")

-- | Compute a 'ConstructorLabel' label for a 'Union' of syntax 'Term's.
constructorLabel :: Apply ConstructorName fs => TermF (Union fs) a b -> ConstructorLabel
constructorLabel (In _ u) = ConstructorLabel $ pack (apply (Proxy :: Proxy ConstructorName) constructorName u)


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
