{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Decorator
( FAlgebra
, RAlgebra
, OpenFAlgebra
, OpenRAlgebra
, fToR
, decoratorWithAlgebra
, identifierAlgebra
, syntaxIdentifierAlgebra
, cyclomaticComplexityAlgebra
, ConstructorName(..)
, ConstructorLabel(..)
, constructorNameAndConstantFields
, constructorLabel
) where

import Data.Aeson
import Data.Bifunctor (second)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Foldable (asum)
import Data.Functor.Classes (Show1 (liftShowsPrec))
import Data.Functor.Foldable
import Data.JSON.Fields
import Data.Record
import Data.Proxy
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Statement as Statement
import Data.Term
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Union
import GHC.Generics
import qualified Syntax as S

-- | An F-algebra on some 'Recursive' type @t@.
type FAlgebra t a = Base t a -> a

-- | An R-algebra on some 'Recursive' type @t@.
type RAlgebra t a = Base t (t, a) -> a

-- | An open-recursive F-algebra on some 'Recursive' type @t@.
type OpenFAlgebra t a = forall b . (b -> a) -> Base t b -> a

-- | An open-recursive R-algebra on some 'Recursive' type @t@.
type OpenRAlgebra t a = forall b . (b -> (t, a)) -> Base t b -> a

-- | Promote an FAlgebra into an RAlgebra (by dropping the original parameter).
fToR :: Functor (Base t) => FAlgebra t a -> RAlgebra t a
fToR f = f . fmap snd

-- | Lift an algebra into a decorator for terms annotated with records.
decoratorWithAlgebra :: Functor f
                     => RAlgebra (Term f (Record fs)) a -- ^ An R-algebra on terms.
                     -> Term f (Record fs) -- ^ A term to decorate with values produced by the R-algebra.
                     -> Term f (Record (a ': fs)) -- ^ A term decorated with values produced by the R-algebra.
decoratorWithAlgebra alg = para $ \ c@(In a f) -> termIn (alg (fmap (second (rhead . termAnnotation)) c) :. a) (fmap snd f)


newtype Identifier = Identifier ByteString
  deriving (Eq, Show)

instance ToJSONFields Identifier where
  toJSONFields (Identifier i) = [ "identifier" .= decodeUtf8 i ]

-- | Produce the identifier for a given term, if any.
--
--   Identifier syntax is labelled, as well as declaration syntax identified by these, but other uses of these identifiers are not, e.g. the declaration of a class or method or binding of a variable will be labelled, but a function call will not.
identifierAlgebra :: (Syntax.Identifier :< fs, Declaration.Method :< fs, Declaration.Class :< fs, Apply Foldable fs, Apply Functor fs) => FAlgebra (Term (Union fs) a) (Maybe Identifier)
identifierAlgebra (In _ union) = case union of
  _ | Just (Syntax.Identifier s) <- prj union -> Just (Identifier s)
  _ | Just Declaration.Class{..} <- prj union -> classIdentifier
  _ | Just Declaration.Method{..} <- prj union -> methodName
  _ -> Nothing

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


-- | The cyclomatic complexity of a (sub)term.
newtype CyclomaticComplexity = CyclomaticComplexity Int
  deriving (Enum, Eq, Num, Ord, Show)

-- | Compute the cyclomatic complexity of a (sub)term, measured as the number places where control exits scope, e.g. returns and yields.
--
--   TODO: Explicit returns at the end of methods should only count once.
--   TODO: Anonymous functions should not increase parent scope’s complexity.
--   TODO: Inner functions should not increase parent scope’s complexity.
cyclomaticComplexityAlgebra :: (Declaration.Method :< fs, Statement.Return :< fs, Statement.Yield :< fs, Apply Foldable fs, Apply Functor fs) => FAlgebra (Term (Union fs) a) CyclomaticComplexity
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
constructorLabel :: ConstructorName syntax => TermF syntax a b -> ConstructorLabel
constructorLabel (In _ s) = ConstructorLabel $ pack (constructorName s)


newtype ConstructorLabel = ConstructorLabel ByteString

instance Show ConstructorLabel where
  showsPrec _ (ConstructorLabel s) = showString (unpack s)

instance ToJSONFields ConstructorLabel where
  toJSONFields (ConstructorLabel s) = [ "category" .= decodeUtf8 s ]


-- | A typeclass to retrieve the name of the data constructor for a value.
--
--   This typeclass employs the Advanced Overlap techniques designed by Oleg Kiselyov & Simon Peyton Jones: https://wiki.haskell.org/GHC/AdvancedOverlap; see also src/Renderer/TOC.hs for discussion of the details of the mechanism.
class ConstructorName syntax where
  constructorName :: syntax a -> String

instance (ConstructorNameStrategy syntax ~ strategy, ConstructorNameWithStrategy strategy syntax) => ConstructorName syntax where
  constructorName = constructorNameWithStrategy (Proxy :: Proxy strategy)

class CustomConstructorName syntax where
  customConstructorName :: syntax a -> String

instance Apply ConstructorName fs => CustomConstructorName (Union fs) where
  customConstructorName = apply (Proxy :: Proxy ConstructorName) constructorName

instance CustomConstructorName [] where
  customConstructorName [] = "[]"
  customConstructorName _  = ""

data Strategy = Default | Custom

type family ConstructorNameStrategy syntax where
  ConstructorNameStrategy (Union _) = 'Custom
  ConstructorNameStrategy []        = 'Custom
  ConstructorNameStrategy syntax    = 'Default

class ConstructorNameWithStrategy (strategy :: Strategy) syntax where
  constructorNameWithStrategy :: proxy strategy -> syntax a -> String

instance (Generic1 syntax, GConstructorName (Rep1 syntax)) => ConstructorNameWithStrategy 'Default syntax where
  constructorNameWithStrategy _ = gconstructorName . from1

instance CustomConstructorName syntax => ConstructorNameWithStrategy 'Custom syntax where
  constructorNameWithStrategy _ = customConstructorName


class GConstructorName f where
  gconstructorName :: f a -> String

instance GConstructorName f => GConstructorName (M1 D c f) where
  gconstructorName = gconstructorName . unM1

instance (GConstructorName f, GConstructorName g) => GConstructorName (f :+: g) where
  gconstructorName (L1 l) = gconstructorName l
  gconstructorName (R1 r) = gconstructorName r

instance Constructor c => GConstructorName (M1 C c f) where
  gconstructorName = conName
