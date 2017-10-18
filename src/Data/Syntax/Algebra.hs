{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Syntax.Algebra
( FAlgebra
, RAlgebra
, fToR
, decoratorWithAlgebra
, identifierAlgebra
, syntaxIdentifierAlgebra
, CyclomaticComplexity(..)
, HasCyclomaticComplexity
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
import Data.Proxy
import Data.Record
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Statement as Statement
import Data.Term
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Union
import GHC.Generics
import qualified Syntax as S

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

syntaxIdentifierAlgebra :: RAlgebra (TermF S.Syntax a) (Term S.Syntax a) (Maybe Identifier)
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
  where identifier = fmap (Identifier . encodeUtf8) . S.extractLeafValue . unwrap . fst


-- | The cyclomatic complexity of a (sub)term.
newtype CyclomaticComplexity = CyclomaticComplexity Int
  deriving (Enum, Eq, Num, Ord, Show, ToJSON)

-- | Compute the cyclomatic complexity of a (sub)term, measured as the number places where control exits scope, e.g. returns and yields.
--
--   TODO: Explicit returns at the end of methods or functions should only count once.
--   TODO: Anonymous functions should not increase parent scope’s complexity.
--   TODO: Inner functions should not increase parent scope’s complexity.

-- | An f-algebra producing a 'CyclomaticComplexity' for syntax nodes corresponding to their summary cyclomatic complexity, defaulting to the sum of their contents’ cyclomatic complexities.
--
--   Customizing this for a given syntax type involves two steps:
--
--   1. Defining a 'CustomHasCyclomaticComplexity' instance for the type.
--   2. Adding the type to the 'CyclomaticComplexityStrategy' type family.
--
--   If you’re getting errors about missing a 'CustomHasCyclomaticComplexity' instance for your syntax type, you probably forgot step 1.
--
--   If you’re getting 'Nothing' for your syntax node at runtime, you probably forgot step 2.
cyclomaticComplexityAlgebra :: (Foldable syntax, HasCyclomaticComplexity syntax) => FAlgebra (TermF syntax ann) CyclomaticComplexity
cyclomaticComplexityAlgebra (In _ syntax) = toCyclomaticComplexity syntax


-- | Types for which we can produce a 'CyclomaticComplexity'. There is exactly one instance of this typeclass; adding customized 'CyclomaticComplexity's for a new type is done by defining an instance of 'CustomHasCyclomaticComplexity' instead.
--
--   This typeclass employs the Advanced Overlap techniques designed by Oleg Kiselyov & Simon Peyton Jones: https://wiki.haskell.org/GHC/AdvancedOverlap.
class HasCyclomaticComplexity syntax where
  -- | Compute a 'CyclomaticComplexity' for a syntax type using its 'CustomHasCyclomaticComplexity' instance, if any, or else falling back to the default definition (which simply returns the sum of any contained cyclomatic complexities).
  toCyclomaticComplexity :: FAlgebra syntax CyclomaticComplexity

-- | Define 'toCyclomaticComplexity' using the 'CustomHasCyclomaticComplexity' instance for a type if there is one or else use the default definition.
--
--   This instance determines whether or not there is an instance for @syntax@ by looking it up in the 'CyclomaticComplexityStrategy' type family. Thus producing a 'CyclomaticComplexity' for a node requires both defining a 'CustomHasCyclomaticComplexity' instance _and_ adding a definition for the type to the 'CyclomaticComplexityStrategy' type family to return 'Custom'.
--
--   Note that since 'CyclomaticComplexityStrategy' has a fallback case for its final entry, this instance will hold for all types of kind @* -> *@. Thus, this must be the only instance of 'HasCyclomaticComplexity', as any other instance would be indistinguishable.
instance (CyclomaticComplexityStrategy syntax ~ strategy, HasCyclomaticComplexityWithStrategy strategy syntax) => HasCyclomaticComplexity syntax where
  toCyclomaticComplexity = toCyclomaticComplexityWithStrategy (Proxy :: Proxy strategy)


-- | Types for which we can produce a customized 'CyclomaticComplexity'.
class CustomHasCyclomaticComplexity syntax where
  -- | Produce a customized 'CyclomaticComplexity' for a given syntax node.
  customToCyclomaticComplexity :: FAlgebra syntax CyclomaticComplexity

instance CustomHasCyclomaticComplexity Declaration.Method where
  customToCyclomaticComplexity = succ . sum

instance CustomHasCyclomaticComplexity Declaration.Function where
  customToCyclomaticComplexity = succ . sum

instance CustomHasCyclomaticComplexity Expression.Call where
  customToCyclomaticComplexity = succ . sum

instance CustomHasCyclomaticComplexity Statement.Break where
  customToCyclomaticComplexity = succ . sum

instance CustomHasCyclomaticComplexity Statement.Catch where
  customToCyclomaticComplexity = succ . sum

instance CustomHasCyclomaticComplexity Statement.Return where
  customToCyclomaticComplexity = succ . sum

instance CustomHasCyclomaticComplexity Statement.Yield where
  customToCyclomaticComplexity = succ . sum

-- | Produce a 'CyclomaticComplexity' for 'Union's using the 'HasCyclomaticComplexity' instance & therefore using a 'CustomHasCyclomaticComplexity' instance when one exists & the type is listed in 'CyclomaticComplexityStrategy'.
instance Apply HasCyclomaticComplexity fs => CustomHasCyclomaticComplexity (Union fs) where
  customToCyclomaticComplexity = apply (Proxy :: Proxy HasCyclomaticComplexity) toCyclomaticComplexity


-- | A strategy for defining a 'HasCyclomaticComplexity' instance. Intended to be promoted to the kind level using @-XDataKinds@.
data Strategy = Default | Custom

-- | Produce a 'CyclomaticComplexity' for a syntax node using either the 'Default' or 'Custom' strategy.
--
--   You should probably be using 'CustomHasCyclomaticComplexity' instead of this class; and you should not define new instances of this class.
class HasCyclomaticComplexityWithStrategy (strategy :: Strategy) syntax where
  toCyclomaticComplexityWithStrategy :: proxy strategy -> FAlgebra syntax CyclomaticComplexity


-- | A predicate on syntax types selecting either the 'Custom' or 'Default' strategy.
--
--   Only entries for which we want to use the 'Custom' strategy should be listed, with the exception of the final entry which maps all other types onto the 'Default' strategy.
--
--   If you’re seeing errors about missing a 'CustomHasCyclomaticComplexity' instance for a given type, you’ve probably listed it in here but not defined a 'CustomHasCyclomaticComplexity' instance for it, or else you’ve listed the wrong type in here. Conversely, if your 'customHasCyclomaticComplexity' method is never being called, you may have forgotten to list the type in here.
type family CyclomaticComplexityStrategy syntax where
  CyclomaticComplexityStrategy Declaration.Method = 'Custom
  CyclomaticComplexityStrategy Declaration.Function = 'Custom
  CyclomaticComplexityStrategy Expression.Call = 'Custom
  CyclomaticComplexityStrategy Statement.Break = 'Custom
  CyclomaticComplexityStrategy Statement.Catch = 'Custom
  CyclomaticComplexityStrategy Statement.Return = 'Custom
  CyclomaticComplexityStrategy Statement.Yield = 'Custom
  CyclomaticComplexityStrategy (Union fs) = 'Custom
  CyclomaticComplexityStrategy a = 'Default


-- | The 'Default' strategy produces 'Nothing'.
instance Foldable syntax => HasCyclomaticComplexityWithStrategy 'Default syntax where
  toCyclomaticComplexityWithStrategy _ = sum

-- | The 'Custom' strategy delegates the selection of the strategy to the 'CustomHasCyclomaticComplexity' instance for the type.
instance CustomHasCyclomaticComplexity syntax => HasCyclomaticComplexityWithStrategy 'Custom syntax where
  toCyclomaticComplexityWithStrategy _ = customToCyclomaticComplexity




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
