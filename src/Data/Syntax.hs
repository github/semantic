{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, DeriveAnyClass, DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, RankNTypes, RecordWildCards, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Syntax (module Data.Syntax) where

import Data.Abstract.Evaluatable hiding (Empty, Error)
import Data.Aeson as Aeson (ToJSON(..), object)
import Data.JSON.Fields
import qualified Data.Set as Set
import Data.Sum
import Data.Term
import GHC.Types (Constraint)
import GHC.TypeLits
import Diffing.Algorithm
import Prelude
import Prologue
import Source.Loc
import Source.Range as Range
import Source.Span as Span
import qualified Assigning.Assignment as Assignment
import qualified Data.Error as Error
import Control.Abstract.ScopeGraph (reference, Reference(..), Declaration(..))
import Control.Abstract.Heap (deref, lookupSlot)
import qualified Data.Abstract.ScopeGraph as ScopeGraph

-- Combinators

-- | Lift syntax and an annotation into a term, injecting the syntax into a union & ensuring the annotation encompasses all children.
makeTerm :: (Element syntax syntaxes, Sum syntaxes ~ Syntax term, Semigroup ann, Apply Foldable syntaxes, IsTerm term) => ann -> syntax (term ann) -> term ann
makeTerm ann = makeTerm' ann . inject

-- | Lift a union and an annotation into a term, ensuring the annotation encompasses all children.
makeTerm' :: (Semigroup ann, Foldable (Syntax term), IsTerm term) => ann -> Syntax term (term ann) -> term ann
makeTerm' ann syntax = termIn (sconcat (ann :| (termAnnotation <$> toList syntax))) syntax

-- | Lift syntax and an annotation into a term, injecting the syntax into a union & ensuring the annotation encompasses all children. Removes extra structure if term is a list of a single item.
makeTerm'' :: (Element syntax syntaxes, Sum syntaxes ~ Syntax term, Semigroup ann, Apply Foldable syntaxes, Foldable syntax, IsTerm term) => ann -> syntax (term ann) -> term ann
makeTerm'' ann children = case toList children of
  [x] -> x
  _ -> makeTerm' ann (inject children)

-- | Lift non-empty syntax into a term, injecting the syntax into a union & appending all subterms’.annotations to make the new term’s annotation.
makeTerm1 :: (HasCallStack, Element syntax syntaxes, Sum syntaxes ~ Syntax term, Semigroup ann, Apply Foldable syntaxes, IsTerm term) => syntax (term ann) -> term ann
makeTerm1 = makeTerm1' . inject

-- | Lift a non-empty union into a term, appending all subterms’ annotations to make the new term’s annotation.
makeTerm1' :: (HasCallStack, Semigroup ann, Foldable (Syntax term), IsTerm term) => Syntax term (term ann) -> term ann
makeTerm1' syntax = case toList syntax of
  a : _ -> makeTerm' (termAnnotation a) syntax
  _ -> error "makeTerm1': empty structure"

-- | Construct an empty term at the current position.
emptyTerm :: (Empty :< syntaxes, Sum syntaxes ~ Syntax term, Apply Foldable syntaxes, IsTerm term) => Assignment.Assignment ast grammar (term Loc)
emptyTerm = makeTerm . startLocation <$> Assignment.location <*> pure Empty
  where startLocation Loc{..} = Loc (Range.point (Range.start byteRange)) (Span.point (Span.start span))

-- | Catch assignment errors into an error term.
handleError :: (HasCallStack, Error :< syntaxes, Sum syntaxes ~ Syntax term, Enum grammar, Eq1 ast, Ix grammar, Show grammar, Apply Foldable syntaxes, IsTerm term) => Assignment.Assignment ast grammar (term Loc) -> Assignment.Assignment ast grammar (term Loc)
handleError = flip Assignment.catchError (\ err -> makeTerm <$> Assignment.location <*> pure (errorSyntax (either id show <$> err) []) <* Assignment.source)

-- | Catch parse errors into an error term.
parseError :: (Error :< syntaxes, Sum syntaxes ~ Syntax term, Bounded grammar, Enum grammar, Apply Foldable syntaxes, IsTerm term) => Assignment.Assignment ast grammar (term Loc)
parseError = makeTerm <$> Assignment.token maxBound <*> pure (Error (ErrorStack $ errorSite <$> getCallStack (freezeCallStack callStack)) [] (Just "ParseError") [])

-- | Match context terms before a subject term, wrapping both up in a Context term if any context terms matched, or otherwise returning the subject term.
contextualize :: (HasCallStack, Context :< syntaxes, Sum syntaxes ~ Syntax term, Alternative m, Semigroup ann, Apply Foldable syntaxes, IsTerm term)
              => m (term ann)
              -> m (term ann)
              -> m (term ann)
contextualize context rule = make <$> Assignment.manyThrough context rule
  where make (cs, node) = case nonEmpty cs of
          Just cs -> makeTerm1 (Context cs node)
          _ -> node

-- | Match context terms after a subject term and before a delimiter, returning the delimiter paired with a Context term if any context terms matched, or the subject term otherwise.
postContextualizeThrough :: (HasCallStack, Context :< syntaxes, Sum syntaxes ~ Syntax term, Alternative m, Semigroup ann, Apply Foldable syntaxes, IsTerm term)
                         => m (term ann)
                         -> m (term ann)
                         -> m delimiter
                         -> m (term ann, delimiter)
postContextualizeThrough context rule end = make <$> rule <*> Assignment.manyThrough context end
  where make node (cs, end) = case nonEmpty cs of
          Just cs -> (makeTerm1 (Context cs node), end)
          _ -> (node, end)

-- | Match context terms after a subject term, wrapping both up in a Context term if any context terms matched, or otherwise returning the subject term.
postContextualize :: (HasCallStack, Context :< syntaxes, Sum syntaxes ~ Syntax term, Alternative m, Semigroup ann, Apply Foldable syntaxes, IsTerm term)
                  => m (term ann)
                  -> m (term ann)
                  -> m (term ann)
postContextualize context rule = make <$> rule <*> many context
  where make node cs = case nonEmpty cs of
          Just cs -> makeTerm1 (Context cs node)
          _ -> node

-- | Match infix terms separated by any of a list of operators, with optional context terms following each operand.
infixContext :: (Context :< syntaxes, Sum syntaxes ~ Syntax term, Assignment.Parsing m, Semigroup ann, HasCallStack, Apply Foldable syntaxes, IsTerm term)
             => m (term ann)
             -> m (term ann)
             -> m (term ann)
             -> [m (term ann -> term ann -> Sum syntaxes (term ann))]
             -> m (Sum syntaxes (term ann))
infixContext context left right operators = uncurry (&) <$> postContextualizeThrough context left (asum operators) <*> postContextualize context right

class Generate (c :: (* -> *) -> Constraint) (all :: [* -> *]) (fs :: [* -> *]) where
  generate :: Monoid b => (forall f proxy. (Element f all, c f) => proxy f -> Integer -> b) -> b

instance Generate c all '[] where
  generate _ = mempty

instance (Element f all, c f, Generate c all fs) => Generate c all (f ': fs) where
  generate each = each (Proxy @f) (natVal (Proxy @(ElemIndex f all))) `mappend` generate @c @all @fs each


-- Common

-- | An identifier of some other construct, whether a containing declaration (e.g. a class name) or a reference (e.g. a variable).
newtype Identifier a = Identifier { name :: Name }
  deriving (Diffable, Foldable, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Identifier where liftEq = genericLiftEq
instance Ord1 Identifier where liftCompare = genericLiftCompare
instance Show1 Identifier where liftShowsPrec = genericLiftShowsPrec


instance Evaluatable Identifier where
  eval eval ref' term@(Identifier name) = do
    -- TODO: Set the span up correctly in ref so we can move the `reference` call there.
    span <- ask @Span
    reference (Reference name) span ScopeGraph.Identifier (Declaration name)
    deref =<< ref eval ref' term

  ref _ _ (Identifier name) = lookupSlot (Declaration name)


instance FreeVariables1 Identifier where
  liftFreeVariables _ (Identifier x) = Set.singleton x

instance Declarations1 Identifier where
  liftDeclaredName _ (Identifier x) = pure x
  liftDeclaredAlias _ (Identifier x) = pure x

-- | An accessibility modifier, e.g. private, public, protected, etc.
newtype AccessibilityModifier a = AccessibilityModifier { contents :: Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 AccessibilityModifier where liftEq = genericLiftEq
instance Ord1 AccessibilityModifier where liftCompare = genericLiftCompare
instance Show1 AccessibilityModifier where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for AccessibilityModifier
instance Evaluatable AccessibilityModifier

-- | Empty syntax, with essentially no-op semantics.
--
--   This can be used to represent an implicit no-op, e.g. the alternative in an 'if' statement without an 'else'.
data Empty a = Empty
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Empty where liftEq = genericLiftEq
instance Ord1 Empty where liftCompare = genericLiftCompare
instance Show1 Empty where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Empty where
  eval _ _ _ = unit

-- | Syntax representing a parsing or assignment error.
data Error a = Error { errorCallStack :: ErrorStack, errorExpected :: [String], errorActual :: Maybe String, errorChildren :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Error where liftEq = genericLiftEq
instance Ord1 Error where liftCompare = genericLiftCompare
instance Show1 Error where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Error

errorSyntax :: Error.Error String -> [a] -> Error a
errorSyntax Error.Error{..} = Error (ErrorStack $ errorSite <$> getCallStack callStack) errorExpected errorActual

unError :: Span -> Error a -> Error.Error String
unError span Error{..} = Error.Error span errorExpected errorActual stack
  where stack = fromCallSiteList $ unErrorSite <$> unErrorStack errorCallStack

data ErrorSite = ErrorSite { errorMessage :: String, errorLocation :: SrcLoc }
  deriving (Eq, Show, Generic)

errorSite :: (String, SrcLoc) -> ErrorSite
errorSite = uncurry ErrorSite

unErrorSite :: ErrorSite -> (String, SrcLoc)
unErrorSite ErrorSite{..} = (errorMessage, errorLocation)

newtype ErrorStack = ErrorStack { unErrorStack :: [ErrorSite] }
  deriving (Eq, Show, Generic)

instance ToJSON ErrorStack where
  toJSON (ErrorStack es) = toJSON (jSite <$> es) where
    jSite (ErrorSite site SrcLoc{..}) = Aeson.object
      [ "site" .= site
      , "package" .= srcLocPackage
      , "module" .= srcLocModule
      , "file" .= srcLocFile
      , "startLine" .= srcLocStartLine
      , "startColumn" .= srcLocStartCol
      , "endColumn" .= srcLocEndCol
      ]

instance Hashable ErrorStack where
  hashWithSalt = hashUsing (map (second ((,,,,,,) <$> srcLocPackage <*> srcLocModule <*> srcLocFile <*> srcLocStartLine <*> srcLocStartCol <*> srcLocEndLine <*> srcLocEndCol) . unErrorSite) . unErrorStack)

instance Ord ErrorStack where
  compare = liftCompare (liftCompare compareSrcLoc) `on` (fmap unErrorSite . unErrorStack)
    where compareSrcLoc s1 s2 = mconcat
            [ (compare `on` srcLocPackage) s1 s2
            , (compare `on` srcLocModule) s1 s2
            , (compare `on` srcLocFile) s1 s2
            , (compare `on` srcLocStartLine) s1 s2
            , (compare `on` srcLocStartCol) s1 s2
            , (compare `on` srcLocEndLine) s1 s2
            , (compare `on` srcLocEndCol) s1 s2
            ]


class HasErrors term where
  getErrors :: term Loc -> [Error.Error String]

instance (Error :< fs, Apply Foldable fs, Apply Functor fs) => HasErrors (Term (Sum fs)) where
  getErrors = cata $ \ (In Loc{..} syntax) ->
    maybe (fold syntax) (pure . unError span) (Data.Sum.project syntax)


data Context a = Context { contextTerms :: NonEmpty a, contextSubject :: a }
  deriving (Foldable, FreeVariables1, Functor, Generic1, ToJSONFields1, Traversable)

instance Eq1 Context where liftEq = genericLiftEq
instance Ord1 Context where liftCompare = genericLiftCompare
instance Show1 Context where liftShowsPrec = genericLiftShowsPrec

instance Diffable Context where
  subalgorithmFor blur focus (Context n s) = Context <$> traverse blur n <*> focus s

  equivalentBySubterm = Just . contextSubject

instance Hashable1 Context where liftHashWithSalt = foldl

instance Evaluatable Context where
  eval eval _ Context{..} = eval contextSubject

instance Declarations1 Context where
  liftDeclaredName declaredName = declaredName . contextSubject
