{-# LANGUAGE AllowAmbiguousTypes, DeriveAnyClass, DerivingVia, GADTs, TypeOperators, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables, KindSignatures, RankNTypes, ConstraintKinds, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-redundant-constraints #-} -- For HasCallStack
module Data.Syntax where

import Data.Abstract.Evaluatable hiding (Empty, Error)
import Data.Aeson as Aeson (ToJSON(..), object)
import Data.JSON.Fields
import qualified Data.Set as Set
import Data.Sum
import Data.Term
import qualified Data.Reprinting.Token as Token
import GHC.Types (Constraint)
import GHC.TypeLits
import Diffing.Algorithm
import Prelude
import Prologue
import Reprinting.Tokenize hiding (Element)
import Source.Loc
import qualified Assigning.Assignment as Assignment
import qualified Data.Error as Error
import Control.Abstract.ScopeGraph (reference, Reference(..), Declaration(..))
import Control.Abstract.Heap (deref, lookupSlot)
import qualified Data.Abstract.ScopeGraph as ScopeGraph

-- Combinators

-- | Lift syntax and an annotation into a term, injecting the syntax into a union & ensuring the annotation encompasses all children.
makeTerm :: (HasCallStack, Element syntax syntaxes, Semigroup ann, Apply Foldable syntaxes) => ann -> syntax (Term (Sum syntaxes) ann) -> Term (Sum syntaxes) ann
makeTerm ann = makeTerm' ann . inject

-- | Lift a union and an annotation into a term, ensuring the annotation encompasses all children.
makeTerm' :: (HasCallStack, Semigroup ann, Foldable syntax) => ann -> syntax (Term syntax ann) -> Term syntax ann
makeTerm' ann syntax = termIn (sconcat (ann :| (termAnnotation <$> toList syntax))) syntax

-- | Lift syntax and an annotation into a term, injecting the syntax into a union & ensuring the annotation encompasses all children. Removes extra structure if term is a list of a single item.
makeTerm'' :: (HasCallStack, Element syntax syntaxes, Semigroup ann, Apply Foldable syntaxes, Foldable syntax) => ann -> syntax (Term (Sum syntaxes) ann) -> Term (Sum syntaxes) ann
makeTerm'' ann children = case toList children of
  [x] -> x
  _ -> makeTerm' ann (inject children)

-- | Lift non-empty syntax into a term, injecting the syntax into a union & appending all subterms’.annotations to make the new term’s annotation.
makeTerm1 :: (HasCallStack, Element syntax syntaxes, Semigroup ann, Apply Foldable syntaxes) => syntax (Term (Sum syntaxes) ann) -> Term (Sum syntaxes) ann
makeTerm1 = makeTerm1' . inject

-- | Lift a non-empty union into a term, appending all subterms’ annotations to make the new term’s annotation.
makeTerm1' :: (HasCallStack, Semigroup ann, Foldable syntax) => syntax (Term syntax ann) -> Term syntax ann
makeTerm1' syntax = case toList syntax of
  a : _ -> makeTerm' (termAnnotation a) syntax
  _ -> error "makeTerm1': empty structure"

-- | Construct an empty term at the current position.
emptyTerm :: (HasCallStack, Empty :< syntaxes, Apply Foldable syntaxes) => Assignment.Assignment ast grammar (Term (Sum syntaxes) Loc)
emptyTerm = makeTerm . startLocation <$> Assignment.location <*> pure Empty
  where startLocation Loc{..} = Loc (Range (start locByteRange) (start locByteRange)) (Span (spanStart locSpan) (spanStart locSpan))

-- | Catch assignment errors into an error term.
handleError :: (HasCallStack, Error :< syntaxes, Enum grammar, Eq1 ast, Ix grammar, Show grammar, Apply Foldable syntaxes) => Assignment.Assignment ast grammar (Term (Sum syntaxes) Loc) -> Assignment.Assignment ast grammar (Term (Sum syntaxes) Loc)
handleError = flip Assignment.catchError (\ err -> makeTerm <$> Assignment.location <*> pure (errorSyntax (either id show <$> err) []) <* Assignment.source)

-- | Catch parse errors into an error term.
parseError :: (HasCallStack, Error :< syntaxes, Bounded grammar, Enum grammar, Ix grammar, Apply Foldable syntaxes) => Assignment.Assignment ast grammar (Term (Sum syntaxes) Loc)
parseError = makeTerm <$> Assignment.token maxBound <*> pure (Error (ErrorStack $ errorSite <$> getCallStack (freezeCallStack callStack)) [] (Just "ParseError") [])

-- | Match context terms before a subject term, wrapping both up in a Context term if any context terms matched, or otherwise returning the subject term.
contextualize :: (HasCallStack, Context :< syntaxes, Alternative m, Semigroup ann, Apply Foldable syntaxes)
              => m (Term (Sum syntaxes) ann)
              -> m (Term (Sum syntaxes) ann)
              -> m (Term (Sum syntaxes) ann)
contextualize context rule = make <$> Assignment.manyThrough context rule
  where make (cs, node) = case nonEmpty cs of
          Just cs -> makeTerm1 (Context cs node)
          _ -> node

-- | Match context terms after a subject term and before a delimiter, returning the delimiter paired with a Context term if any context terms matched, or the subject term otherwise.
postContextualizeThrough :: (HasCallStack, Context :< syntaxes, Alternative m, Semigroup ann, Apply Foldable syntaxes)
                         => m (Term (Sum syntaxes) ann)
                         -> m (Term (Sum syntaxes) ann)
                         -> m delimiter
                         -> m (Term (Sum syntaxes) ann, delimiter)
postContextualizeThrough context rule end = make <$> rule <*> Assignment.manyThrough context end
  where make node (cs, end) = case nonEmpty cs of
          Just cs -> (makeTerm1 (Context cs node), end)
          _ -> (node, end)

-- | Match context terms after a subject term, wrapping both up in a Context term if any context terms matched, or otherwise returning the subject term.
postContextualize :: (HasCallStack, Context :< syntaxes, Alternative m, Semigroup ann, Apply Foldable syntaxes)
                  => m (Term (Sum syntaxes) ann)
                  -> m (Term (Sum syntaxes) ann)
                  -> m (Term (Sum syntaxes) ann)
postContextualize context rule = make <$> rule <*> many context
  where make node cs = case nonEmpty cs of
          Just cs -> makeTerm1 (Context cs node)
          _ -> node

-- | Match infix terms separated by any of a list of operators, with optional context terms following each operand.
infixContext :: (Context :< syntaxes, Assignment.Parsing m, Semigroup ann, HasCallStack, Apply Foldable syntaxes)
             => m (Term (Sum syntaxes) ann)
             -> m (Term (Sum syntaxes) ann)
             -> m (Term (Sum syntaxes) ann)
             -> [m (Term (Sum syntaxes) ann -> Term (Sum syntaxes) ann -> Sum syntaxes (Term (Sum syntaxes) ann))]
             -> m (Sum syntaxes (Term (Sum syntaxes) ann))
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
  deriving newtype (Eq, Ord, Show)
  deriving stock (Foldable, Functor, Generic1, Traversable)
  deriving anyclass (Diffable, Hashable1, ToJSONFields1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Identifier


instance Evaluatable Identifier where
  eval eval ref' term@(Identifier name) = do
    -- TODO: Set the span up correctly in ref so we can move the `reference` call there.
    span <- ask @Span
    reference (Reference name) span ScopeGraph.Identifier (Declaration name)
    deref =<< ref eval ref' term

  ref _ _ (Identifier name) = lookupSlot (Declaration name)


instance Tokenize Identifier where
  tokenize = yield . Token.Run . formatName . Data.Syntax.name

instance FreeVariables1 Identifier where
  liftFreeVariables _ (Identifier x) = Set.singleton x

instance Declarations1 Identifier where
  liftDeclaredName _ (Identifier x) = pure x
  liftDeclaredAlias _ (Identifier x) = pure x

-- | An accessibility modifier, e.g. private, public, protected, etc.
newtype AccessibilityModifier a = AccessibilityModifier { contents :: Text }
  deriving newtype (Eq, Ord, Show)
  deriving stock (Foldable, Functor, Generic1, Traversable)
  deriving anyclass (Declarations1, Diffable, FreeVariables1, Hashable1, ToJSONFields1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically AccessibilityModifier

-- TODO: Implement Eval instance for AccessibilityModifier
instance Evaluatable AccessibilityModifier

-- | Empty syntax, with essentially no-op semantics.
--
--   This can be used to represent an implicit no-op, e.g. the alternative in an 'if' statement without an 'else'.
data Empty a = Empty
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, FreeVariables1, Declarations1, ToJSONFields1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Empty

instance Evaluatable Empty where
  eval _ _ _ = unit

instance Tokenize Empty where
  tokenize = ignore

-- | Syntax representing a parsing or assignment error.
data Error a = Error { errorCallStack :: ErrorStack, errorExpected :: [String], errorActual :: Maybe String, errorChildren :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Error

instance Evaluatable Error

instance Tokenize Error where
  -- TODO: Considering producing comments like "ERROR: due to.." instead of ignoring.
  tokenize = ignore

errorSyntax :: Error.Error String -> [a] -> Error a
errorSyntax Error.Error{..} = Error (ErrorStack $ errorSite <$> getCallStack callStack) errorExpected errorActual

unError :: Span -> Error a -> Error.Error String
unError span Error{..} = Error.Error span errorExpected errorActual stack
  where stack = fromCallSiteList $ unErrorSite <$> unErrorStack errorCallStack

data ErrorSite = ErrorSite { errorMessage :: String, errorLocation :: SrcLoc }
  deriving (Eq, Show, Generic, NFData)

errorSite :: (String, SrcLoc) -> ErrorSite
errorSite = uncurry ErrorSite

unErrorSite :: ErrorSite -> (String, SrcLoc)
unErrorSite ErrorSite{..} = (errorMessage, errorLocation)

newtype ErrorStack = ErrorStack { unErrorStack :: [ErrorSite] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

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


data Context a = Context { contextTerms :: NonEmpty a, contextSubject :: a }
  deriving (Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Context

instance Diffable Context where
  subalgorithmFor blur focus (Context n s) = Context <$> traverse blur n <*> focus s

  equivalentBySubterm = Just . contextSubject

instance Hashable1 Context where liftHashWithSalt = foldl

instance Evaluatable Context where
  eval eval _ Context{..} = eval contextSubject

instance Tokenize Context where
  tokenize Context{..} = sequenceA_ (sepTrailing contextTerms) *> contextSubject

instance Declarations1 Context where
  liftDeclaredName declaredName = declaredName . contextSubject
