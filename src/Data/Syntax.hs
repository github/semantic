{-# LANGUAGE AllowAmbiguousTypes, DeriveAnyClass, GADTs, TypeOperators, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables, KindSignatures, RankNTypes, ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -fno-warn-orphans #-} -- For HasCallStack
module Data.Syntax where

import Data.Abstract.Evaluatable
import Data.Aeson (ToJSON(..), object)
import Data.AST
import Data.JSON.Fields
import Data.Range
import Data.Record
import Data.Span
import Data.Sum
import Data.Term
import Diffing.Algorithm hiding (Empty)
import Prelude
import Prologue
import qualified Assigning.Assignment as Assignment
import qualified Data.Error as Error
import Proto3.Suite.Class
import Proto3.Wire.Decode
import Proto3.Wire.Types
import GHC.Types (Constraint)
import GHC.TypeLits
import qualified Proto3.Suite.DotProto as Proto
import Data.Char (toLower)
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
emptyTerm :: (HasCallStack, Empty :< syntaxes, Apply Foldable syntaxes) => Assignment.Assignment ast grammar (Term (Sum syntaxes) (Record Location))
emptyTerm = makeTerm . startLocation <$> Assignment.location <*> pure Empty
  where startLocation ann = Range (start (getField ann)) (start (getField ann)) :. Span (spanStart (getField ann)) (spanStart (getField ann)) :. Nil

-- | Catch assignment errors into an error term.
handleError :: (HasCallStack, Error :< syntaxes, Enum grammar, Eq1 ast, Ix grammar, Show grammar, Apply Foldable syntaxes) => Assignment.Assignment ast grammar (Term (Sum syntaxes) (Record Location)) -> Assignment.Assignment ast grammar (Term (Sum syntaxes) (Record Location))
handleError = flip catchError (\ err -> makeTerm <$> Assignment.location <*> pure (errorSyntax (either id show <$> err) []) <* Assignment.source)

-- | Catch parse errors into an error term.
parseError :: (HasCallStack, Error :< syntaxes, Bounded grammar, Enum grammar, Ix grammar, Apply Foldable syntaxes) => Assignment.Assignment ast grammar (Term (Sum syntaxes) (Record Location))
parseError = makeTerm <$> Assignment.token maxBound <*> pure (Error (ErrorStack (getCallStack (freezeCallStack callStack))) [] (Just "ParseError") [])


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

instance (Apply Message1 fs, Generate Message1 fs fs, Generate Named1 fs fs) => Message1 (Sum fs) where
  liftEncodeMessage encodeMessage num = apply @Message1 (liftEncodeMessage encodeMessage num)
  liftDecodeMessage decodeMessage _ = oneof undefined listOfParsers
    where
      listOfParsers =
        generate @Message1 @fs @fs (\ (_ :: proxy f) i -> let num = FieldNumber (fromInteger (succ i)) in [(num, fromJust <$> embedded (inject @f @fs <$> liftDecodeMessage decodeMessage num))])
  liftDotProto _ =
    [Proto.DotProtoMessageOneOf (Proto.Single "syntax") (generate @Named1 @fs @fs (\ (_ :: proxy f) i ->
      let
        num = FieldNumber (fromInteger (succ i))
        fieldType = Proto.Prim (Proto.Named . Proto.Single $ nameOf1 (Proxy @f))
        fieldName = Proto.Single (camelCase $ nameOf1 (Proxy @f))
        camelCase (x : xs) = toLower x : xs
        camelCase [] = []
      in
        [ Proto.DotProtoField num fieldType fieldName [] Nothing ]))]

class Generate (c :: (* -> *) -> Constraint) (all :: [* -> *]) (fs :: [* -> *]) where
  generate :: Monoid b => (forall f proxy. (Element f all, c f) => proxy f -> Integer -> b) -> b

instance Generate c all '[] where
  generate _ = mempty

instance (Element f all, c f, Generate c all fs) => Generate c all (f ': fs) where
  generate each = each (Proxy @f) (natVal (Proxy @(ElemIndex f all))) `mappend` generate @c @all @fs each

-- Common

-- | An identifier of some other construct, whether a containing declaration (e.g. a class name) or a reference (e.g. a variable).
newtype Identifier a = Identifier { name :: Name }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, Named1, ToJSONFields1)

instance Eq1 Identifier where liftEq = genericLiftEq
instance Ord1 Identifier where liftCompare = genericLiftCompare
instance Show1 Identifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Identifier where
  eval (Identifier name) = pure (LvalLocal name)

instance FreeVariables1 Identifier where
  liftFreeVariables _ (Identifier x) = pure x

instance Declarations1 Identifier where
  liftDeclaredName _ (Identifier x) = pure x

-- | An accessibility modifier, e.g. private, public, protected, etc.
newtype AccessibilityModifier a = AccessibilityModifier Text
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AccessibilityModifier where liftEq = genericLiftEq
instance Ord1 AccessibilityModifier where liftCompare = genericLiftCompare
instance Show1 AccessibilityModifier where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for AccessibilityModifier
instance Evaluatable AccessibilityModifier

-- | Empty syntax, with essentially no-op semantics.
--
--   This can be used to represent an implicit no-op, e.g. the alternative in an 'if' statement without an 'else'.
data Empty a = Empty
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, Mergeable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

instance Eq1 Empty where liftEq _ _ _ = True
instance Ord1 Empty where liftCompare _ _ _ = EQ
instance Show1 Empty where liftShowsPrec _ _ _ _ = showString "Empty"

instance Evaluatable Empty where
  eval _ = rvalBox unit

-- | Syntax representing a parsing or assignment error.
data Error a = Error { errorCallStack :: ErrorStack, errorExpected :: [String], errorActual :: Maybe String, errorChildren :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Error where liftEq = genericLiftEq
instance Ord1 Error where liftCompare = genericLiftCompare
instance Show1 Error where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Error


errorSyntax :: Error.Error String -> [a] -> Error a
errorSyntax Error.Error{..} = Error (ErrorStack (getCallStack callStack)) errorExpected errorActual

unError :: Span -> Error a -> Error.Error String
unError span Error{..} = Error.withCallStack (freezeCallStack (fromCallSiteList (unErrorStack errorCallStack))) (Error.Error span errorExpected errorActual)

newtype ErrorStack = ErrorStack { unErrorStack :: [(String, SrcLoc)] }
  deriving (Eq)

instance Show ErrorStack where
  showsPrec _ = shows . map showPair . unErrorStack
    where showPair (sym, loc) = sym <> " " <> srcLocFile loc <> ":" <> show (srcLocStartLine loc) <> ":" <> show (srcLocStartCol loc)

instance ToJSON ErrorStack where
  toJSON (ErrorStack es) = toJSON (jSite <$> es) where
    jSite (site, SrcLoc{..}) = object
      [ "site" .= site
      , "package" .= srcLocPackage
      , "module" .= srcLocModule
      , "file" .= srcLocFile
      , "startLine" .= srcLocStartLine
      , "startColumn" .= srcLocStartCol
      , "endColumn" .= srcLocEndCol
      ]

instance Hashable ErrorStack where
  hashWithSalt = hashUsing (map (second ((,,,,,,) <$> srcLocPackage <*> srcLocModule <*> srcLocFile <*> srcLocStartLine <*> srcLocStartCol <*> srcLocEndLine <*> srcLocEndCol)) . unErrorStack)

instance Ord ErrorStack where
  compare = liftCompare (liftCompare compareSrcLoc) `on` unErrorStack
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
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Mergeable, FreeVariables1, Declarations1, ToJSONFields1)

instance Diffable Context where
  subalgorithmFor blur focus (Context n s) = Context <$> traverse blur n <*> focus s

  equivalentBySubterm = Just . contextSubject

instance Hashable1 Context where liftHashWithSalt = foldl

instance Eq1 Context where liftEq = genericLiftEq
instance Ord1 Context where liftCompare = genericLiftCompare
instance Show1 Context where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Context where
  eval Context{..} = subtermRef contextSubject
