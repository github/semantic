{-# LANGUAGE AllowAmbiguousTypes, DeriveAnyClass, GADTs, TypeOperators, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables, KindSignatures, RankNTypes, ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For HasCallStack
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
makeTerm :: (HasCallStack, f :< fs, Semigroup a, Apply Foldable fs) => a -> f (Term (Sum fs) a) -> Term (Sum fs) a
makeTerm a = makeTerm' a . inject

-- | Lift a union and an annotation into a term, ensuring the annotation encompasses all children.
makeTerm' :: (HasCallStack, Semigroup a, Foldable f) => a -> f (Term f a) -> Term f a
makeTerm' a f = termIn (sconcat (a :| (termAnnotation <$> toList f))) f

-- | Lift syntax and an annotation into a term, injecting the syntax into a union & ensuring the annotation encompasses all children. Removes extra structure if term is a list of a single item.
makeTerm'' :: (HasCallStack, f :< fs, Semigroup a, Apply Foldable fs, Foldable f) => a -> f (Term (Sum fs) a) -> Term (Sum fs) a
makeTerm'' a children = case toList children of
  [x] -> x
  _ -> makeTerm' a (inject children)

-- | Lift non-empty syntax into a term, injecting the syntax into a union & appending all subterms’.annotations to make the new term’s annotation.
makeTerm1 :: (HasCallStack, f :< fs, Semigroup a, Apply Foldable fs) => f (Term (Sum fs) a) -> Term (Sum fs) a
makeTerm1 = makeTerm1' . inject

-- | Lift a non-empty union into a term, appending all subterms’.annotations to make the new term’s annotation.
makeTerm1' :: (HasCallStack, Semigroup a, Foldable f) => f (Term f a) -> Term f a
makeTerm1' f = case toList f of
  a : _ -> makeTerm' (termAnnotation a) f
  _ -> error "makeTerm1': empty structure"

-- | Construct an empty term at the current position.
emptyTerm :: (HasCallStack, Empty :< fs, Apply Foldable fs) => Assignment.Assignment ast grammar (Term (Sum fs) (Record Location))
emptyTerm = makeTerm . startLocation <$> Assignment.location <*> pure Empty
  where startLocation ann = Range (start (getField ann)) (start (getField ann)) :. Span (spanStart (getField ann)) (spanStart (getField ann)) :. Nil

-- | Catch assignment errors into an error term.
handleError :: (HasCallStack, Error :< fs, Enum grammar, Eq1 ast, Ix grammar, Show grammar, Apply Foldable fs) => Assignment.Assignment ast grammar (Term (Sum fs) (Record Location)) -> Assignment.Assignment ast grammar (Term (Sum fs) (Record Location))
handleError = flip catchError (\ err -> makeTerm <$> Assignment.location <*> pure (errorSyntax (either id show <$> err) []) <* Assignment.source)

-- | Catch parse errors into an error term.
parseError :: (HasCallStack, Error :< fs, Bounded grammar, Enum grammar, Ix grammar, Apply Foldable fs) => Assignment.Assignment ast grammar (Term (Sum fs) (Record Location))
parseError = makeTerm <$> Assignment.token maxBound <*> pure (Error (ErrorStack (getCallStack (freezeCallStack callStack))) [] (Just "ParseError") [])


-- | Match context terms before a subject term, wrapping both up in a Context term if any context terms matched, or otherwise returning the subject term.
contextualize :: (HasCallStack, Context :< fs, Alternative m, Semigroup a, Apply Foldable fs)
              => m (Term (Sum fs) a)
              -> m (Term (Sum fs) a)
              -> m (Term (Sum fs) a)
contextualize context rule = make <$> Assignment.manyThrough context rule
  where make (cs, node) = case nonEmpty cs of
          Just cs -> makeTerm1 (Context cs node)
          _ -> node

-- | Match context terms after a subject term and before a delimiter, returning the delimiter paired with a Context term if any context terms matched, or the subject term otherwise.
postContextualizeThrough :: (HasCallStack, Context :< fs, Alternative m, Semigroup a, Apply Foldable fs)
                         => m (Term (Sum fs) a)
                         -> m (Term (Sum fs) a)
                         -> m b
                         -> m (Term (Sum fs) a, b)
postContextualizeThrough context rule end = make <$> rule <*> Assignment.manyThrough context end
  where make node (cs, end) = case nonEmpty cs of
          Just cs -> (makeTerm1 (Context cs node), end)
          _ -> (node, end)

-- | Match context terms after a subject term, wrapping both up in a Context term if any context terms matched, or otherwise returning the subject term.
postContextualize :: (HasCallStack, Context :< fs, Alternative m, Semigroup a, Apply Foldable fs)
                  => m (Term (Sum fs) a)
                  -> m (Term (Sum fs) a)
                  -> m (Term (Sum fs) a)
postContextualize context rule = make <$> rule <*> many context
  where make node cs = case nonEmpty cs of
          Just cs -> makeTerm1 (Context cs node)
          _ -> node

-- | Match infix terms separated by any of a list of operators, with optional context terms following each operand.
infixContext :: (Context :< fs, Assignment.Parsing m, Semigroup a, HasCallStack, Apply Foldable fs)
             => m (Term (Sum fs) a)
             -> m (Term (Sum fs) a)
             -> m (Term (Sum fs) a)
             -> [m (Term (Sum fs) a -> Term (Sum fs) a -> Sum fs (Term (Sum fs) a))]
             -> m (Sum fs (Term (Sum fs) a))
infixContext context left right operators = uncurry (&) <$> postContextualizeThrough context left (asum operators) <*> postContextualize context right

instance (Apply Message1 fs, Generate Message1 fs fs, Generate NameOf1 fs fs) => Message1 (Sum fs) where
  liftEncodeMessage encodeMessage num fs = apply @Message1 (liftEncodeMessage encodeMessage num) fs
  liftDecodeMessage decodeMessage num = oneof undefined listOfParsers
    where
      listOfParsers =
        -- zipWith (\i generator -> (FieldNumber i, generator (FieldNumber i))) [1..] (generate @fs @fs (Proxy @fs) decodeMessage)
        generate @Message1 @fs @fs (\ (proxy :: proxy f) i -> let num = FieldNumber (fromInteger (succ i)) in [(num, fromJust <$> embedded (inject @f @fs <$> liftDecodeMessage decodeMessage num))])
  liftDotProto dotProto _ =
    [Proto.DotProtoMessageOneOf (Proto.Single "syntax") (generate @NameOf1 @fs @fs (\ (proxy :: proxy f) i ->
      let
        num = FieldNumber (fromInteger (succ i))
        fieldType = Proto.Prim (Proto.Named . Proto.Single $ nameOf1 (Proxy @f))
        fieldName = Proto.Single (camelCase $ nameOf1 (Proxy @f))
        camelCase (x : xs) = toLower x : xs
        camelCase [] = []
      in
        [ Proto.DotProtoField num fieldType fieldName [] Nothing ]))]

class NameOf1 (f :: * -> *) where
  nameOf1 :: proxy f -> String

instance (Generic1 f, Rep1 f ~ D1 c f, Datatype c) => NameOf1 f where
  nameOf1 _ = datatypeName (undefined :: t c f a)

class Generate (c :: (* -> *) -> Constraint) (all :: [* -> *]) (fs :: [* -> *]) where
  generate :: Monoid b => (forall f proxy. (Element f all, c f) => proxy f -> Integer -> b) -> b

instance Generate c all '[] where
  generate _ = mempty

instance (Element f all, c f, Generate c all fs) => Generate c all (f ': fs) where
  generate each = (each (Proxy @f) (natVal (Proxy @(ElemIndex f all)))) `mappend` generate @c @all @fs each

-- Common

-- | An identifier of some other construct, whether a containing declaration (e.g. a class name) or a reference (e.g. a variable).
newtype Identifier a = Identifier Name
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, Generic, Named)

instance Eq1 Identifier where liftEq = genericLiftEq
instance Ord1 Identifier where liftCompare = genericLiftCompare
instance Show1 Identifier where liftShowsPrec = genericLiftShowsPrec

-- Propagating the identifier name into JSON is handled with the IdentifierName analysis.
instance ToJSONFields1 Identifier

instance Evaluatable Identifier where
  eval (Identifier name) = pure (LvalLocal name)

instance FreeVariables1 Identifier where
  liftFreeVariables _ (Identifier x) = pure x

instance Declarations1 Identifier where
  liftDeclaredName _ (Identifier x) = pure x

newtype Program a = Program [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Program where liftEq = genericLiftEq
instance Ord1 Program where liftCompare = genericLiftCompare
instance Show1 Program where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Program

instance Evaluatable Program where
  eval (Program xs) = eval xs

-- | An accessibility modifier, e.g. private, public, protected, etc.
newtype AccessibilityModifier a = AccessibilityModifier ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 AccessibilityModifier where liftEq = genericLiftEq
instance Ord1 AccessibilityModifier where liftCompare = genericLiftCompare
instance Show1 AccessibilityModifier where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 AccessibilityModifier

-- TODO: Implement Eval instance for AccessibilityModifier
instance Evaluatable AccessibilityModifier

-- | Empty syntax, with essentially no-op semantics.
--
--   This can be used to represent an implicit no-op, e.g. the alternative in an 'if' statement without an 'else'.
data Empty a = Empty
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1, Generic, Named, Message, Message1)

instance ToJSONFields1 Empty

instance Eq1 Empty where liftEq _ _ _ = True
instance Ord1 Empty where liftCompare _ _ _ = EQ
instance Show1 Empty where liftShowsPrec _ _ _ _ = showString "Empty"

instance Evaluatable Empty where
  eval _ = Rval <$> unit

data Identity a = Identity a
  deriving (Generic1, Message1)
data Product a = Product a a
  deriving (Generic1, Message1)


-- | Syntax representing a parsing or assignment error.
data Error a = Error { errorCallStack :: ErrorStack, errorExpected :: [String], errorActual :: Maybe String, errorChildren :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Error where liftEq = genericLiftEq
instance Ord1 Error where liftCompare = genericLiftCompare
instance Show1 Error where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Error

instance ToJSONFields1 Error where
  toJSONFields1 f@Error{..} = withChildren f [ "stack" .= errorCallStack
                                             , "expected" .= errorExpected
                                             , "actual" .= errorActual
                                             ]


errorSyntax :: Error.Error String -> [a] -> Error a
errorSyntax Error.Error{..} = Error (ErrorStack (getCallStack callStack)) errorExpected errorActual

unError :: Span -> Error a -> Error.Error String
unError span Error{..} = Error.withCallStack (freezeCallStack (fromCallSiteList (unErrorStack errorCallStack))) (Error.Error span errorExpected errorActual)

newtype ErrorStack = ErrorStack { unErrorStack :: [(String, SrcLoc)] }
  deriving (Eq, Show)

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
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance ToJSONFields1 Context

instance Diffable Context where
  subalgorithmFor blur focus (Context n s) = Context <$> traverse blur n <*> focus s

  equivalentBySubterm = Just . contextSubject

instance Hashable1 Context where liftHashWithSalt = foldl

instance Eq1 Context where liftEq = genericLiftEq
instance Ord1 Context where liftCompare = genericLiftCompare
instance Show1 Context where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Context where
  eval Context{..} = subtermRef contextSubject
