{-# LANGUAGE DeriveAnyClass, GADTs, TypeOperators, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For HasCallStack
module Data.Syntax where

import Data.Abstract.Evaluatable
import Data.Aeson (ToJSON(..), ToJSON1(..), object)
import Data.AST
import Data.JSON.Fields
import Data.Range
import Data.Record
import Data.Span
import Data.Semigroup.App
import Data.Semigroup.Foldable
import Data.Sum
import Data.Term
import Diffing.Algorithm hiding (Empty)
import Prelude
import Prologue
import qualified GHC.Exts as Exts
import qualified Assigning.Assignment as Assignment
import qualified Data.Error as Error

-- Combinators

-- | Lift syntax and an annotation into a term, injecting the syntax into a union & ensuring the annotation encompasses all children.
makeTerm :: (HasCallStack, f :< fs, Semigroup a, Apply Foldable fs) => a -> f (Term (Sum fs) a) -> Term (Sum fs) a
makeTerm a = makeTerm' a . inject

-- | Lift a union and an annotation into a term, ensuring the annotation encompasses all children.
makeTerm' :: (HasCallStack, Semigroup a, Foldable f) => a -> f (Term f a) -> Term f a
makeTerm' a f = termIn (sconcat (a :| (termAnnotation <$> toList f))) f

-- | Lift non-empty syntax into a term, injecting the syntax into a union & appending all subterms’.annotations to make the new term’s annotation.
makeTerm1 :: (HasCallStack, f :< fs, Semigroup a, Apply Foldable fs) => f (Term (Sum fs) a) -> Term (Sum fs) a
makeTerm1 = makeTerm1' . inject

-- | Lift a non-empty union into a term, appending all subterms’.annotations to make the new term’s annotation.
makeTerm1' :: (HasCallStack, Semigroup a, Foldable f) => f (Term f a) -> Term f a
makeTerm1' f = case toList f of
  a : _ -> makeTerm' (termAnnotation a) f
  _ -> error "makeTerm1': empty structure"

-- FIXME: I think this might be an anti-pattern.
-- | Lift syntax and an annotation into a term, injecting the syntax into a union & ensuring the annotation encompasses all children. Removes extra structure if term is a list of a single item.
makeStatementTerm :: (HasCallStack, f :< fs, Statements :< fs, Semigroup a, Apply Foldable fs, Foldable f) => a -> f (Term (Sum fs) a) -> Term (Sum fs) a
makeStatementTerm a children = case toList children of
  [x] -> x
  xs -> makeTerm' a (inject (Statements xs))

-- | Construct an empty term at the current position.
emptyTerm :: (HasCallStack, Empty :< fs, Apply Foldable fs) => Assignment.Assignment ast grammar (Term (Sum fs) (Record Location))
emptyTerm = makeTerm . startLocation <$> Assignment.location <*> pure Empty
  where startLocation ann = Range (start (getField ann)) (start (getField ann)) :. Span (spanStart (getField ann)) (spanStart (getField ann)) :. Nil

-- | Construct zero or more Statements.
manyStatements :: (Enum grammar, Eq1 ast, Ix grammar, Show grammar) => Assignment.Assignment ast grammar (Term f a) -> Assignment.Assignment ast grammar (Statements (Term f a))
manyStatements expr = Exts.fromList <$> (many expr)

-- | Construct an empty list of Statements.
emptyStatements :: Assignment.Assignment ast grammar (Statements (Term f a))
emptyStatements = pure (Exts.fromList [])

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


-- Common

-- | An identifier of some other construct, whether a containing declaration (e.g. a class name) or a reference (e.g. a variable).
newtype Identifier a = Identifier { name :: Name }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, ToJSONFields1)

instance Eq1 Identifier where liftEq = genericLiftEq
instance Ord1 Identifier where liftCompare = genericLiftCompare
instance Show1 Identifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Identifier where
  eval (Identifier name) = pure (LvalLocal name)

instance FreeVariables1 Identifier where
  liftFreeVariables _ (Identifier x) = pure x

instance Declarations1 Identifier where
  liftDeclaredName _ (Identifier x) = pure x


newtype Program a = Program (Statements a)
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 Program where liftEq = genericLiftEq
instance Ord1 Program where liftCompare = genericLiftCompare
instance Show1 Program where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Program where
  eval (Program statements) = eval statements

-- | Imperative sequence of statements/declarations
--
--   1. Each statement’s effects on the store are accumulated;
--   2. Each statement can affect the environment of later statements (e.g. by 'modify'-ing the environment); and
--   3. Only the last statement’s return value is returned.
newtype Statements a = Statements [a]
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 Statements where liftEq = genericLiftEq
instance Ord1 Statements where liftCompare = genericLiftCompare
instance Show1 Statements where liftShowsPrec = genericLiftShowsPrec
instance ToJSON1 Statements

-- | Imperative sequence of statements is evaluated s.t:
--
--   1. Each statement’s effects on the store are accumulated;
--   2. Each statement can affect the environment of later statements (e.g. by 'modify'-ing the environment); and
--   3. Only the last statement’s return value is returned.
instance Evaluatable Statements where
  eval (Statements xs) = maybe (pure (Rval unit)) (runApp . foldMap1 (App . subtermRef)) (nonEmpty xs)

instance Exts.IsList (Statements a) where
  type Item (Statements a) = a
  fromList = Statements
  toList (Statements xs) = xs


-- | An accessibility modifier, e.g. private, public, protected, etc.
newtype AccessibilityModifier a = AccessibilityModifier ByteString
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 AccessibilityModifier where liftEq = genericLiftEq
instance Ord1 AccessibilityModifier where liftCompare = genericLiftCompare
instance Show1 AccessibilityModifier where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for AccessibilityModifier
instance Evaluatable AccessibilityModifier

-- | Empty syntax, with essentially no-op semantics.
--
--   This can be used to represent an implicit no-op, e.g. the alternative in an 'if' statement without an 'else'.
data Empty a = Empty
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance ToJSONFields1 Empty

instance Eq1 Empty where liftEq _ _ _ = True
instance Ord1 Empty where liftCompare _ _ _ = EQ
instance Show1 Empty where liftShowsPrec _ _ _ _ = showString "Empty"

instance Evaluatable Empty where
  eval _ = pure (Rval unit)


-- | Syntax representing a parsing or assignment error.
data Error a = Error { errorCallStack :: ErrorStack, errorExpected :: [String], errorActual :: Maybe String, errorChildren :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

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
  deriving (Eq, Foldable, Functor, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

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
