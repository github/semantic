{-# LANGUAGE DeriveAnyClass, GADTs, TypeOperators, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables, TypeApplications #-}
module Data.Syntax where

import Abstract.Eval
import Abstract.Value
import Abstract.Type
import Abstract.Primitive
import Abstract.FreeVariables
import Abstract.Environment
import Control.Monad.Effect
import Algorithm hiding (Empty)
import Control.Applicative
import Control.Monad.Error.Class hiding (Error)
import Data.Align.Generic
import Data.ByteString (ByteString)
import qualified Data.Error as Error
import Data.Foldable (asum, toList)
import Data.Function ((&), on)
import Data.Ix
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Functor.Classes.Show.Generic
import Data.Mergeable
import Data.Range
import Data.Record
import Data.Pointed
import Data.Semigroup
import Data.Span
import qualified Data.Syntax.Assignment as Assignment
import Data.Term
import Data.Union
import GHC.Generics
import GHC.Stack

-- Combinators

-- | Lift syntax and an annotation into a term, injecting the syntax into a union & ensuring the annotation encompasses all children.
makeTerm :: (HasCallStack, f :< fs, Semigroup a, Apply Foldable fs) => a -> f (Term (Union fs) a) -> Term (Union fs) a
makeTerm a = makeTerm' a . inj

-- | Lift a union and an annotation into a term, ensuring the annotation encompasses all children.
makeTerm' :: (HasCallStack, Semigroup a, Foldable f) => a -> f (Term f a) -> Term f a
makeTerm' a f = termIn (sconcat (a :| (termAnnotation . unTerm <$> toList f))) f

-- | Lift non-empty syntax into a term, injecting the syntax into a union & appending all subterms’.annotations to make the new term’s annotation.
makeTerm1 :: (HasCallStack, f :< fs, Semigroup a, Apply Foldable fs) => f (Term (Union fs) a) -> Term (Union fs) a
makeTerm1 = makeTerm1' . inj

-- | Lift a non-empty union into a term, appending all subterms’.annotations to make the new term’s annotation.
makeTerm1' :: (HasCallStack, Semigroup a, Foldable f) => f (Term f a) -> Term f a
makeTerm1' f = case toList f of
  a : _ -> makeTerm' (termAnnotation (unTerm a)) f
  _ -> error "makeTerm1': empty structure"

-- | Construct an empty term at the current position.
emptyTerm :: (HasCallStack, Empty :< fs, Apply Foldable fs) => Assignment.Assignment ast grammar (Term (Union fs) (Record Assignment.Location))
emptyTerm = makeTerm . startLocation <$> Assignment.location <*> pure Empty
  where startLocation ann = Range (start (getField ann)) (start (getField ann)) :. Span (spanStart (getField ann)) (spanStart (getField ann)) :. Nil

-- | Catch assignment errors into an error term.
handleError :: (HasCallStack, Error :< fs, Enum grammar, Eq1 ast, Ix grammar, Show grammar, Apply Foldable fs) => Assignment.Assignment ast grammar (Term (Union fs) (Record Assignment.Location)) -> Assignment.Assignment ast grammar (Term (Union fs) (Record Assignment.Location))
handleError = flip catchError (\ err -> makeTerm <$> Assignment.location <*> pure (errorSyntax (either id show <$> err) []) <* Assignment.source)

-- | Catch parse errors into an error term.
parseError :: (HasCallStack, Error :< fs, Bounded grammar, Enum grammar, Ix grammar, Apply Foldable fs) => Assignment.Assignment ast grammar (Term (Union fs) (Record Assignment.Location))
parseError = makeTerm <$> Assignment.token maxBound <*> pure (Error (ErrorStack (getCallStack (freezeCallStack callStack))) [] (Just "ParseError") [])


-- | Match context terms before a subject term, wrapping both up in a Context term if any context terms matched, or otherwise returning the subject term.
contextualize :: (HasCallStack, Context :< fs, Alternative m, Semigroup a, Apply Foldable fs)
              => m (Term (Union fs) a)
              -> m (Term (Union fs) a)
              -> m (Term (Union fs) a)
contextualize context rule = make <$> Assignment.manyThrough context rule
  where make (cs, node) = case nonEmpty cs of
          Just cs -> makeTerm1 (Context cs node)
          _ -> node

-- | Match context terms after a subject term and before a delimiter, returning the delimiter paired with a Context term if any context terms matched, or the subject term otherwise.
postContextualizeThrough :: (HasCallStack, Context :< fs, Alternative m, Semigroup a, Apply Foldable fs)
                         => m (Term (Union fs) a)
                         -> m (Term (Union fs) a)
                         -> m b
                         -> m (Term (Union fs) a, b)
postContextualizeThrough context rule end = make <$> rule <*> Assignment.manyThrough context end
  where make node (cs, end) = case nonEmpty cs of
          Just cs -> (makeTerm1 (Context cs node), end)
          _ -> (node, end)

-- | Match context terms after a subject term, wrapping both up in a Context term if any context terms matched, or otherwise returning the subject term.
postContextualize :: (HasCallStack, Context :< fs, Alternative m, Semigroup a, Apply Foldable fs)
                  => m (Term (Union fs) a)
                  -> m (Term (Union fs) a)
                  -> m (Term (Union fs) a)
postContextualize context rule = make <$> rule <*> many context
  where make node cs = case nonEmpty cs of
          Just cs -> makeTerm1 (Context cs node)
          _ -> node

-- | Match infix terms separated by any of a list of operators, with optional context terms following each operand.
infixContext :: (Context :< fs, Assignment.Parsing m, Semigroup a, HasCallStack, Apply Foldable fs)
             => m (Term (Union fs) a)
             -> m (Term (Union fs) a)
             -> m (Term (Union fs) a)
             -> [m (Term (Union fs) a -> Term (Union fs) a -> Union fs (Term (Union fs) a))]
             -> m (Union fs (Term (Union fs) a))
infixContext context left right operators = uncurry (&) <$> postContextualizeThrough context left (asum operators) <*> postContextualize context right


-- Common

-- | An identifier of some other construct, whether a containing declaration (e.g. a class name) or a reference (e.g. a variable).
newtype Identifier a = Identifier ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Identifier where liftEq = genericLiftEq
instance Ord1 Identifier where liftCompare = genericLiftCompare
instance Show1 Identifier where liftShowsPrec = genericLiftShowsPrec
-- TODO: Implement Eval instance for Identifier
instance (Monad m) => Eval (Value s a l) m Identifier
instance (Monad m) => Eval Type m Identifier

instance FreeVariables1 Identifier where
  liftFreeVariables _ (Identifier x) = point x

newtype Program a = Program [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Program where liftEq = genericLiftEq
instance Ord1 Program where liftCompare = genericLiftCompare
instance Show1 Program where liftShowsPrec = genericLiftShowsPrec

instance ( Monad m
         , Ord l
         , Functor s
         , MonadGC l (Value s a l) m
         , MonadEnv l (Value s a l) m
         , FreeVariables1 s)
        => Eval (Value s a l) m Program where
  eval _  yield (Program [])     = yield (I PUnit)
  eval ev yield (Program [a])    = ev pure a >>= yield
  eval ev yield (Program (a:as)) = do
    env <- askEnv @l @(Value s a l)
    extraRoots (envRoots @l env (freeVariables1 as)) (ev (const (eval ev pure (Program as))) a) >>= yield

instance ( Monad m
         , MonadGC (LocationFor Type) Type m
         , MonadEnv (LocationFor Type) Type m
         )
        => Eval Type m Program where
  eval _  yield (Program [])     = yield Unit
  eval ev yield (Program [a])    = ev pure a >>= yield
  eval ev yield (Program (a:as)) = do
    env <- askEnv @(LocationFor Type) @Type
    extraRoots (envRoots @(LocationFor Type) env (freeVariables1 as)) (ev (const (eval ev pure (Program as))) a) >>= yield

-- | An accessibility modifier, e.g. private, public, protected, etc.
newtype AccessibilityModifier a = AccessibilityModifier ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 AccessibilityModifier where liftEq = genericLiftEq
instance Ord1 AccessibilityModifier where liftCompare = genericLiftCompare
instance Show1 AccessibilityModifier where liftShowsPrec = genericLiftShowsPrec


-- | Empty syntax, with essentially no-op semantics.
--
--   This can be used to represent an implicit no-op, e.g. the alternative in an 'if' statement without an 'else'.
data Empty a = Empty
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Empty where liftEq _ _ _ = True
instance Ord1 Empty where liftCompare _ _ _ = EQ
instance Show1 Empty where liftShowsPrec _ _ _ _ = showString "Empty"
-- TODO: Define Value semantics for Empty
instance (Monad m) => Eval (Value s a l) m Empty where
  eval _ yield _ = yield (I PUnit)
instance (Monad m) => Eval Type m Empty where
  eval _ yield _ = yield Unit


-- | Syntax representing a parsing or assignment error.
data Error a = Error { errorCallStack :: ErrorStack, errorExpected :: [String], errorActual :: Maybe String, errorChildren :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Error where liftEq = genericLiftEq
instance Ord1 Error where liftCompare = genericLiftCompare
instance Show1 Error where liftShowsPrec = genericLiftShowsPrec
-- TODO: Define Value semantics for Error
instance (Monad m) => Eval (Value s a l) m Error
instance (Monad m) => Eval Type m Error

errorSyntax :: Error.Error String -> [a] -> Error a
errorSyntax Error.Error{..} = Error (ErrorStack (getCallStack callStack)) errorExpected errorActual

unError :: Span -> Error a -> Error.Error String
unError span Error{..} = Error.withCallStack (freezeCallStack (fromCallSiteList (unErrorStack errorCallStack))) (Error.Error span errorExpected errorActual)

newtype ErrorStack = ErrorStack { unErrorStack :: [(String, SrcLoc)] }
  deriving (Eq, Show)

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
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Diffable Context where
  subalgorithmFor blur focus (Context n s) = Context <$> traverse blur n <*> focus s

  equivalentBySubterm = Just . contextSubject

instance Eq1 Context where liftEq = genericLiftEq
instance Ord1 Context where liftCompare = genericLiftCompare
instance Show1 Context where liftShowsPrec = genericLiftShowsPrec

instance (Monad m) => Eval (Value s a l) m Context where
  eval ev yield Context{..} = ev pure contextSubject >>= yield

instance (Monad m) => Eval Type m Context where
  eval ev yield Context{..} = ev pure contextSubject >>= yield
