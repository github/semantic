{-# LANGUAGE DeriveAnyClass, TypeOperators #-}
module Data.Syntax where

import Algorithm hiding (Empty)
import Control.Applicative
import Control.Monad.Error.Class hiding (Error)
import Data.Align.Generic
import Data.ByteString (ByteString)
import qualified Data.Error as Error
import Data.Foldable (asum, toList)
import Data.Function ((&))
import Data.Ix
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import Data.Mergeable
import Data.Record
import Data.Semigroup
import Data.Span
import qualified Data.Syntax.Assignment as Assignment
import Data.Union
import GHC.Generics
import GHC.Stack
import Term

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
emptyTerm = makeTerm <$> Assignment.location <*> pure Empty

-- | Catch assignment errors into an error term.
handleError :: (HasCallStack, Error :< fs, Enum grammar, Eq1 ast, Ix grammar, Show grammar, Apply Foldable fs) => Assignment.Assignment ast grammar (Term (Union fs) (Record Assignment.Location)) -> Assignment.Assignment ast grammar (Term (Union fs) (Record Assignment.Location))
handleError = flip catchError (\ err -> makeTerm <$> Assignment.location <*> pure (errorSyntax (either id show <$> err) []) <* Assignment.source)

-- | Catch parse errors into an error term.
parseError :: (HasCallStack, Error :< fs, Bounded grammar, Enum grammar, Ix grammar, Apply Foldable fs) => Assignment.Assignment ast grammar (Term (Union fs) (Record Assignment.Location))
parseError = makeTerm <$> Assignment.token maxBound <*> pure (Error (getCallStack (freezeCallStack callStack)) [] (Just "ParseError") [])


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
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Show, Traversable)

instance Eq1 Identifier where liftEq = genericLiftEq
instance Show1 Identifier where liftShowsPrec = genericLiftShowsPrec

newtype Program a = Program [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Show, Traversable)

instance Eq1 Program where liftEq = genericLiftEq
instance Show1 Program where liftShowsPrec = genericLiftShowsPrec


-- | Empty syntax, with essentially no-op semantics.
--
--   This can be used to represent an implicit no-op, e.g. the alternative in an 'if' statement without an 'else'.
data Empty a = Empty
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Show, Traversable)

instance Eq1 Empty where liftEq _ _ _ = True
instance Show1 Empty where liftShowsPrec _ _ _ _ = showString "Empty"


-- | Syntax representing a parsing or assignment error.
data Error a = Error { errorCallStack :: [([Char], SrcLoc)], errorExpected :: [String], errorActual :: Maybe String, errorChildren :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Show, Traversable)

instance Eq1 Error where liftEq = genericLiftEq
instance Show1 Error where liftShowsPrec = genericLiftShowsPrec

errorSyntax :: Error.Error String -> [a] -> Error a
errorSyntax Error.Error{..} = Error (getCallStack callStack) errorExpected errorActual

unError :: Span -> Error a -> Error.Error String
unError span Error{..} = Error.withCallStack (freezeCallStack (fromCallSiteList errorCallStack)) (Error.Error span errorExpected errorActual)


data Context a = Context { contextTerms :: NonEmpty a, contextSubject :: a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Show, Traversable)

instance Diffable Context where
  subalgorithmFor blur focus (Context n1 s1) = Context <$> traverse blur n1 <*> focus s1

instance Eq1 Context where liftEq = genericLiftEq
instance Show1 Context where liftShowsPrec = genericLiftShowsPrec
