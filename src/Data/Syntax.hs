{-# LANGUAGE DeriveAnyClass, TypeOperators #-}
module Data.Syntax where

import Algorithm
import Control.Applicative
import Control.Comonad.Trans.Cofree (headF)
import Control.Monad.Error.Class hiding (Error)
import Data.Align.Generic
import Data.ByteString (ByteString)
import qualified Data.Error as Error
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Ix
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import Data.Record
import Data.Semigroup
import Data.Span
import qualified Data.Syntax.Assignment as Assignment
import Data.Union
import GHC.Generics
import GHC.Stack
import Term

-- Combinators

makeTerm :: (HasCallStack, f :< fs, Semigroup a, Apply1 Foldable fs) => a -> f (Term (Union fs) a) -> Term (Union fs) a
makeTerm a = makeTerm' a . inj

makeTerm' :: (HasCallStack, Semigroup a, Foldable f) => a -> f (Term f a) -> Term f a
makeTerm' a f = cofree (sconcat (a :| (headF . runCofree <$> toList f)) :< f)

makeTerm1 :: (HasCallStack, f :< fs, Semigroup a, Apply1 Foldable fs) => f (Term (Union fs) a) -> Term (Union fs) a
makeTerm1 = makeTerm1' . inj

makeTerm1' :: (HasCallStack, Semigroup a, Foldable f) => f (Term f a) -> Term f a
makeTerm1' f = case toList f of
  a : _ -> makeTerm' (headF (runCofree a)) f
  _ -> error "makeTerm1': empty structure"

emptyTerm :: (HasCallStack, Empty :< fs, Apply1 Foldable fs) => Assignment.Assignment ast grammar (Term (Union fs) (Record Assignment.Location))
emptyTerm = makeTerm <$> Assignment.location <*> pure Empty

handleError :: (HasCallStack, Error :< fs, Show grammar, Apply1 Foldable fs) => Assignment.Assignment ast grammar (Term (Union fs) (Record Assignment.Location)) -> Assignment.Assignment ast grammar (Term (Union fs) (Record Assignment.Location))
handleError = flip catchError (\ err -> makeTerm <$> Assignment.location <*> pure (errorSyntax (either id show <$> err) []) <* Assignment.source)

parseError :: (HasCallStack, Error :< fs, Bounded grammar, Ix grammar, Apply1 Foldable fs) => Assignment.Assignment ast grammar (Term (Union fs) (Record Assignment.Location))
parseError = makeTerm <$> Assignment.symbol maxBound <*> pure (Error (getCallStack (freezeCallStack callStack)) [] Nothing []) <* Assignment.source


contextualize :: (HasCallStack, Context :< fs, Alternative m, Semigroup a, Apply1 Foldable fs)
              => m (Term (Union fs) a)
              -> m (Term (Union fs) a)
              -> m (Term (Union fs) a)
contextualize context rule = make <$> Assignment.manyThrough context rule
  where make (cs, node) = case nonEmpty cs of
          Just cs -> makeTerm1 (Context cs node)
          _ -> node

postContextualizeThrough :: (HasCallStack, Context :< fs, Alternative m, Semigroup a, Apply1 Foldable fs)
                         => m (Term (Union fs) a)
                         -> m (Term (Union fs) a)
                         -> m b
                         -> m (Term (Union fs) a, b)
postContextualizeThrough context rule end = make <$> rule <*> Assignment.manyThrough context end
  where make node (cs, end) = case nonEmpty cs of
          Just cs -> (makeTerm1 (Context cs node), end)
          _ -> (node, end)

postContextualize :: (HasCallStack, Context :< fs, Alternative m, Semigroup a, Apply1 Foldable fs)
                  => m (Term (Union fs) a)
                  -> m (Term (Union fs) a)
                  -> m (Term (Union fs) a)
postContextualize context rule = make <$> rule <*> many context
  where make node cs = case nonEmpty cs of
          Just cs -> makeTerm1 (Context cs node)
          _ -> node

infixContext :: (Context :< fs, Assignment.Parsing m, Semigroup a, HasCallStack, Apply1 Foldable fs)
             => m (Term (Union fs) a)
             -> m (Term (Union fs) a)
             -> m (Term (Union fs) a)
             -> [m (Term (Union fs) a -> Term (Union fs) a -> Union fs (Term (Union fs) a))]
             -> m (Union fs (Term (Union fs) a))
infixContext context left right operators = uncurry (&) <$> postContextualizeThrough context left (Assignment.choice operators) <*> postContextualize context right


-- Undifferentiated

newtype Leaf a = Leaf { leafContent :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Leaf where liftEq = genericLiftEq
instance Show1 Leaf where liftShowsPrec = genericLiftShowsPrec

newtype Branch a = Branch { branchElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Branch where liftEq = genericLiftEq
instance Show1 Branch where liftShowsPrec = genericLiftShowsPrec


-- Common

-- | An identifier of some other construct, whether a containing declaration (e.g. a class name) or a reference (e.g. a variable).
newtype Identifier a = Identifier ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Identifier where liftEq = genericLiftEq
instance Show1 Identifier where liftShowsPrec = genericLiftShowsPrec

newtype Program a = Program [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Program where liftEq = genericLiftEq
instance Show1 Program where liftShowsPrec = genericLiftShowsPrec


-- | Empty syntax, with essentially no-op semantics.
--
--   This can be used to represent an implicit no-op, e.g. the alternative in an 'if' statement without an 'else'.
data Empty a = Empty
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Empty where liftEq _ _ _ = True
instance Show1 Empty where liftShowsPrec _ _ _ _ = showString "Empty"


-- | Syntax representing a parsing or assignment error.
data Error a = Error { errorCallStack :: [([Char], SrcLoc)], errorExpected :: [String], errorActual :: Maybe String, errorChildren :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Error where liftEq = genericLiftEq
instance Show1 Error where liftShowsPrec = genericLiftShowsPrec

errorSyntax :: Error.Error String -> [a] -> Error a
errorSyntax Error.Error{..} = Error (getCallStack callStack) errorExpected errorActual

unError :: Span -> Error a -> Error.Error String
unError span Error{..} = Error.withCallStack (freezeCallStack (fromCallSiteList errorCallStack)) (Error.Error span errorExpected errorActual)


data Context a = Context { contextTerms :: NonEmpty a, contextSubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Context where liftEq = genericLiftEq
instance Show1 Context where liftShowsPrec = genericLiftShowsPrec
