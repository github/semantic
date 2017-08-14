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
          Just cs -> makeTerm (sconcat (headF . runCofree <$> cs)) (Context (toList cs) node)
          _ -> node


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


data Context a = Context { contextTerms :: [a], contextSubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Context where liftEq = genericLiftEq
instance Show1 Context where liftShowsPrec = genericLiftShowsPrec
