{-# LANGUAGE DeriveAnyClass, TypeOperators #-}
module Data.Syntax where

import Algorithm
import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import Data.Record
import Data.String
import qualified Data.Syntax.Assignment as Assignment
import Data.Union
import GHC.Generics
import Prologue hiding (show)
import Term
import Text.Show

-- Combinators

makeTerm :: (HasCallStack, f :< fs) => a -> f (Term (Union fs) a) -> Term (Union fs) a
makeTerm a f = cofree (a :< inj f)

emptyTerm :: (HasCallStack, Empty :< fs) => Assignment.Assignment ast grammar (Term (Union fs) (Record Assignment.Location))
emptyTerm = makeTerm <$> Assignment.location <*> pure Empty

handleError :: (HasCallStack, Error :< fs, Show grammar) => Assignment.Assignment ast grammar (Term (Union fs) (Record Assignment.Location)) -> Assignment.Assignment ast grammar (Term (Union fs) (Record Assignment.Location))
handleError = flip catchError (\ err -> makeTerm <$> Assignment.location <*> pure (Error (fmap show err) []) <* Assignment.source)


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
data Error a = Error { errorDetails :: Assignment.Error String, errorChildren :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Error where liftEq = genericLiftEq
instance Show1 Error where liftShowsPrec = genericLiftShowsPrec
