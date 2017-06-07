{-# LANGUAGE DataKinds, DeriveAnyClass, RankNTypes, TypeOperators #-}
module Language.Markdown.Syntax
( assignment
, Syntax
, Grammar.Grammar
, Error
, Term
) where

import qualified CMark
import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import Data.Functor.Union
import Data.Record
import Data.Syntax.Assignment hiding (Assignment, Error)
import GHC.Generics
import GHC.Stack
import qualified Language.Markdown as Grammar (Grammar(..))
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Syntax as Syntax
import Prologue hiding (Location)
import qualified Term

type Syntax =
  '[ Document
   , Paragraph
   , Syntax.Error Error
   ]

newtype Document a = Document [a]
 deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Document where liftEq = genericLiftEq
instance Show1 Document where liftShowsPrec = genericLiftShowsPrec

newtype Paragraph a = Paragraph [a]
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Paragraph where liftEq = genericLiftEq
instance Show1 Paragraph where liftShowsPrec = genericLiftShowsPrec


type Error = Assignment.Error Grammar.Grammar
type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment (Cofree [] (Record (CMark.NodeType ': Location))) Grammar.Grammar Term

assignment :: Assignment
assignment = makeTerm <$> symbol Grammar.Document <*> children (Document <$> many paragraph)

inlineElement :: Assignment
inlineElement = empty

paragraph :: Assignment
paragraph = makeTerm <$> symbol Grammar.Paragraph <*> children (Paragraph <$> many inlineElement)


makeTerm :: (InUnion fs f, HasCallStack) => a -> f (Term.Term (Union fs) a) -> Term.Term (Union fs) a
makeTerm a f = cofree $ a :< inj f
