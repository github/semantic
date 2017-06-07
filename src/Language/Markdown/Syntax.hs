{-# LANGUAGE DataKinds, DeriveAnyClass, GADTs, RankNTypes, TypeOperators #-}
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
import qualified Language.Markdown as Grammar (Grammar(..), NodeType(..))
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Syntax as Syntax
import Prologue hiding (Location, Text, list)
import qualified Term

type Syntax =
  '[ Document
   , Paragraph
   , Heading
   , Strong
   , Emphasis
   , Text
   , Syntax.Error Error
   , []
   ]

newtype Document a = Document [a]
 deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Document where liftEq = genericLiftEq
instance Show1 Document where liftShowsPrec = genericLiftShowsPrec

newtype Paragraph a = Paragraph [a]
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Paragraph where liftEq = genericLiftEq
instance Show1 Paragraph where liftShowsPrec = genericLiftShowsPrec

data Heading a = Heading { headingLevel :: Int, headingContent :: [a] }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Heading where liftEq = genericLiftEq
instance Show1 Heading where liftShowsPrec = genericLiftShowsPrec

newtype Strong a = Strong [a]
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Strong where liftEq = genericLiftEq
instance Show1 Strong where liftShowsPrec = genericLiftShowsPrec

newtype Emphasis a = Emphasis [a]
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Emphasis where liftEq = genericLiftEq
instance Show1 Emphasis where liftShowsPrec = genericLiftShowsPrec

newtype Text a = Text ByteString
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Text where liftEq = genericLiftEq
instance Show1 Text where liftShowsPrec = genericLiftShowsPrec

type Error = Assignment.Error Grammar.Grammar
type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment (Cofree [] (Record (CMark.NodeType ': Location))) Grammar.Grammar Term

assignment :: Assignment
assignment = makeTerm <$> symbol Grammar.Document <*> children (Document <$> many blockElement)

-- Block elements

blockElement :: Assignment
blockElement = paragraph <|> list <|> heading

paragraph :: Assignment
paragraph = makeTerm <$> symbol Grammar.Paragraph <*> children (Paragraph <$> many inlineElement)

list :: Assignment
list = makeTerm <$> symbol Grammar.List <*> children (many blockElement)

heading :: Assignment
heading = makeTerm <$> symbol Grammar.Heading <*> (Heading <$> withNode (\ ((Grammar.HEADING level :. _) :< _) -> level) <*> children (many inlineElement))


-- Inline elements

inlineElement :: Assignment
inlineElement = strong <|> emphasis <|> text

strong :: Assignment
strong = makeTerm <$> symbol Grammar.Strong <*> children (Strong <$> many inlineElement)

emphasis :: Assignment
emphasis = makeTerm <$> symbol Grammar.Emphasis <*> children (Emphasis <$> many inlineElement)

text :: Assignment
text = makeTerm <$> symbol Grammar.Text <*> (Text <$> source)


makeTerm :: (InUnion fs f, HasCallStack) => a -> f (Term.Term (Union fs) a) -> Term.Term (Union fs) a
makeTerm a f = cofree $ a :< inj f
