{-# LANGUAGE DataKinds, DeriveAnyClass, GADTs, RankNTypes, TypeOperators #-}
module Language.Markdown.Syntax
( assignment
, Syntax
, Grammar.Grammar
, Error
, Term
) where

import qualified CMark
import Data.Functor.Union
import Data.Record
import Data.Syntax.Assignment hiding (Assignment, Error)
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Syntax.Markup as Markup
import qualified Data.Syntax as Syntax
import GHC.Stack
import qualified Language.Markdown as Grammar (Grammar(..), NodeType(..))
import Prologue hiding (Location, list)
import qualified Term

type Syntax =
  '[ Markup.Document
   , Markup.Emphasis
   , Markup.Heading
   , Markup.Paragraph
   , Markup.Strong
   , Markup.Text
   , Syntax.Error Error
   , []
   ]

type Error = Assignment.Error Grammar.Grammar
type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment (Cofree [] (Record (CMark.NodeType ': Location))) Grammar.Grammar Term

assignment :: Assignment
assignment = makeTerm <$> symbol Grammar.Document <*> children (Markup.Document <$> many blockElement)

-- Block elements

blockElement :: Assignment
blockElement = paragraph <|> list <|> heading

paragraph :: Assignment
paragraph = makeTerm <$> symbol Grammar.Paragraph <*> children (Markup.Paragraph <$> many inlineElement)

list :: Assignment
list = makeTerm <$> symbol Grammar.List <*> children (many item)

item :: Assignment
item = symbol Grammar.Item *> children blockElement

heading :: Assignment
heading = makeTerm <$> symbol Grammar.Heading <*> (Markup.Heading <$> project (\ ((Grammar.HEADING level :. _) :< _) -> level) <*> children (many inlineElement))


-- Inline elements

inlineElement :: Assignment
inlineElement = strong <|> emphasis <|> text

strong :: Assignment
strong = makeTerm <$> symbol Grammar.Strong <*> children (Markup.Strong <$> many inlineElement)

emphasis :: Assignment
emphasis = makeTerm <$> symbol Grammar.Emphasis <*> children (Markup.Emphasis <$> many inlineElement)

text :: Assignment
text = makeTerm <$> symbol Grammar.Text <*> (Markup.Text <$> source)


makeTerm :: (InUnion fs f, HasCallStack) => a -> f (Term.Term (Union fs) a) -> Term.Term (Union fs) a
makeTerm a f = cofree $ a :< inj f
