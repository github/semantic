{-# LANGUAGE DataKinds, DeriveAnyClass, GADTs, RankNTypes, TypeOperators #-}
module Language.Markdown.Syntax
( assignment
, Syntax
, Grammar
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
import Language.Markdown as Grammar (Grammar(..))
import Prologue hiding (Location, link, list)
import qualified Term

type Syntax =
  '[ Markup.Document
   -- Block elements
   , Markup.BlockQuote
   , Markup.Heading
   , Markup.OrderedList
   , Markup.Paragraph
   , Markup.UnorderedList
   -- Inline elements
   , Markup.Code
   , Markup.Emphasis
   , Markup.Image
   , Markup.Link
   , Markup.Strong
   , Markup.Text
   -- Assignment errors; cmark does not provide parse errors.
   , Syntax.Error Error
   ]

type Error = Assignment.Error Grammar
type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment (Cofree [] (Record (CMark.NodeType ': Location))) Grammar Term


assignment :: Assignment
assignment = makeTerm <$> symbol Document <*> children (Markup.Document <$> many blockElement)


-- Block elements

blockElement :: Assignment
blockElement = paragraph <|> list <|> heading <|> blockQuote

paragraph :: Assignment
paragraph = makeTerm <$> symbol Paragraph <*> children (Markup.Paragraph <$> many inlineElement)

list :: Assignment
list = (cofree .) . (:<) <$> symbol List <*> (project (\ (((CMark.LIST CMark.ListAttributes{..}) :. _) :< _) -> case listType of
  CMark.BULLET_LIST -> inj . Markup.UnorderedList
  CMark.ORDERED_LIST -> inj . Markup.OrderedList) <*> children (many item))

item :: Assignment
item = symbol Item *> children blockElement

heading :: Assignment
heading = makeTerm <$> symbol Heading <*> (Markup.Heading <$> project (\ ((CMark.HEADING level :. _) :< _) -> level) <*> children (many inlineElement))

blockQuote :: Assignment
blockQuote = makeTerm <$> symbol BlockQuote <*> children (Markup.BlockQuote <$> many blockElement)


-- Inline elements

inlineElement :: Assignment
inlineElement = strong <|> emphasis <|> text <|> link <|> image <|> code

strong :: Assignment
strong = makeTerm <$> symbol Strong <*> children (Markup.Strong <$> many inlineElement)

emphasis :: Assignment
emphasis = makeTerm <$> symbol Emphasis <*> children (Markup.Emphasis <$> many inlineElement)

text :: Assignment
text = makeTerm <$> symbol Text <*> (Markup.Text <$> source)

link :: Assignment
link = makeTerm <$> symbol Link <*> (uncurry Markup.Link <$> project (\ (((CMark.LINK url title) :. _) :< _) -> (toS url, toS title))) <* source

image :: Assignment
image = makeTerm <$> symbol Image <*> (uncurry Markup.Image <$> project (\ (((CMark.IMAGE url title) :. _) :< _) -> (toS url, toS title))) <* source

code :: Assignment
code = makeTerm <$> symbol Code <*> (Markup.Code <$> source)


-- Implementation details

makeTerm :: (InUnion fs f, HasCallStack) => a -> f (Term.Term (Union fs) a) -> Term.Term (Union fs) a
makeTerm a f = cofree $ a :< inj f
