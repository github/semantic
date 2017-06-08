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
import qualified Data.Text as Text
import GHC.Stack
import Language.Markdown as Grammar (Grammar(..))
import Prologue hiding (Location, link, list)
import qualified Term

type Syntax =
  '[ Markup.Document
   -- Block elements
   , Markup.BlockQuote
   , Markup.Heading
   , Markup.HTMLBlock
   , Markup.OrderedList
   , Markup.Paragraph
   , Markup.ThematicBreak
   , Markup.UnorderedList
   -- Inline elements
   , Markup.Code
   , Markup.Emphasis
   , Markup.Image
   , Markup.LineBreak
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
blockElement = paragraph <|> list <|> heading <|> blockQuote <|> codeBlock <|> thematicBreak <|> htmlBlock

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

codeBlock :: Assignment
codeBlock = makeTerm <$> symbol CodeBlock <*> (Markup.Code <$> project (\ (((CMark.CODE_BLOCK language _) :. _) :< _) -> nullText language) <*> source)

thematicBreak :: Assignment
thematicBreak = makeTerm <$> symbol ThematicBreak <*> (Markup.ThematicBreak <$ source)

htmlBlock :: Assignment
htmlBlock = makeTerm <$> symbol HTMLBlock <*> (Markup.HTMLBlock <$> source)


-- Inline elements

inlineElement :: Assignment
inlineElement = strong <|> emphasis <|> text <|> link <|> image <|> code <|> lineBreak <|> softBreak

strong :: Assignment
strong = makeTerm <$> symbol Strong <*> children (Markup.Strong <$> many inlineElement)

emphasis :: Assignment
emphasis = makeTerm <$> symbol Emphasis <*> children (Markup.Emphasis <$> many inlineElement)

text :: Assignment
text = makeTerm <$> symbol Text <*> (Markup.Text <$> source)

link :: Assignment
link = makeTerm <$> symbol Link <*> (uncurry Markup.Link <$> project (\ (((CMark.LINK url title) :. _) :< _) -> (toS url, nullText title))) <* source

image :: Assignment
image = makeTerm <$> symbol Image <*> (uncurry Markup.Image <$> project (\ (((CMark.IMAGE url title) :. _) :< _) -> (toS url, nullText title))) <* source

code :: Assignment
code = makeTerm <$> symbol Code <*> (Markup.Code Nothing <$> source)

lineBreak :: Assignment
lineBreak = makeTerm <$> symbol LineBreak <*> (Markup.LineBreak <$ source)

softBreak :: Assignment
softBreak = makeTerm <$> symbol SoftBreak <*> (Markup.LineBreak <$ source)


-- Implementation details

makeTerm :: (InUnion fs f, HasCallStack) => a -> f (Term.Term (Union fs) a) -> Term.Term (Union fs) a
makeTerm a f = cofree $ a :< inj f

nullText :: Text.Text -> Maybe ByteString
nullText text = if Text.null text then Nothing else Just (toS text)
