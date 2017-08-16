{-# LANGUAGE DataKinds, DeriveAnyClass, GADTs, RankNTypes, TypeOperators #-}
module Language.Markdown.Syntax
( assignment
, Syntax
, Grammar
, Term
) where

import Control.Comonad.Cofree (Cofree(..), unwrap)
import qualified CMarkGFM
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Record
import Data.Syntax (makeTerm)
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment hiding (Assignment, Error)
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Syntax.Markup as Markup
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Union
import GHC.Stack
import Language.Markdown as Grammar (Grammar(..))
import qualified Term

type Syntax =
  '[ Markup.Document
   -- Block elements
   , Markup.BlockQuote
   , Markup.Heading
   , Markup.HTMLBlock
   , Markup.OrderedList
   , Markup.Paragraph
   , Markup.Section
   , Markup.ThematicBreak
   , Markup.UnorderedList
   , Markup.Table
   , Markup.TableRow
   , Markup.TableCell
   -- Inline elements
   , Markup.Code
   , Markup.Emphasis
   , Markup.Image
   , Markup.LineBreak
   , Markup.Link
   , Markup.Strong
   , Markup.Text
   , Markup.Strikethrough
   -- Assignment errors; cmark does not provide parse errors.
   , Syntax.Error
   , []
   ]

type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment (AST CMarkGFM.NodeType) Grammar Term


assignment :: Assignment
assignment = makeTerm <$> symbol Document <*> children (Markup.Document <$> many blockElement)


-- Block elements

blockElement :: Assignment
blockElement = paragraph <|> list <|> blockQuote <|> codeBlock <|> thematicBreak <|> htmlBlock <|> section <|> table

paragraph :: Assignment
paragraph = makeTerm <$> symbol Paragraph <*> children (Markup.Paragraph <$> many inlineElement)

list :: Assignment
list = (:<) <$> symbol List <*> ((\ (Node (CMarkGFM.LIST CMarkGFM.ListAttributes{..}) _ _ Term.:< _) -> case listType of
  CMarkGFM.BULLET_LIST -> inj . Markup.UnorderedList
  CMarkGFM.ORDERED_LIST -> inj . Markup.OrderedList) <$> currentNode <*> children (many item))

item :: Assignment
item = makeTerm <$> symbol Item <*> children (many blockElement)

section :: Assignment
section = makeTerm <$> symbol Heading <*> (heading >>= \ headingTerm -> Markup.Section (level headingTerm) headingTerm <$> while (((<) `on` level) headingTerm) blockElement)
  where heading = makeTerm <$> symbol Heading <*> ((\ (Node (CMarkGFM.HEADING level) _ _ Term.:< _) -> Markup.Heading level) <$> currentNode <*> children (many inlineElement))
        level term = case term of
          _ | Just section <- prj (unwrap term) -> level (Markup.sectionHeading section)
          _ | Just heading <- prj (unwrap term) -> Markup.headingLevel heading
          _ -> maxBound

blockQuote :: Assignment
blockQuote = makeTerm <$> symbol BlockQuote <*> children (Markup.BlockQuote <$> many blockElement)

codeBlock :: Assignment
codeBlock = makeTerm <$> symbol CodeBlock <*> ((\ (Node (CMarkGFM.CODE_BLOCK language _) _ _ Term.:< _) -> Markup.Code (nullText language)) <$> currentNode <*> source)

thematicBreak :: Assignment
thematicBreak = makeTerm <$> token ThematicBreak <*> pure Markup.ThematicBreak

htmlBlock :: Assignment
htmlBlock = makeTerm <$> symbol HTMLBlock <*> (Markup.HTMLBlock <$> source)

table :: Assignment
table = makeTerm <$> symbol Table <*> children (Markup.Table <$> many tableRow)

tableRow :: Assignment
tableRow = makeTerm <$> symbol TableRow <*> children (Markup.TableRow <$> many tableCell)

tableCell :: Assignment
tableCell = makeTerm <$> symbol TableCell <*> children (Markup.TableCell <$> many inlineElement)

-- Inline elements

inlineElement :: Assignment
inlineElement = strong <|> emphasis <|> strikethrough <|> text <|> link <|> htmlInline <|> image <|> code <|> lineBreak <|> softBreak

strong :: Assignment
strong = makeTerm <$> symbol Strong <*> children (Markup.Strong <$> many inlineElement)

emphasis :: Assignment
emphasis = makeTerm <$> symbol Emphasis <*> children (Markup.Emphasis <$> many inlineElement)

strikethrough :: Assignment
strikethrough = makeTerm <$> symbol Strikethrough <*> children (Markup.Strikethrough <$> many inlineElement)

text :: Assignment
text = makeTerm <$> symbol Text <*> (Markup.Text <$> source)

htmlInline :: Assignment
htmlInline = makeTerm <$> symbol HTMLInline <*> (Markup.HTMLBlock <$> source)

link :: Assignment
link = makeTerm <$> symbol Link <*> ((\ (Node (CMarkGFM.LINK url title) _ _ Term.:< _) -> Markup.Link (encodeUtf8 url) (nullText title)) <$> currentNode) <* advance

image :: Assignment
image = makeTerm <$> symbol Image <*> ((\ (Node (CMarkGFM.IMAGE url title) _ _ Term.:< _) -> Markup.Image (encodeUtf8 url) (nullText title)) <$> currentNode) <* advance

code :: Assignment
code = makeTerm <$> symbol Code <*> (Markup.Code Nothing <$> source)

lineBreak :: Assignment
lineBreak = makeTerm <$> token LineBreak <*> pure Markup.LineBreak

softBreak :: Assignment
softBreak = makeTerm <$> token SoftBreak <*> pure Markup.LineBreak


-- Implementation details

nullText :: Text.Text -> Maybe ByteString
nullText text = if Text.null text then Nothing else Just (encodeUtf8 text)
