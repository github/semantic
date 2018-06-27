{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.Markdown.Assignment
( assignment
, Syntax
, Grammar
, Language.Markdown.Assignment.Term
) where

import Assigning.Assignment hiding (Assignment, Error)
import Data.Record
import Data.Syntax (makeTerm)
import Data.Term as Term (Term(..), TermF(..), termFAnnotation, termFOut, termIn)
import Parsing.CMark as Grammar (Grammar(..))
import qualified Assigning.Assignment as Assignment
import qualified CMarkGFM
import Data.Sum
import qualified Data.Syntax as Syntax
import qualified Data.Text as Text
import qualified Language.Markdown.Syntax as Markup
import Prologue

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

type Term = Term.Term (Sum Syntax) (Record Location)
type Assignment = Assignment.Assignment (TermF [] CMarkGFM.NodeType) Grammar Language.Markdown.Assignment.Term


assignment :: Assignment
assignment = Syntax.handleError $ makeTerm <$> symbol Document <*> children (Markup.Document <$> many blockElement)


-- Block elements

blockElement :: Assignment
blockElement = choice
  [ paragraph
  , list
  , blockQuote
  , codeBlock
  , thematicBreak
  , htmlBlock
  , heading
  , table
  ]

paragraph :: Assignment
paragraph = makeTerm <$> symbol Paragraph <*> children (Markup.Paragraph <$> many inlineElement)

list :: Assignment
list = termIn <$> symbol List <*> (makeList . termFAnnotation . termFOut <$> currentNode <*> children (many item))
  where
    makeList (CMarkGFM.LIST CMarkGFM.ListAttributes{..}) = case listType of
      CMarkGFM.BULLET_LIST -> inject . Markup.UnorderedList
      CMarkGFM.ORDERED_LIST -> inject . Markup.OrderedList
    makeList _ = inject . Markup.UnorderedList

item :: Assignment
item = makeTerm <$> symbol Item <*> children (many blockElement)

heading :: Assignment
heading = makeTerm <$> symbol Heading <*> (makeHeading . termFAnnotation . termFOut <$> currentNode <*> children (many inlineElement) <*> manyTill blockElement (void (symbol Heading) <|> eof))
  where
    makeHeading (CMarkGFM.HEADING level) = Markup.Heading level
    makeHeading _ = Markup.Heading 0

blockQuote :: Assignment
blockQuote = makeTerm <$> symbol BlockQuote <*> children (Markup.BlockQuote <$> many blockElement)

codeBlock :: Assignment
codeBlock = makeTerm <$> symbol CodeBlock <*> (makeCode . termFAnnotation . termFOut <$> currentNode <*> source)
  where
    makeCode (CMarkGFM.CODE_BLOCK language _) = Markup.Code (nullText language)
    makeCode _ = Markup.Code Nothing

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
inlineElement = choice
  [ strong
  , emphasis
  , strikethrough
  , text
  , link
  , htmlInline
  , image
  , code
  , lineBreak
  , softBreak
  ]

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
link = makeTerm <$> symbol Link <*> (makeLink . termFAnnotation . termFOut <$> currentNode) <* advance
  where
    makeLink (CMarkGFM.LINK url title) = Markup.Link url (nullText title)
    makeLink _ = Markup.Link mempty Nothing

image :: Assignment
image = makeTerm <$> symbol Image <*> (makeImage . termFAnnotation . termFOut <$> currentNode) <* advance
  where
    makeImage (CMarkGFM.IMAGE url title) = Markup.Image url (nullText title)
    makeImage _ = Markup.Image mempty Nothing

code :: Assignment
code = makeTerm <$> symbol Code <*> (Markup.Code Nothing <$> source)

lineBreak :: Assignment
lineBreak = makeTerm <$> token LineBreak <*> pure Markup.LineBreak

softBreak :: Assignment
softBreak = makeTerm <$> token SoftBreak <*> pure Markup.LineBreak


-- Implementation details

nullText :: Text.Text -> Maybe Text.Text
nullText text = if Text.null text then Nothing else Just text
