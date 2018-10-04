{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- FIXME
module Language.Markdown.Assignment
( assignment
, Syntax
, Grammar
, Language.Markdown.Assignment.Term
) where

import Prologue

import           Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import qualified CMarkGFM
import           Data.Syntax (makeTerm)
import qualified Data.Syntax as Syntax
import qualified Data.Term as Term
import qualified Data.Diff as Diff
import qualified Data.Text as Text
import qualified Language.Markdown.Syntax as Markup
import           Parsing.CMark as Grammar (Grammar (..))
import           Proto3.Suite (Named (..), Named1 (..))

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

type Term = Term.Term (Sum Syntax) Location
type Assignment = Assignment.Assignment (Term.TermF [] CMarkGFM.NodeType) Grammar

-- For Protobuf serialization
instance Named1 (Sum Syntax) where nameOf1 _ = "MarkdownSyntax"
instance Named (Term.Term (Sum Syntax) ()) where nameOf _ = "MarkdownTerm"
instance Named (Diff.Diff (Sum Syntax) () ()) where nameOf _ = "MarkdownDiff"

assignment :: Assignment Term
assignment = Syntax.handleError $ makeTerm <$> symbol Document <*> children (Markup.Document <$> many blockElement)


-- Block elements

blockElement :: Assignment Term
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

paragraph :: Assignment Term
paragraph = makeTerm <$> symbol Paragraph <*> children (Markup.Paragraph <$> many inlineElement)

list :: Assignment Term
list = Term.termIn <$> symbol List <*> (makeList . Term.termFAnnotation . Term.termFOut <$> currentNode <*> children (many item))
  where
    makeList (CMarkGFM.LIST CMarkGFM.ListAttributes{..}) = case listType of
      CMarkGFM.BULLET_LIST -> inject . Markup.UnorderedList
      CMarkGFM.ORDERED_LIST -> inject . Markup.OrderedList
    makeList _ = inject . Markup.UnorderedList

item :: Assignment Term
item = makeTerm <$> symbol Item <*> children (many blockElement)

heading :: Assignment Term
heading = makeTerm <$> symbol Heading <*> (makeHeading . Term.termFAnnotation . Term.termFOut <$> currentNode <*> children (many inlineElement) <*> manyTill blockElement (void (symbol Heading) <|> eof))
  where
    makeHeading (CMarkGFM.HEADING level) = Markup.Heading level
    makeHeading _ = Markup.Heading 0

blockQuote :: Assignment Term
blockQuote = makeTerm <$> symbol BlockQuote <*> children (Markup.BlockQuote <$> many blockElement)

codeBlock :: Assignment Term
codeBlock = makeTerm <$> symbol CodeBlock <*> (makeCode . Term.termFAnnotation . Term.termFOut <$> currentNode <*> source)
  where
    makeCode (CMarkGFM.CODE_BLOCK language _) = Markup.Code (nullText language)
    makeCode _ = Markup.Code Nothing

thematicBreak :: Assignment Term
thematicBreak = makeTerm <$> token ThematicBreak <*> pure Markup.ThematicBreak

htmlBlock :: Assignment Term
htmlBlock = makeTerm <$> symbol HTMLBlock <*> (Markup.HTMLBlock <$> source)

table :: Assignment Term
table = makeTerm <$> symbol Table <*> children (Markup.Table <$> many tableRow)

tableRow :: Assignment Term
tableRow = makeTerm <$> symbol TableRow <*> children (Markup.TableRow <$> many tableCell)

tableCell :: Assignment Term
tableCell = makeTerm <$> symbol TableCell <*> children (Markup.TableCell <$> many inlineElement)

-- Inline elements

inlineElement :: Assignment Term
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

strong :: Assignment Term
strong = makeTerm <$> symbol Strong <*> children (Markup.Strong <$> many inlineElement)

emphasis :: Assignment Term
emphasis = makeTerm <$> symbol Emphasis <*> children (Markup.Emphasis <$> many inlineElement)

strikethrough :: Assignment Term
strikethrough = makeTerm <$> symbol Strikethrough <*> children (Markup.Strikethrough <$> many inlineElement)

text :: Assignment Term
text = makeTerm <$> symbol Text <*> (Markup.Text <$> source)

htmlInline :: Assignment Term
htmlInline = makeTerm <$> symbol HTMLInline <*> (Markup.HTMLBlock <$> source)

link :: Assignment Term
link = makeTerm <$> symbol Link <*> (makeLink . Term.termFAnnotation . Term.termFOut <$> currentNode) <* advance
  where
    makeLink (CMarkGFM.LINK url title) = Markup.Link url (nullText title)
    makeLink _ = Markup.Link mempty Nothing

image :: Assignment Term
image = makeTerm <$> symbol Image <*> (makeImage . Term.termFAnnotation . Term.termFOut <$> currentNode) <* advance
  where
    makeImage (CMarkGFM.IMAGE url title) = Markup.Image url (nullText title)
    makeImage _ = Markup.Image mempty Nothing

code :: Assignment Term
code = makeTerm <$> symbol Code <*> (Markup.Code Nothing <$> source)

lineBreak :: Assignment Term
lineBreak = makeTerm <$> token LineBreak <*> pure Markup.LineBreak

softBreak :: Assignment Term
softBreak = makeTerm <$> token SoftBreak <*> pure Markup.LineBreak


-- Implementation details

nullText :: Text.Text -> Maybe Text.Text
nullText text = if Text.null text then Nothing else Just text
