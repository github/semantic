{-# LANGUAGE DataKinds, FlexibleContexts, RankNTypes, RecordWildCards, TypeFamilies, TypeOperators #-}
module Language.Markdown.Assignment
( assignment
, Markdown.Syntax
, Grammar
, Markdown.Term(..)
) where

import Prologue

import           Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import qualified CMarkGFM
import           Data.Syntax (makeTerm)
import qualified Data.Syntax as Syntax
import qualified Data.Term as Term
import qualified Data.Text as Text
import qualified Language.Markdown.Syntax as Markup
import           Language.Markdown.Term as Markdown
import           Parsing.CMark as Grammar (Grammar (..))

type Assignment = Assignment.Assignment (Term.TermF [] CMarkGFM.NodeType) Grammar

assignment :: Assignment (Term Loc)
assignment = Syntax.handleError $ makeTerm <$> symbol Document <*> children (Markup.Document <$> many blockElement)


-- Block elements

blockElement :: Assignment (Term Loc)
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

paragraph :: Assignment (Term Loc)
paragraph = makeTerm <$> symbol Paragraph <*> children (Markup.Paragraph <$> many inlineElement)

list :: Assignment (Term Loc)
list = Term.termIn <$> symbol List <*> (makeList . Term.termFAnnotation . Term.termFOut <$> currentNode <*> children (many item))
  where
    makeList (CMarkGFM.LIST CMarkGFM.ListAttributes{..}) = case listType of
      CMarkGFM.BULLET_LIST -> inject . Markup.UnorderedList
      CMarkGFM.ORDERED_LIST -> inject . Markup.OrderedList
    makeList _ = inject . Markup.UnorderedList

item :: Assignment (Term Loc)
item = makeTerm <$> symbol Item <*> children (many blockElement)

heading :: Assignment (Term Loc)
heading = makeTerm <$> symbol Heading <*> (makeHeading . Term.termFAnnotation . Term.termFOut <$> currentNode <*> children (many inlineElement) <*> manyTill blockElement (void (symbol Heading) <|> eof))
  where
    makeHeading (CMarkGFM.HEADING level) = Markup.Heading level
    makeHeading _ = Markup.Heading 0

blockQuote :: Assignment (Term Loc)
blockQuote = makeTerm <$> symbol BlockQuote <*> children (Markup.BlockQuote <$> many blockElement)

codeBlock :: Assignment (Term Loc)
codeBlock = makeTerm <$> symbol CodeBlock <*> (makeCode . Term.termFAnnotation . Term.termFOut <$> currentNode <*> source)
  where
    makeCode (CMarkGFM.CODE_BLOCK language _) = Markup.Code (nullText language)
    makeCode _ = Markup.Code Nothing

thematicBreak :: Assignment (Term Loc)
thematicBreak = makeTerm <$> token ThematicBreak <*> pure Markup.ThematicBreak

htmlBlock :: Assignment (Term Loc)
htmlBlock = makeTerm <$> symbol HTMLBlock <*> (Markup.HTMLBlock <$> source)

table :: Assignment (Term Loc)
table = makeTerm <$> symbol Table <*> children (Markup.Table <$> many tableRow)

tableRow :: Assignment (Term Loc)
tableRow = makeTerm <$> symbol TableRow <*> children (Markup.TableRow <$> many tableCell)

tableCell :: Assignment (Term Loc)
tableCell = makeTerm <$> symbol TableCell <*> children (Markup.TableCell <$> many inlineElement)

-- Inline elements

inlineElement :: Assignment (Term Loc)
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

strong :: Assignment (Term Loc)
strong = makeTerm <$> symbol Strong <*> children (Markup.Strong <$> many inlineElement)

emphasis :: Assignment (Term Loc)
emphasis = makeTerm <$> symbol Emphasis <*> children (Markup.Emphasis <$> many inlineElement)

strikethrough :: Assignment (Term Loc)
strikethrough = makeTerm <$> symbol Strikethrough <*> children (Markup.Strikethrough <$> many inlineElement)

text :: Assignment (Term Loc)
text = makeTerm <$> symbol Text <*> (Markup.Text <$> source)

htmlInline :: Assignment (Term Loc)
htmlInline = makeTerm <$> symbol HTMLInline <*> (Markup.HTMLBlock <$> source)

link :: Assignment (Term Loc)
link = makeTerm <$> symbol Link <*> (makeLink . Term.termFAnnotation . Term.termFOut <$> currentNode) <* advance
  where
    makeLink (CMarkGFM.LINK url title) = Markup.Link url (nullText title)
    makeLink _ = Markup.Link mempty Nothing

image :: Assignment (Term Loc)
image = makeTerm <$> symbol Image <*> (makeImage . Term.termFAnnotation . Term.termFOut <$> currentNode) <* advance
  where
    makeImage (CMarkGFM.IMAGE url title) = Markup.Image url (nullText title)
    makeImage _ = Markup.Image mempty Nothing

code :: Assignment (Term Loc)
code = makeTerm <$> symbol Code <*> (Markup.Code Nothing <$> source)

lineBreak :: Assignment (Term Loc)
lineBreak = makeTerm <$> token LineBreak <*> pure Markup.LineBreak

softBreak :: Assignment (Term Loc)
softBreak = makeTerm <$> token SoftBreak <*> pure Markup.LineBreak


-- Implementation details

nullText :: Text.Text -> Maybe Text.Text
nullText text = if Text.null text then Nothing else Just text
