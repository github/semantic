{-# LANGUAGE DataKinds, TypeOperators #-}
module Parsing.CMark
( Grammar(..)
, cmarkParser
, toGrammar
) where

import CMarkGFM
import qualified Data.AST as A
import Data.Ix
import Data.Range
import Data.Location
import Data.Span
import Data.Source
import Data.Term
import TreeSitter.Language (Symbol(..), SymbolType(..))

data Grammar
  = Document
  | ThematicBreak
  | Paragraph
  | BlockQuote
  | HTMLBlock
  | CustomBlock
  | CodeBlock
  | Heading
  | List
  | Item
  | Text
  | SoftBreak
  | LineBreak
  | HTMLInline
  | CustomInline
  | Code
  | Emphasis
  | Strong
  | Link
  | Image
  | Strikethrough
  | Table
  | TableRow
  | TableCell
  deriving (Bounded, Enum, Eq, Ix, Ord, Show)

exts :: [CMarkExtension]
exts = [
    extStrikethrough
  , extTable
  , extAutolink
  , extTagfilter
  ]

cmarkParser :: Source -> A.AST (TermF [] NodeType) Grammar
cmarkParser source = toTerm (totalRange source) (totalSpan source) $ commonmarkToNode [ optSourcePos, optSafe ] exts (toText source)
  where toTerm :: Range -> Span -> Node -> A.AST (TermF [] NodeType) Grammar
        toTerm within withinSpan (Node position t children) =
          let range = maybe within (spanToRangeInLineRanges lineRanges . toSpan) position
              span = maybe withinSpan toSpan position
          in termIn (A.Node (toGrammar t) (Location range span)) (In t (toTerm range span <$> children))

        toSpan PosInfo{..} = Span (Pos startLine startColumn) (Pos (max startLine endLine) (succ (if endLine <= startLine then max startColumn endColumn else endColumn)))

        lineRanges = sourceLineRangesByLineNumber source

toGrammar :: NodeType -> Grammar
toGrammar DOCUMENT{} = Document
toGrammar THEMATIC_BREAK{} = ThematicBreak
toGrammar PARAGRAPH{} = Paragraph
toGrammar BLOCK_QUOTE{} = BlockQuote
toGrammar HTML_BLOCK{} = HTMLBlock
toGrammar CUSTOM_BLOCK{} = CustomBlock
toGrammar CODE_BLOCK{} = CodeBlock
toGrammar HEADING{} = Heading
toGrammar LIST{} = List
toGrammar ITEM{} = Item
toGrammar TEXT{} = Text
toGrammar SOFTBREAK{} = SoftBreak
toGrammar LINEBREAK{} = LineBreak
toGrammar HTML_INLINE{} = HTMLInline
toGrammar CUSTOM_INLINE{} = CustomInline
toGrammar CODE{} = Code
toGrammar EMPH{} = Emphasis
toGrammar STRONG{} = Strong
toGrammar LINK{} = Link
toGrammar IMAGE{} = Image
toGrammar STRIKETHROUGH{} = Strikethrough
toGrammar TABLE{} = Table
toGrammar TABLE_ROW{} = TableRow
toGrammar TABLE_CELL{} = TableCell


instance Symbol Grammar where
  symbolType _ = Regular
