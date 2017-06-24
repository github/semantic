{-# LANGUAGE DataKinds, TypeOperators #-}
module Language.Markdown
( Grammar(..)
, cmarkParser
, toGrammar
) where

import CMark
import Data.Record
import Data.Source
import Data.Syntax.Assignment (Location)
import Info
import Prologue hiding (Location)
import Text.Parser.TreeSitter.Language (Symbol(..), SymbolType(..))

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
  deriving (Bounded, Enum, Eq, Ord, Show)

cmarkParser :: Source -> Cofree [] (Record (NodeType ': Location))
cmarkParser source = toTerm (totalRange source) (totalSpan source) $ commonmarkToNode [ optSourcePos, optSafe ] (toText source)
  where toTerm :: Range -> Span -> Node -> Cofree [] (Record (NodeType ': Location))
        toTerm within withinSpan (Node position t children) =
          let range = maybe within (spanToRangeInLineRanges lineRanges . toSpan) position
              span = maybe withinSpan toSpan position
          in cofree $ (t :. range :. span :. Nil) :< (toTerm range span <$> children)

        toSpan PosInfo{..} = Span (Pos startLine startColumn) (Pos endLine (succ endColumn))

        lineRanges = actualLineRanges source

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


instance Symbol Grammar where
  symbolType _ = Regular
