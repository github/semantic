{-# LANGUAGE DataKinds #-}
module Language.Markdown where

import CMark
import Data.Record
import Data.Text
import Info
import Prologue
import Source
import Syntax
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

cmarkParser :: Source -> IO (Cofree (Syntax Text) (Record DefaultFields))
cmarkParser source = pure . toTerm (totalRange source) (rangeToSourceSpan source $ totalRange source) $ commonmarkToNode [ optSourcePos, optSafe ] (toText source)
  where toTerm :: Range -> SourceSpan -> Node -> Cofree (Syntax Text) (Record DefaultFields)
        toTerm within withinSpan (Node position t children) =
          let
            range = maybe within (sourceSpanToRange source . toSpan) position
            span = maybe withinSpan toSpan position
          in
            cofree $ (range :. toCategory t :. span :. Nil) :< case t of
          -- Leaves
          CODE text -> Leaf text
          TEXT text -> Leaf text
          CODE_BLOCK _ text -> Leaf text
          -- Branches
          _ -> Indexed (toTerm range span <$> children)

        toCategory :: NodeType -> Category
        toCategory (TEXT _) = Other "text"
        toCategory (CODE _) = Other "code"
        toCategory (HTML_BLOCK _) = Other "html"
        toCategory (HTML_INLINE _) = Other "html"
        toCategory (HEADING _) = Other "heading"
        toCategory (LIST ListAttributes{..}) = Other $ case listType of
          BULLET_LIST -> "unordered list"
          ORDERED_LIST -> "ordered list"
        toCategory LINK{} = Other "link"
        toCategory IMAGE{} = Other "image"
        toCategory t = Other (show t)
        toSpan PosInfo{..} = SourceSpan (SourcePos (pred startLine) (pred startColumn)) (SourcePos (pred endLine) endColumn)

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
