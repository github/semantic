{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Parser where

import Data.Record
import Data.Syntax.Assignment (Location)
import qualified Data.Text as T
import Info hiding (Go)
import Language
import Language.Markdown
import qualified Language.Ruby.Syntax as Ruby
import Prologue hiding (Location)
import Source
import Syntax hiding (Go)
import Term
import Text.Parser.TreeSitter.C
import Text.Parser.TreeSitter.Go
import Text.Parser.TreeSitter.Ruby
import Text.Parser.TreeSitter.TypeScript
import TreeSitter

data Parser term where
  ALaCarteRubyParser :: Parser (Maybe (Term Ruby.Syntax Location))
  CParser :: Parser (SyntaxTerm Text DefaultFields)
  GoParser :: Parser (SyntaxTerm Text DefaultFields)
  MarkdownParser :: Parser (SyntaxTerm Text DefaultFields)
  RubyParser :: Parser (SyntaxTerm Text DefaultFields)
  TypeScriptParser :: Parser (SyntaxTerm Text DefaultFields)
  LineByLineParser :: Parser (SyntaxTerm Text DefaultFields)

-- | Return a parser for a given langauge or the lineByLineParser parser.
parserForLanguage :: Maybe Language -> Parser (SyntaxTerm Text DefaultFields)
parserForLanguage Nothing = LineByLineParser
parserForLanguage (Just language) = case language of
  C -> CParser
  TypeScript -> TypeScriptParser
  Markdown -> MarkdownParser
  Ruby -> RubyParser
  Language.Go -> GoParser

runParser :: Parser term -> SourceBlob -> IO term
runParser parser = case parser of
  ALaCarteRubyParser -> parseRubyToTerm . source
  CParser -> treeSitterParser C tree_sitter_c
  GoParser -> treeSitterParser Go tree_sitter_go
  MarkdownParser -> cmarkParser
  RubyParser -> treeSitterParser Ruby tree_sitter_ruby
  TypeScriptParser -> treeSitterParser TypeScript tree_sitter_typescript
  LineByLineParser -> lineByLineParser

-- | A fallback parser that treats a file simply as rows of strings.
lineByLineParser :: SourceBlob -> IO (SyntaxTerm Text DefaultFields)
lineByLineParser SourceBlob{..} = pure . cofree . root $ case foldl' annotateLeaves ([], 0) lines of
  (leaves, _) -> cofree <$> leaves
  where
    lines = actualLines source
    root children = (sourceRange :. Program :. rangeToSourceSpan source sourceRange :. Nil) :< Indexed children
    sourceRange = Source.totalRange source
    leaf charIndex line = (Range charIndex (charIndex + T.length line) :. Program :. rangeToSourceSpan source (Range charIndex (charIndex + T.length line)) :. Nil) :< Leaf line
    annotateLeaves (accum, charIndex) line =
      (accum <> [ leaf charIndex (Source.toText line) ] , charIndex + Source.length line)
