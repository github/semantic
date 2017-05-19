{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Parser where

import Data.Record
import qualified Data.Text as T
import Info hiding (Go)
import Language
import Prologue
import Source
import Syntax hiding (Go)
import Term
import Text.Parser.TreeSitter.C
import Text.Parser.TreeSitter.Go
import Text.Parser.TreeSitter.Ruby
import Text.Parser.TreeSitter.TypeScript
import TreeSitter

data Parser term where
  CParser :: Parser (SyntaxTerm Text DefaultFields)
  GoParser :: Parser (SyntaxTerm Text DefaultFields)
  RubyParser :: Parser (SyntaxTerm Text DefaultFields)
  TypeScriptParser :: Parser (SyntaxTerm Text DefaultFields)
  LineByLineParser :: Parser (SyntaxTerm Text DefaultFields)

runParser :: Parser term -> SourceBlob -> IO term
runParser parser = case parser of
  CParser -> treeSitterParser C tree_sitter_c
  GoParser -> treeSitterParser Go tree_sitter_go
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
