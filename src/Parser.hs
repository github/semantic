{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeOperators #-}
module Parser where

import Data.Functor.Union
import Data.Record
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment
import Data.Functor.Union (inj)
import qualified Data.Text as T
import Info hiding (Empty, Go)
import Language
import Language.Markdown
import qualified Language.Python.Syntax as Python
import qualified Language.Ruby.Syntax as Ruby
import Prologue hiding (Location)
import Source
import Syntax hiding (Go)
import Term
import qualified Text.Parser.TreeSitter as TS
import Text.Parser.TreeSitter.Language (Symbol)
import Text.Parser.TreeSitter.C
import Text.Parser.TreeSitter.Go
import Text.Parser.TreeSitter.Python
import Text.Parser.TreeSitter.Ruby
import Text.Parser.TreeSitter.TypeScript
import TreeSitter

-- | A parser from 'Source' onto some term type.
data Parser term where
  -- | A parser producing 'AST' using a 'TS.Language'.
  ASTParser :: (Bounded grammar, Enum grammar) => Ptr TS.Language -> Parser (AST grammar)
  -- | A parser producing an à la carte term given an 'AST'-producing parser and an 'Assignment' onto 'Term's in some syntax type. Assignment errors will result in a top-level 'Syntax.Error' node.
  AssignmentParser :: (Bounded grammar, Enum grammar, Eq grammar, Show grammar, Symbol grammar, Functor (Union fs))
                   => Parser (AST grammar)                                                -- ^ A parser producing 'AST'.
                   -> Assignment (Node grammar) (Term (Union fs) Location)                -- ^ An assignment from 'AST' onto 'Term's.
                   -> Parser (Term (Union (Syntax.Error [Error grammar] ': fs)) Location) -- ^ A parser of 'Term's, weakened to allow for 'Syntax.Error' cases.
  -- | A tree-sitter parser.
  TreeSitterParser :: Language -> Ptr TS.Language -> Parser (SyntaxTerm Text DefaultFields)
  -- | A parser for 'Markdown' using cmark.
  MarkdownParser :: Parser (SyntaxTerm Text DefaultFields)
  -- | A parser which will parse any input 'Source' into a top-level 'Term' whose children are leaves consisting of the 'Source's lines.
  LineByLineParser :: Parser (SyntaxTerm Text DefaultFields)

-- | Return a 'Langauge'-specific 'Parser', if one exists, falling back to the 'LineByLineParser'.
parserForLanguage :: Maybe Language -> Parser (SyntaxTerm Text DefaultFields)
parserForLanguage Nothing = LineByLineParser
parserForLanguage (Just language) = case language of
  C -> TreeSitterParser C tree_sitter_c
  Go -> TreeSitterParser Go tree_sitter_go
  Markdown -> MarkdownParser
  Ruby -> TreeSitterParser Ruby tree_sitter_ruby
  TypeScript -> TreeSitterParser TypeScript tree_sitter_typescript
  _ -> LineByLineParser

rubyParser :: Parser (Term (Union (Syntax.Error [Error Ruby.Grammar] ': Ruby.Syntax')) Location)
rubyParser = AssignmentParser (ASTParser tree_sitter_ruby) Ruby.assignment

pythonParser :: Parser (Term (Union (Syntax.Error [Error Python.Grammar] ': Python.Syntax')) Location)
pythonParser = AssignmentParser (ASTParser tree_sitter_python) Python.assignment

runParser :: Parser term -> Source -> IO term
runParser parser = case parser of
  ASTParser language -> parseToAST language
  AssignmentParser parser assignment -> \ source -> do
    ast <- runParser parser source
    let Result errors term = assign assignment source ast
    traverse_ (putStr . ($ "") . showError source) errors
    pure (maybe (cofree ((totalRange source :. totalSpan source :. Nil) :< inj (Syntax.Error errors))) (hoistCofree weaken) term)
  TreeSitterParser language tslanguage -> treeSitterParser language tslanguage
  MarkdownParser -> cmarkParser
  LineByLineParser -> lineByLineParser

-- | A fallback parser that treats a file simply as rows of strings.
lineByLineParser :: Source -> IO (SyntaxTerm Text DefaultFields)
lineByLineParser source = pure . cofree . root $ case foldl' annotateLeaves ([], 0) lines of
  (leaves, _) -> cofree <$> leaves
  where
    lines = actualLines source
    root children = (sourceRange :. Program :. rangeToSourceSpan source sourceRange :. Nil) :< Indexed children
    sourceRange = Source.totalRange source
    leaf byteIndex line = (Range byteIndex (byteIndex + T.length line) :. Program :. rangeToSourceSpan source (Range byteIndex (byteIndex + T.length line)) :. Nil) :< Leaf line
    annotateLeaves (accum, byteIndex) line =
      (accum <> [ leaf byteIndex (Source.toText line) ] , byteIndex + Source.length line)
