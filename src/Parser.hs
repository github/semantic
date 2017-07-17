{-# LANGUAGE DataKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Parser
( Parser
, runParser
-- Syntax parsers
, parserForLanguage
-- À la carte parsers
, markdownParser
, pythonParser
, rubyParser
) where

import qualified CMark
import Data.Functor.Foldable hiding (fold, Nil)
import Data.Record
import Data.Source as Source
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment
import Data.Union
import Info hiding (Empty, Go)
import Language
import Language.Markdown
import qualified Language.Markdown.Syntax as Markdown
import qualified Language.Python.Syntax as Python
import qualified Language.Ruby.Syntax as Ruby
import Prologue hiding (Location)
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
  AssignmentParser :: (Enum grammar, Eq grammar, Show grammar, Symbol grammar, Syntax.Error :< fs, Foldable (Union fs), Functor (Union fs), Recursive ast, Foldable (Base ast))
                   => Parser ast                                                   -- ^ A parser producing AST.
                   -> (forall x. Base ast x -> Node grammar) -- ^ A function extracting the symbol and location.
                   -> Assignment ast grammar (Term (Union fs) (Record Location))   -- ^ An assignment from AST onto 'Term's.
                   -> Parser (Term (Union fs) (Record Location))                   -- ^ A parser producing 'Term's.
  -- | A tree-sitter parser.
  TreeSitterParser :: Language -> Ptr TS.Language -> Parser (SyntaxTerm Text DefaultFields)
  -- | A parser for 'Markdown' using cmark.
  MarkdownParser :: Parser (AST CMark.NodeType)
  -- | A parser which will parse any input 'Source' into a top-level 'Term' whose children are leaves consisting of the 'Source's lines.
  LineByLineParser :: Parser (SyntaxTerm Text DefaultFields)

-- | Return a 'Language'-specific 'Parser', if one exists, falling back to the 'LineByLineParser'.
parserForLanguage :: Maybe Language -> Parser (SyntaxTerm Text DefaultFields)
parserForLanguage Nothing = LineByLineParser
parserForLanguage (Just language) = case language of
  C -> TreeSitterParser C tree_sitter_c
  Go -> TreeSitterParser Go tree_sitter_go
  JavaScript -> TreeSitterParser TypeScript tree_sitter_typescript
  Ruby -> TreeSitterParser Ruby tree_sitter_ruby
  TypeScript -> TreeSitterParser TypeScript tree_sitter_typescript
  _ -> LineByLineParser

rubyParser :: Parser Ruby.Term
rubyParser = AssignmentParser (ASTParser tree_sitter_ruby) headF Ruby.assignment

pythonParser :: Parser Python.Term
pythonParser = AssignmentParser (ASTParser tree_sitter_python) headF Python.assignment

markdownParser :: Parser Markdown.Term
markdownParser = AssignmentParser MarkdownParser (\ (node@Node{..} :< _) -> node { nodeSymbol = toGrammar nodeSymbol }) Markdown.assignment

runParser :: Parser term -> Source -> IO term
runParser parser = case parser of
  ASTParser language -> parseToAST language
  AssignmentParser parser by assignment -> \ source -> do
    ast <- runParser parser source
    let Result err term = assignBy by assignment source ast
    traverse_ (printError source) err
    pure $! fromMaybe (errorTerm source) term
  TreeSitterParser language tslanguage -> treeSitterParser language tslanguage
  MarkdownParser -> pure . cmarkParser
  LineByLineParser -> pure . lineByLineParser

errorTerm :: Syntax.Error :< fs => Source -> Term (Union fs) (Record Location)
errorTerm source = cofree ((totalRange source :. totalSpan source :. Nil) :< inj (Syntax.Error []))

-- | A fallback parser that treats a file simply as rows of strings.
lineByLineParser :: Source -> SyntaxTerm Text DefaultFields
lineByLineParser source = cofree $ (totalRange source :. Program :. totalSpan source :. Nil) :< Indexed (zipWith toLine [1..] (sourceLineRanges source))
  where toLine line range = cofree $ (range :. Program :. Span (Pos line 1) (Pos line (end range)) :. Nil) :< Leaf (toText (slice range source))
