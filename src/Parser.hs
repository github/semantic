{-# LANGUAGE DataKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Parser
( Parser(..)
-- Syntax parsers
, parserForLanguage
-- À la carte parsers
, jsonParser
, markdownParser
, pythonParser
, rubyParser
, typescriptParser
) where

import qualified CMarkGFM
import Data.Functor.Classes (Eq1)
import Data.Ix
import Data.Record
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment
import Data.Term
import Data.Union
import Foreign.Ptr
import Info hiding (Empty, Go)
import Language
import qualified Language.JSON.Syntax as JSON
import qualified Language.Markdown.Syntax as Markdown
import qualified Language.Python.Syntax as Python
import qualified Language.Ruby.Syntax as Ruby
import qualified Language.TypeScript.Syntax as TypeScript
import Syntax hiding (Go)
import qualified TreeSitter.Language as TS (Language, Symbol)
import TreeSitter.Go
import TreeSitter.Python
import TreeSitter.Ruby
import TreeSitter.TypeScript
import TreeSitter.JSON

-- | A parser from 'Source' onto some term type.
data Parser term where
  -- | A parser producing 'AST' using a 'TS.Language'.
  ASTParser :: (Bounded grammar, Enum grammar) => Ptr TS.Language -> Parser (AST [] grammar)
  -- | A parser producing an à la carte term given an 'AST'-producing parser and an 'Assignment' onto 'Term's in some syntax type.
  AssignmentParser :: (Enum grammar, Ix grammar, Show grammar, TS.Symbol grammar, Syntax.Error :< fs, Eq1 ast, Apply Foldable fs, Apply Functor fs, Foldable ast, Functor ast)
                   => Parser (Term ast (Node grammar))                           -- ^ A parser producing AST.
                   -> Assignment ast grammar (Term (Union fs) (Record Location)) -- ^ An assignment from AST onto 'Term's.
                   -> Parser (Term (Union fs) (Record Location))                 -- ^ A parser producing 'Term's.
  -- | A tree-sitter parser.
  TreeSitterParser :: Ptr TS.Language -> Parser (Term Syntax (Record DefaultFields))
  -- | A parser for 'Markdown' using cmark.
  MarkdownParser :: Parser (Term (TermF [] CMarkGFM.NodeType) (Node Markdown.Grammar))

-- | Return a 'Language'-specific 'Parser', if one exists, falling back to the 'LineByLineParser'.
parserForLanguage :: Language -> Maybe (Parser (Term Syntax (Record DefaultFields)))
parserForLanguage language = case language of
  Go         -> Just (TreeSitterParser tree_sitter_go)
  JavaScript -> Just (TreeSitterParser tree_sitter_typescript)
  JSON       -> Just (TreeSitterParser tree_sitter_json)
  JSX        -> Just (TreeSitterParser tree_sitter_typescript)
  Ruby       -> Just (TreeSitterParser tree_sitter_ruby)
  TypeScript -> Just (TreeSitterParser tree_sitter_typescript)
  _ -> Nothing

rubyParser :: Parser Ruby.Term
rubyParser = AssignmentParser (ASTParser tree_sitter_ruby) Ruby.assignment

pythonParser :: Parser Python.Term
pythonParser = AssignmentParser (ASTParser tree_sitter_python) Python.assignment

jsonParser :: Parser JSON.Term
jsonParser = AssignmentParser (ASTParser tree_sitter_json) JSON.assignment

typescriptParser :: Parser TypeScript.Term
typescriptParser = AssignmentParser (ASTParser tree_sitter_typescript) TypeScript.assignment

markdownParser :: Parser Markdown.Term
markdownParser = AssignmentParser MarkdownParser Markdown.assignment
