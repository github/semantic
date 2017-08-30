{-# LANGUAGE DataKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Parser
( Parser(..)
, goParser
, jsonParser
, markdownParser
, pythonParser
, rubyParser
, typescriptParser
) where

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Trans.Cofree (CofreeF)
import qualified CMarkGFM
import Data.Ix
import Data.Record
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment
import Data.Union
import Foreign.Ptr
import qualified Language.Go.Syntax as Go
import qualified Language.JSON.Syntax as JSON
import qualified Language.Markdown.Syntax as Markdown
import qualified Language.Python.Syntax as Python
import qualified Language.Ruby.Syntax as Ruby
import qualified Language.TypeScript.Syntax as TypeScript
import Term
import qualified TreeSitter.Language as TS (Language, Symbol)
import TreeSitter.Go
import TreeSitter.JSON
import TreeSitter.Python
import TreeSitter.Ruby
import TreeSitter.TypeScript

-- | A parser from 'Source' onto some term type.
data Parser term where
  -- | A parser producing 'AST' using a 'TS.Language'.
  ASTParser :: (Bounded grammar, Enum grammar) => Ptr TS.Language -> Parser (AST [] grammar)
  -- | A parser producing an à la carte term given an 'AST'-producing parser and an 'Assignment' onto 'Term's in some syntax type.
  AssignmentParser :: (Bounded grammar, Ix grammar, Show grammar, TS.Symbol grammar, Syntax.Error :< fs, Eq (ast (Cofree ast (Node grammar))), Apply1 Foldable fs, Apply1 Functor fs, Foldable ast, Functor ast)
                   => Parser (Cofree ast (Node grammar))                         -- ^ A parser producing AST.
                   -> Assignment ast grammar (Term (Union fs) (Record Location)) -- ^ An assignment from AST onto 'Term's.
                   -> Parser (Term (Union fs) (Record Location))                 -- ^ A parser producing 'Term's.
  -- | A parser for 'Markdown' using cmark.
  MarkdownParser :: Parser (Cofree (CofreeF [] CMarkGFM.NodeType) (Node Markdown.Grammar))

goParser :: Parser Go.Term
goParser = AssignmentParser (ASTParser tree_sitter_go) Go.assignment

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
