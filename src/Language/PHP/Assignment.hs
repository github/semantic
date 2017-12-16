{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.PHP.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Data.Union
import Data.Record
import qualified Data.Term as Term
import Data.Syntax (emptyTerm, handleError, parseError, infixContext, makeTerm, makeTerm', makeTerm1, contextualize, postContextualize)
import qualified Data.Syntax as Syntax
import qualified Language.PHP.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import Language.PHP.Grammar as Grammar
import Data.List.NonEmpty (some1)

type Syntax = '[
    Literal.TextElement
  , Comment.Comment
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Context
  , Syntax.Program
  , Syntax.Text
  , Statement.Assignment
  , Declaration.VariableDeclaration
  , Syntax.Identifier
  , Syntax.VariableName
  , Syntax.RequireOnce
  , Syntax.Require
  , [] ]

type Term = Term.Term (Data.Union.Union Syntax) (Record Location)
type Assignment = Assignment.Assignment [] Grammar Term

-- | Assignment from AST in TypeScript’s grammar onto a program in TypeScript’s syntax.
assignment :: Assignment
assignment = handleError $ makeTerm <$> symbol Program <*> children (Syntax.Program <$> ((\a b c -> a : b ++ [c]) <$> (text <|> emptyTerm) <*> manyTerm statement <*> (text <|> emptyTerm))) <|> parseError

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
manyTerm term = many (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))


text :: Assignment
text = makeTerm <$> symbol Text <*> (Syntax.Text <$> source)

statement :: Assignment
statement = handleError everything
  where
    everything = choice [
  --   compoundStatement
  -- , namedLabelStatement
  -- , expressionStatement
  -- , selectionStatement
  -- , jumpStatement
  -- , tryStatement
  -- , declareStatement
  -- , echoStatement
  -- , constDeclaration
  -- , functionDefinition
  -- , classDeclaration
  -- , interfaceDeclaration
  -- , traitDeclaration
  -- , namespaceDefinition
  -- , namespaceUseDeclaration
  -- , globalDeclaration
      functionStaticDeclaration
      ]

expression :: Assignment
expression = choice [
  -- assignmentExpression,
  -- yieldExpression,
  -- unaryExpression,
  -- binaryExpression,
  -- includeExpression,
  -- includeOnceExpression,
  requireExpression,
  requireOnceExpression
  ]

requireExpression :: Assignment
requireExpression = makeTerm <$> symbol RequireExpression <*> children (Syntax.Require <$> expression)


requireOnceExpression :: Assignment
requireOnceExpression = makeTerm <$> symbol RequireOnceExpression <*> children (Syntax.RequireOnce <$> expression)

variableName :: Assignment
variableName = makeTerm <$> symbol VariableName <*> children (Syntax.VariableName <$> name)

name :: Assignment
name = makeTerm <$> symbol Name <*> (Syntax.Identifier <$> source)

functionStaticDeclaration :: Assignment
functionStaticDeclaration = makeTerm <$> symbol FunctionStaticDeclaration <*> children (Declaration.VariableDeclaration . pure <$> staticVariableDeclaration)

staticVariableDeclaration :: Assignment
staticVariableDeclaration = makeTerm <$> symbol StaticVariableDeclaration <*> children (Statement.Assignment <$> pure [] <*> variableName <*> (expression <|> emptyTerm))

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

string :: Assignment
string = makeTerm <$> symbol Grammar.String <*> (Literal.TextElement <$> source)
