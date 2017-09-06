{-# LANGUAGE DataKinds, DeriveAnyClass, RankNTypes, TypeOperators #-}
module Language.Go.Syntax
( assignment
, Syntax
, Grammar
, Term
) where

import Data.Functor (void)
import Data.Record
import Data.Syntax (emptyTerm, handleError, makeTerm)
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment hiding (Assignment, Error)
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import Data.Union
import GHC.Stack
import Language.Go.Grammar as Grammar
import qualified Term

type Syntax =
  '[ Comment.Comment
   , Declaration.Function
   , Declaration.Import
   , Declaration.Method
   , Declaration.Module
   , Literal.Integer
   , Literal.TextElement
   , Statement.Assignment
   , Syntax.Error
   , Syntax.Empty
   , Syntax.Identifier
   , Syntax.Program
   , Type.Annotation
   , []
   ]

type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment [] Grammar Term

assignment :: Assignment
assignment = handleError $ makeTerm <$> symbol SourceFile <*> children (Syntax.Program <$> many expression)

expression :: Assignment
expression = handleError
           $  comment
          <|> constVarDeclaration
          <|> constVarSpecification
          <|> expressionList
          <|> functionDeclaration
          <|> importDeclaration
          <|> importSpec
          <|> typedIdentifier
          <|> literal
          <|> packageClause
          <|> parameterDeclaration

identifiers :: Assignment
identifiers = makeTerm <$> location <*> many identifier

expressions :: Assignment
expressions = makeTerm <$> location <*> many expression

literal :: Assignment
literal = identifier
       <|> interpretedStringLiteral
       <|> intLiteral
       <|> typing

intLiteral :: Assignment
intLiteral = makeTerm <$> symbol IntLiteral <*> (Literal.Integer <$> source)

typedIdentifier :: Assignment
typedIdentifier = mkTypedIdentifier <$> symbol Identifier <*> source <*> symbol TypeIdentifier <*> source
  where
    mkTypedIdentifier loc' identifier' loc'' identifier'' = makeTerm loc' (Type.Annotation (makeTerm loc' (Syntax.Identifier identifier')) (makeTerm loc'' (Syntax.Identifier identifier'')))

identifier :: Assignment
identifier =
      mk Identifier
  <|> mk PackageIdentifier
  <|> mk TypeIdentifier
  <|> mk ParenthesizedType
  where mk s = makeTerm <$> symbol s <*> (Syntax.Identifier <$> source)

typing :: Assignment
typing =
      mk TypeIdentifier
  <|> mk ParenthesizedType
  where mk s = makeTerm <$> symbol s <*> (Syntax.Identifier <$> source)

interpretedStringLiteral :: Assignment
interpretedStringLiteral = makeTerm <$> symbol InterpretedStringLiteral <*> (Literal.TextElement <$> source)

packageClause :: Assignment
packageClause = makeTerm <$> symbol PackageClause <*> children (Declaration.Module <$> identifier <*> pure [])

importDeclaration :: Assignment
importDeclaration = makeTerm <$> symbol ImportDeclaration <*> children (Declaration.Import <$> many expression)

importSpec :: Assignment
importSpec = symbol ImportSpec *> children expressions

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

constVarDeclaration :: Assignment
constVarDeclaration = (symbol ConstDeclaration <|> symbol VarDeclaration) *> children expressions

constVarSpecification :: Assignment
constVarSpecification = makeTerm <$> (symbol ConstSpec <|> symbol VarSpec) <*> children (Statement.Assignment
                                                                           <$> (annotatedLHS <|> identifiers)
                                                                           <*> expressions)
    where
      annotatedLHS = makeTerm <$> location <*> (Type.Annotation
                                              <$> (makeTerm <$> location <*> (manyTermsTill identifier (void (symbol TypeIdentifier))))
                                              <*> typing)

expressionList :: Assignment
expressionList = symbol ExpressionList *> children expressions

parameterDeclaration :: Assignment
parameterDeclaration = symbol ParameterDeclaration *> children expressions

functionDeclaration :: Assignment
functionDeclaration = mkTypedFunctionDeclaration <$> symbol FunctionDeclaration <*> children ((,,,) <$> identifier <*> parameters <*> types <*> block)
  where parameters = symbol Parameters *> children (many expression)
        block = symbol Block *> children expressions
        types = symbol Parameters *> children expressions <|> identifier <|> emptyTerm
        mkTypedFunctionDeclaration loc (name', params', types', block') = makeTerm loc (Type.Annotation (makeTerm loc (Declaration.Function name' params' block')) types')

-- | Match a series of terms or comments until a delimiter is matched
manyTermsTill :: Show b => Assignment.Assignment [] Grammar Term -> Assignment.Assignment [] Grammar b -> Assignment.Assignment [] Grammar [Term]
manyTermsTill step end = manyTill (step <|> comment) end
