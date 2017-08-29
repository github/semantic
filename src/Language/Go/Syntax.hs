{-# LANGUAGE DataKinds, DeriveAnyClass, RankNTypes, TypeOperators #-}
module Language.Go.Syntax
( assignment
, Syntax
, Grammar
, Term
) where

import Algorithm
import Data.Align.Generic
import Data.Functor (void)
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Record
import Data.Syntax (contextualize, emptyTerm, handleError, infixContext, makeTerm, makeTerm', makeTerm1, postContextualize)
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment hiding (Assignment, Error)
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import Data.Union
import GHC.Generics
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
   , Syntax.Error
   , Syntax.Empty
   , Syntax.Identifier
   , Syntax.Program
   , []
   ]

type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment [] Grammar Term

assignment :: Assignment
assignment = handleError $ makeTerm <$> symbol SourceFile <*> children (Syntax.Program <$> many expression)

expression :: Assignment
expression = handleError
           $ literal
          <|> importDeclaration
          <|> importSpec
          <|> packageClause
          <|> comment

expressions :: Assignment
expressions = makeTerm <$> location <*> many expression

literal :: Assignment
literal = identifier
       <|> interpretedStringLiteral
       <|> intLiteral

intLiteral :: Assignment
intLiteral = makeTerm <$> symbol IntLiteral <*> (Literal.Integer <$> source)

identifier :: Assignment
identifier = makeTerm <$> (symbol Identifier <|> symbol PackageIdentifier) <*> (Syntax.Identifier <$> source)

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
