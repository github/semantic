{-# LANGUAGE DataKinds, TemplateHaskell #-}
module Language.Python.Syntax where

import Data.Functor.Union
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import Language.Haskell.TH hiding (location)
import Prologue hiding (Location)
import Term
import Text.Parser.TreeSitter.Python
import Text.Parser.TreeSitter.Language

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_python

type Syntax = Union Syntax'
type Syntax' =
  '[ Comment.Comment
   , Literal.Boolean
   , Literal.String
   , Statement.If
   , Statement.Import
   , Syntax.Empty
   ]

-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment (Node Grammar) [Term Syntax Location]
assignment = symbol Module *> children (many (comment
                                            <|> statement
                                            <|> literal))


statement :: Assignment (Node Grammar) (Term Syntax Location)
statement = expressionStatement
          <|> ifStatement
          <|> importStatement
          <|> importFromStatement

literal :: Assignment (Node Grammar) (Term Syntax Location)
literal = string

string :: Assignment (Node Grammar) (Term Syntax Location)
string = makeTerm <$ symbol String <*> location <*> (Syntax.Empty <$ source)

comment :: Assignment (Node Grammar) (Term Syntax Location)
comment = makeTerm <$ symbol Comment <*> location <*> (Comment.Comment <$> source)


expressionStatement :: Assignment (Node Grammar) (Term Syntax Location)
expressionStatement = symbol ExpressionStatement *> children (statement <|> literal)


-- TODO Possibly match against children for dotted name and identifiers
importStatement :: Assignment (Node Grammar) (Term Syntax Location)
importStatement = makeTerm <$ symbol ImportStatement <*> location <*> (Statement.Import <$> source)

-- TODO Possibly match against children nodes
importFromStatement :: Assignment (Node Grammar) (Term Syntax Location)
importFromStatement = makeTerm <$ symbol ImportFromStatement <*> location <*> (Statement.Import <$> source)


ifStatement :: Assignment (Node Grammar) (Term Syntax Location)
ifStatement = makeTerm <$ symbol IfStatement <*> location <*> children (Statement.If <$> condition <*> statement <*> (flip (foldr makeElif) <$> many elifClause <*> optionalElse))
  where elseClause = symbol ElseClause *> children statement
        elifClause = (,) <$ symbol ElifClause <*> location <*> children (Statement.If <$> condition <*> statement)
        condition = boolean
        optionalElse = fromMaybe <$> (makeTerm <$> location <*> pure Syntax.Empty) <*> optional elseClause
        makeElif (loc, makeIf) rest = makeTerm loc (makeIf rest)


boolean :: Assignment (Node Grammar) (Term Syntax Location)
boolean =  makeTerm <$ symbol Language.Python.Syntax.True <*> location <*> (Literal.true <$ source)
       <|> makeTerm <$ symbol Language.Python.Syntax.False <*> location <*> (Literal.false <$ source)


makeTerm :: InUnion Syntax' f => a -> f (Term Syntax a) -> Term Syntax a
makeTerm a f = cofree (a :< inj f)
