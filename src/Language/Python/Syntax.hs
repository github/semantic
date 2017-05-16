{-# LANGUAGE DataKinds, TemplateHaskell #-}
module Language.Python.Syntax where

import Data.Functor.Union
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import GHC.Stack
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
   , Literal.Float
   , Literal.Integer
   , Literal.None
   , Literal.String
   , Literal.TextElement
   , Statement.If
   , Statement.Import
   , Statement.Return
   , Syntax.Empty
   , Syntax.Identifier
   ]

-- | Assignment from AST in Python's grammar onto a program in Python's syntax.
assignment :: HasCallStack => Assignment (Node Grammar) [Term Syntax Location]
assignment = symbol Module *> children (many declaration)


declaration :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
declaration = comment <|> literal <|> statement <|> importStatement <|> importFromStatement


statement :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
statement = expressionStatement
          <|> ifStatement
          <|> returnStatement
          <|> identifier


identifier :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
identifier = makeTerm <$> symbol Identifier <*> (Syntax.Identifier <$> source)

literal :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
literal = string <|> integer <|> float <|> boolean <|> none

-- TODO: Wrap `Literal.TextElement` with a `Litera.String`
string :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
string = makeTerm <$> symbol String <*> (Literal.TextElement <$> source)

float :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
float = makeTerm <$> symbol Float <*> (Literal.Float <$> source)

integer :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

comment :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

expressionStatement :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
expressionStatement = symbol ExpressionStatement *> children (statement <|> literal)


-- TODO Possibly match against children for dotted name and identifiers
importStatement :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
importStatement = makeTerm <$> symbol ImportStatement <*> (Statement.Import <$> source)

-- TODO Possibly match against children nodes
importFromStatement :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
importFromStatement = makeTerm <$> symbol ImportFromStatement <*> (Statement.Import <$> source)

returnStatement :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> (symbol ExpressionList *> children (statement <|> literal)))


ifStatement :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> condition <*> statement <*> (flip (foldr makeElif) <$> many elifClause <*> optionalElse))
  where elseClause = symbol ElseClause *> children statement
        elifClause = (,) <$ symbol ElifClause <*> location <*> children (Statement.If <$> condition <*> statement)
        condition = boolean
        optionalElse = fromMaybe <$> (makeTerm <$> location <*> pure Syntax.Empty) <*> optional elseClause
        makeElif (loc, makeIf) rest = makeTerm loc (makeIf rest)


boolean :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
boolean =  makeTerm <$> symbol Language.Python.Syntax.True  <*> (Literal.true <$ source)
       <|> makeTerm <$> symbol Language.Python.Syntax.False <*> (Literal.false <$ source)

none :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
none = makeTerm <$> symbol None <*> (Literal.None <$> source)

makeTerm :: HasCallStack => InUnion Syntax' f => a -> f (Term Syntax a) -> Term Syntax a
makeTerm a f = cofree (a :< inj f)
