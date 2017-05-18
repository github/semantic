{-# LANGUAGE DataKinds, DeriveAnyClass, GeneralizedNewtypeDeriving, TypeOperators #-}
module Language.Python.Syntax where

import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import Data.Functor.Union
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import GHC.Generics
import GHC.Stack
import Language.Python.Grammar as Grammar
import Prologue hiding (Location)
import Term

type Syntax = Union Syntax'
type Syntax' =
  '[ Comment.Comment
   , Declaration.Import
   , Declaration.Variable
   , Expression.Arithmetic
   , Expression.Boolean
   , Expression.Bitwise
   , Expression.Call
   , Expression.Tuple
   , Expression.Unary
   , Literal.Boolean
   , Literal.Float
   , Literal.Integer
   , Literal.None
   , Literal.String
   , Literal.Boolean
   , Literal.TextElement
   , Redirect
   , Statement.Assignment
   , Statement.If
   , Statement.Return
   , Syntax.Empty
   , Syntax.Identifier
   ]

data Redirect a = Redirect !a !a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Redirect where liftEq = genericLiftEq
instance Show1 Redirect where liftShowsPrec = genericLiftShowsPrec

-- | Assignment from AST in Python's grammar onto a program in Python's syntax.
assignment :: HasCallStack => Assignment (Node Grammar) [Term Syntax Location]
assignment = symbol Module *> children (many declaration)


declaration :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
declaration = comment <|> literal <|> statement <|> import' <|> importFrom


statement :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
statement = expressionStatement
          <|> ifStatement
          <|> returnStatement
          <|> identifier
          <|> assignment'
          <|> printStatement

tuple :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
tuple = makeTerm <$> symbol Tuple <*> children (Expression.Tuple <$> (many expression))

expression :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
expression = identifier <|> statement <|> unaryOperator <|> binaryOperator <|> booleanOperator <|> tuple <|> literal <|> true <|> false

true :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
true = makeTerm <$> symbol Grammar.True <*> (Literal.true <$ source)

false :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
false = makeTerm <$> symbol Grammar.False <*> (Literal.false <$ source)

unaryOperator :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
unaryOperator = makeTerm <$> symbol UnaryOperator <*> children (  Expression.UComplement <$> (symbol AnonTilde *> expression)
                                                              <|> Expression.UMinus      <$> (symbol AnonMinus *> expression)
                                                              <|> Expression.UPlus       <$> (symbol AnonPlus  *> expression))

binaryOperator :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
binaryOperator = symbol BinaryOperator >>= \ location -> children (expression >>= \ lexpression ->
      makeTerm location <$> arithmetic lexpression
  <|> makeTerm location <$> bitwise lexpression)
  where
    arithmetic lexpression =  symbol AnonPlus *> (Expression.Plus lexpression <$> expression)
                          <|> symbol AnonMinus *> (Expression.Minus lexpression <$> expression)
                          <|> symbol AnonStar *> (Expression.Times lexpression <$> expression)
                          <|> symbol AnonSlash *> (Expression.DividedBy lexpression <$> expression)
                          <|> symbol AnonSlashSlash *> (Expression.DividedBy lexpression <$> expression)
                          <|> symbol AnonPercent *> (Expression.Modulo lexpression <$> expression)
                          <|> symbol AnonStarStar *> (Expression.Power lexpression <$> expression)
    bitwise lexpression =  symbol AnonPipe *> (Expression.BOr lexpression <$> expression)
                       <|> symbol AnonAmpersand *> (Expression.BAnd lexpression <$> expression)
                       <|> symbol AnonCaret *> (Expression.BXOr lexpression <$> expression)
                       <|> symbol AnonLAngleLAngle *> (Expression.LShift lexpression <$> expression)
                       <|> symbol AnonRAngleRAngle *> (Expression.RShift lexpression <$> expression)

booleanOperator :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
booleanOperator = makeTerm <$> symbol BooleanOperator <*> children ( expression >>= \ lexpression -> (booleanOperator' lexpression))
  where
    booleanOperator' lexpression =  symbol AnonAnd *> (Expression.And lexpression <$> expression)
                                <|> symbol AnonOr *> (Expression.Or lexpression <$> expression)


-- TODO: Handle multiple assignments
assignment' :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
assignment' =  makeTerm <$> symbol Assignment <*> children (Statement.Assignment <$> (symbol ExpressionList *> children expression) <*> (symbol ExpressionList *> children expression))
identifier :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
identifier = makeTerm <$> symbol Identifier <*> (Syntax.Identifier <$> source)

literal :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
literal = string <|> integer <|> float <|> boolean <|> none <|> concatenatedString

-- TODO: Wrap `Literal.TextElement` with a `Literal.String`
string :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
string = makeTerm <$> symbol String <*> (Literal.TextElement <$> source)

concatenatedString :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
concatenatedString = makeTerm <$> symbol ConcatenatedString <*> children (Literal.TextElement . mconcat <$> many (symbol String *> source))

float :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
float = makeTerm <$> symbol Float <*> (Literal.Float <$> source)

integer :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

comment :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

expressionStatement :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
expressionStatement = symbol ExpressionStatement *> children (statement <|> literal <|> expression)


-- TODO Possibly match against children for dotted name and identifiers
import' :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
import' = makeTerm <$> symbol ImportStatement <*> (Declaration.Import <$> source)

-- TODO Possibly match against children nodes
importFrom :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
importFrom = makeTerm <$> symbol ImportFromStatement <*> (Declaration.Import <$> source)

-- TODO How should we assign chevron for print statements?
chevron :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
chevron = makeTerm <$> symbol Chevron <*> (Syntax.Empty <$ source)

-- TODO How to convert statements like this into a `Expression.Call`? The (pack "assert") produces a ByteString but that reduces the `Call` constructor to only accepting byteStrings.
-- assertStatement :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
-- assertStatement = makeTerm <$> symbol AssertStatement <*> children (Expression.Call <$> (symbol AnonAssert *> pack "assert" <$> many expression))

printStatement :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
printStatement = symbol PrintStatement >>= \ location -> children (printKeyword >>= \ print ->
      redirectCallTerm location print
  <|> printCallTerm location print)
  where
    printKeyword = makeTerm <$> symbol AnonPrint <*> (Syntax.Identifier <$> source)
    redirectCallTerm location keyword = makeTerm location <$ symbol Chevron <*> (flip Redirect <$> children expression <*> printCallTerm location keyword)
    printCallTerm location keyword = makeTerm location . Expression.Call keyword <$> many expression

-- globalStatement :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
-- globalStatement = makeTerm <$ symbol GlobalStatement <*> location <*> children (some identifier)

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
boolean =  makeTerm <$> symbol Grammar.True  <*> (Literal.true <$ source)
       <|> makeTerm <$> symbol Grammar.False <*> (Literal.false <$ source)

none :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
none = makeTerm <$> symbol None <*> (Literal.None <$> source)

makeTerm :: HasCallStack => InUnion Syntax' f => a -> f (Term Syntax a) -> Term Syntax a
makeTerm a f = cofree (a :< inj f)
