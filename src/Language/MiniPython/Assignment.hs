{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- FIXME
module Language.MiniPython.Assignment
(
-- Small version of Python to enable internal framework development.
  assignment
, Syntax
, Grammar
, Term
) where

import           Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import           Data.Abstract.Name (name)
import           Data.Sum
import           Data.Syntax
    ( contextualize
    , emptyTerm
    , handleError
    , infixContext
    , makeTerm
    , makeTerm'
    , makeTerm''
    , makeTerm1
    , parseError
    , postContextualize
    )
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import qualified Data.Term as Term
import           Language.Python.Grammar as Grammar
import           Prologue


-- | The type of Python syntax.
type Syntax =
  '[ Declaration.Function
   , Expression.Call
   , Expression.Minus
   , Expression.Plus
   , Expression.Times
   , Literal.Integer
   , Literal.Boolean
   , Literal.TextElement
   , Statement.If
   , Statement.Return
   , Statement.Statements
   , Syntax.Context
   , Syntax.Empty
   , Syntax.Error
   , Syntax.Identifier
   , Type.Annotation
   , Comment.Comment
   , []
   ]

type Term = Term.Term (Sum Syntax) Location
type Assignment = Assignment.Assignment [] Grammar

-- | Assignment from AST in Python's grammar onto a program in Python's syntax.
assignment :: Assignment Term
assignment = handleError $ makeTerm <$> symbol Module <*> children (Statement.Statements <$> manyTerm expression) <|> parseError

expression :: Assignment Term
expression = handleError (choice expressionChoices)

expressionChoices :: [Assignment Term]
expressionChoices =
  [ binaryOperator
  , boolean
  , call
  , expressionStatement
  , functionDefinition
  , identifier
  , integer
  , string
  , returnStatement
  , ifStatement
  ]

-- NOTE: Important that we don't flatten out the Imperative for single item lists
expressions :: Assignment Term
expressions = makeTerm <$> location <*> manyTerm expression

expressionStatement :: Assignment Term
expressionStatement = makeTerm'' <$> symbol ExpressionStatement <*> children (someTerm expression)

expressionList :: Assignment Term
expressionList = makeTerm'' <$> symbol ExpressionList <*> children (someTerm expression)

functionDefinition :: Assignment Term
functionDefinition =
      makeFunctionDeclaration <$> symbol FunctionDefinition <*> children ((,,,) <$> term expression <* symbol Parameters <*> children (manyTerm expression) <*> optional (symbol Type *> children (term expression)) <*> expressions)
  <|> makeFunctionDeclaration <$> (symbol Lambda' <|> symbol Lambda) <*> children ((,,,) <$ token AnonLambda <*> emptyTerm <*> (symbol LambdaParameters *> children (manyTerm expression) <|> pure []) <*> optional (symbol Type *> children (term expression)) <*> expressions)
  where
    makeFunctionDeclaration loc (functionName', functionParameters, ty, functionBody)
      = let fn = makeTerm loc (Declaration.Function [] functionName' functionParameters functionBody)
        in maybe fn (makeTerm loc . Type.Annotation fn) ty

binaryOperator :: Assignment Term
binaryOperator = makeTerm' <$> symbol BinaryOperator <*> children (infixTerm expression (term expression)
  [ (inject .) . Expression.Plus      <$ symbol AnonPlus
  , (inject .) . Expression.Minus     <$ symbol AnonMinus
  , (inject .) . Expression.Times     <$ symbol AnonStar
  ])

identifier :: Assignment Term
identifier = makeTerm <$> (symbol Identifier <|> symbol Identifier' <|> symbol DottedName) <*> (Syntax.Identifier . name <$> source)

integer :: Assignment Term
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

string :: Assignment Term
string = makeTerm <$> symbol String <*> (Literal.TextElement <$> source)

comment :: Assignment Term
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

returnStatement :: Assignment Term
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> term (expressionList <|> emptyTerm))

call :: Assignment Term
call = makeTerm <$> symbol Call <*> children (Expression.Call [] <$> term (identifier <|> expression) <*> (symbol ArgumentList *> children (manyTerm expression)) <*> emptyTerm)

boolean :: Assignment Term
boolean =  makeTerm <$> token Grammar.True <*> pure Literal.true
       <|> makeTerm <$> token Grammar.False <*> pure Literal.false

ifStatement :: Assignment Term
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> term expression <*> term (makeTerm <$> location <*> manyTermsTill expression (void (symbol ElseClause) <|> void (symbol ElifClause) <|> eof)) <*> (flip (foldr makeElif) <$> many elifClause <*> (symbol ElseClause *> children expressions <|> emptyTerm)))
  where elifClause = (,) <$> symbol ElifClause <*> children (Statement.If <$> term expression <*> expressions)
        makeElif (loc, makeIf) rest = makeTerm loc (makeIf rest)

-- Helpers

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment Term -> Assignment [Term]
manyTerm term = many (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

someTerm :: Assignment Term -> Assignment [Term]
someTerm term = some (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

term :: Assignment Term -> Assignment Term
term term = contextualize comment (postContextualize comment term)

-- | Match a series of terms or comments until a delimiter is matched.
manyTermsTill :: Assignment Term -> Assignment b -> Assignment [Term]
manyTermsTill step end = manyTill (step <|> comment) end

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment Term
          -> Assignment Term
          -> [Assignment (Term -> Term -> Sum Syntax Term)]
          -> Assignment (Sum Syntax Term)
infixTerm = infixContext comment

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
