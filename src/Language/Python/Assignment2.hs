{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.Python.Assignment2
( assignment
, Syntax
, Grammar
, Term
) where

import Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import Data.List.NonEmpty (some1)
import Data.Maybe (fromMaybe)
import Data.Record
import Data.Syntax (contextualize, emptyTerm, handleError, makeTerm, makeTerm1, parseError, postContextualize)
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import qualified Data.Term as Term
import Data.Union
import GHC.Stack
import Language.Python.Grammar as Grammar

type Syntax =
  '[ Comment.Comment
   , Declaration.Function
   , Expression.Call
   , Literal.Boolean
   , Literal.Integer
   , Literal.TextElement
   , Statement.Assignment
   , Syntax.Context
   , Syntax.Empty
   , Syntax.Error
   , Syntax.Identifier
   , Syntax.Program
   , Type.Annotation
   , []
   ]

type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment [] Grammar Term

-- | Assignment from AST in Python's grammar onto a program in Python's syntax.
assignment :: Assignment
assignment = handleError $ makeTerm <$> symbol Module <*> children (Syntax.Program <$> manyTerm expression) <|> parseError

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
manyTerm term = many (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

someTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
someTerm term = some (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

term :: Assignment -> Assignment
term term = contextualize comment (postContextualize comment term)

expression :: Assignment
expression = handleError (choice expressionChoices)

expressionStatement :: Assignment
expressionStatement = mk <$> symbol ExpressionStatement <*> children (someTerm expression)
  where mk _ [child] = child
        mk location children = makeTerm location children

expressionChoices :: [Assignment.Assignment [] Grammar Term]
expressionChoices =
  -- Long-term, can we de/serialize assignments and avoid paying the cost of construction altogether?
  [ assignment'
  , boolean
  , call
  , expressionStatement
  , functionDefinition
  , identifier
  , integer
  , string
  ]

expressions :: Assignment
expressions = makeTerm <$> location <*> manyTerm expression

expressionList :: Assignment
expressionList = mk <$> symbol ExpressionList <*> children (someTerm expression)
  where mk _ [child] = child
        mk location children = makeTerm location children

boolean :: Assignment
boolean =  makeTerm <$> token Grammar.True <*> pure Literal.true
       <|> makeTerm <$> token Grammar.False <*> pure Literal.false

identifier :: Assignment
identifier = makeTerm <$> (symbol Identifier <|> symbol Identifier') <*> (Syntax.Identifier <$> source)

string :: Assignment
string = makeTerm <$> symbol String <*> (Literal.TextElement <$> source)

integer :: Assignment
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

call :: Assignment
call = makeTerm <$> symbol Call <*> children
  (Expression.Call <$> pure []
                   <*> term expression
                   <*> (symbol ArgumentList *> children (manyTerm expression))
                   <*> emptyTerm)

functionDefinition :: Assignment
functionDefinition
  =   makeFunctionDeclaration <$> symbol FunctionDefinition <*> children ((,,,) <$> term expression <* symbol Parameters <*> children (manyTerm expression) <*> optional (symbol Type *> children (term expression)) <*> expressions)
  <|> makeFunctionDeclaration <$> (symbol Lambda' <|> symbol Lambda) <*> children ((,,,) <$ token AnonLambda <*> emptyTerm <*> (symbol LambdaParameters *> children (manyTerm expression) <|> pure []) <*> optional (symbol Type *> children (term expression)) <*> expressions)
  where
    makeFunctionDeclaration loc (functionName', functionParameters, ty, functionBody) = makeTerm loc $ Type.Annotation (makeTerm loc $ Declaration.Function [] functionName' functionParameters functionBody) (fromMaybe (makeTerm loc Syntax.Empty) ty)

assignment' :: Assignment
assignment' =  makeTerm  <$> symbol Assignment <*> children (Statement.Assignment [] <$> term expressionList <*> term rvalue)
  where rvalue = expressionList <|> assignment'
