{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.Java.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Assigning.Assignment hiding (Assignment, Error)
import Data.Abstract.FreeVariables
import Data.Functor (void, ($>))
import Data.List.NonEmpty (some1)
import Data.Record
import Data.Semigroup
import Data.Syntax (contextualize, emptyTerm, handleError, infixContext, makeTerm, makeTerm', makeTerm'', makeTerm1, parseError, postContextualize)
import Data.Union
import GHC.Stack
import Language.Java.Grammar as Grammar
import Language.Java.Syntax as Java.Syntax
import qualified Assigning.Assignment as Assignment
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import qualified Data.Term as Term

type Syntax =
  '[ Comment.Comment
   , Declaration.Class
   , Declaration.Method
   , Declaration.VariableDeclaration
   , Java.Syntax.ArrayType
   , Literal.Array
   , Literal.Integer
   , Literal.String
   , Literal.TextElement
   , Statement.Assignment
   , Syntax.Context
   , Syntax.Empty
   , Syntax.Error
   , Syntax.Identifier
   , Syntax.AccessibilityModifier
   , Syntax.Program
   , Type.Int
   , Type.Void
   , Type.Float
   , Type.Annotation
   , Statement.Return
   , []
   ]

type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment [] Grammar Term

-- | Assignment from AST in Java's grammar onto a program in Java's syntax.
assignment :: Assignment
assignment = handleError $ makeTerm <$> symbol Grammar.Program <*> children (Syntax.Program <$> manyTerm expression) <|> parseError

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
manyTerm term = many (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

someTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
someTerm term = some (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

term :: Assignment -> Assignment
term term = contextualize comment (postContextualize comment term)

expression :: Assignment
expression = handleError (choice expressionChoices)

expressionChoices :: [Assignment.Assignment [] Grammar Term]
expressionChoices =
  [
    array_initializer
  , char
  , class'
  , constantDeclaration
  , identifier
  , integer
  , method
  , return'
  , string
  , local_variable_declaration_statement
  ]

modifier :: Assignment
modifier = makeTerm <$> symbol Modifier <*> (Syntax.AccessibilityModifier <$> source)

array_initializer :: Assignment
array_initializer = makeTerm <$> symbol ArrayInitializer <*> (Literal.Array <$> many expression)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

-- constantDeclaration :: Assignment
-- constantDeclaration = makeTerm <$> symbol ConstantDeclaration <*>

-- TODO: Rename these to be conventionally namned functions (camelCase )
local_variable_declaration :: Assignment
local_variable_declaration = makeDecl <$> symbol LocalVariableDeclaration <*> many type' <*> children (Declaration.VariableDeclaration <$> vDeclList)
  where
    makeSingleDecl loc types (target, value) = makeTerm loc (Statement.Assignment types target value)
    makeDecl loc types decls = fmap (makeSingleDecl loc types) decls
    vDeclList = symbol VariableDeclaratorList *> children (many variableDeclarator)
    variableDeclarator = makeTerm <$> symbol VariableDeclarator <*> ((,) <*> variable_declarator_id <*> expression)

local_variable_declaration_statement :: Assignment
local_variable_declaration_statement = makeTerm <$> symbol LocalVariableDeclarationStatement <*> children (many local_variable_declaration)

unannotated_type :: Assignment
unannotated_type = makeTerm <$> symbol Grammar.ArrayType <*> (Java.Syntax.ArrayType <$> source)

variable_declarator_id :: Assignment
variable_declarator_id = makeTerm <$> symbol VariableDeclaratorId <*> (Syntax.Identifier <$> (name <$> source))



integer :: Assignment
integer = makeTerm <$> symbol IntegerLiteral <*> children (symbol DecimalIntegerLiteral >> Literal.Integer <$> source)

string :: Assignment
string = makeTerm <$> symbol StringLiteral <*> (Literal.TextElement <$> source)

char :: Assignment
char = makeTerm <$> symbol CharacterLiteral <*> (Literal.TextElement <$> source)

class' :: Assignment
class' = makeTerm <$> symbol ClassDeclaration <*> children (Declaration.Class <$> many modifier <*> term identifier <*> pure [] <*> classBody)
  where classBody = makeTerm <$> symbol ClassBody <*> children (manyTerm expression)

identifier :: Assignment
identifier = makeTerm <$> symbol Identifier <*> (Syntax.Identifier <$> (name <$> source))

method :: Assignment
method = makeTerm <$> symbol MethodDeclaration <*> children (
             (makeMethod <$> many modifier <* symbol MethodHeader <*> emptyTerm <*> children ((,) <$> type' <* symbol MethodDeclarator <*> children ( (,) <$> identifier <*> manyTerm parameter)) )
          <* symbol MethodBody <*> children (makeTerm <$> symbol Block <*> children (manyTerm expression))
          )
  where makeMethod modifiers receiver (returnType, (name, params)) body = Declaration.Method (returnType : modifiers) receiver name params body
        parameter = makeTerm <$> symbol FormalParameter <*> children (flip Type.Annotation <$> type' <* symbol VariableDeclaratorId <*> children identifier)

-- TODO: re-introduce makeTerm later; matching types as part of the type rule for now

return' :: Assignment
return' = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> expression)

type' :: Assignment
type' =   makeTerm <$> token VoidType <*> pure Type.Void
     <|>  makeTerm <$> token IntegralType <*> pure Type.Int
     <|>  makeTerm <$> token FloatingPointType <*> pure Type.Float
     -- <|> makeTerm <$> symbol FloatingPointType <*> children (token AnonFloat $> Type.Float <|> token AnonDouble $> Type.Double)
