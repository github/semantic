{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.Java.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Assigning.Assignment hiding (Assignment, Error, while, try)
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
import Prelude hiding (break)

type Syntax =
  '[ Comment.Comment
   , Declaration.Class
   , Declaration.InterfaceDeclaration
   , Declaration.Method
   , Declaration.VariableDeclaration
   , Java.Syntax.ArrayType
   , Java.Syntax.EnumDeclaration
   , Java.Syntax.Import
   , Java.Syntax.Module
   , Java.Syntax.Package
   , Literal.Array
   , Literal.Boolean
   , Literal.Integer
   , Literal.Float
   , Literal.Null
   , Literal.String
   , Literal.TextElement
   , Statement.Assignment
   , Statement.Break
   , Statement.Catch
   , Statement.Continue
   , Statement.DoWhile
   , Statement.Finally
   , Statement.If
   , Statement.Match
   , Statement.Pattern
   , Statement.While
   , Statement.Throw
   , Statement.Try
   , Syntax.Context
   , Syntax.Empty
   , Syntax.Error
   , Syntax.Identifier
   , Syntax.AccessibilityModifier
   , Syntax.Program
   , Type.Bool
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
-- matches comments before and after the node

expression :: Assignment
expression = handleError (choice expressionChoices)
-- <?> "expression"
-- choice walks the expressionChoices and inserts <|> (notionally but not really lol)

expressions :: Assignment
expressions = makeTerm'' <$> location <*> many expression

expressionChoices :: [Assignment.Assignment [] Grammar Term]
expressionChoices =
  [
    arrayInitializer
  , block
  , boolean
  , break
  , char
  , class'
  , continue
  -- , constantDeclaration
  , doWhile
  , float
  , enum
  -- , hexadecimal
  , if'
  , interface
  , identifier
  , import'
  , integer
  , method
  , module'
  , null'
  , package
  , return'
  , string
  , switch
  , throw
  , try
  , localVariableDeclaration
  , localVariableDeclarationStatement
  , while
  ]
  -- adding something to expressionChoices list is useful because expression (above) uses expressionChoices, and so
  -- it is available to form assignments when we encounter any of those terms

modifier :: Assignment
modifier = makeTerm <$> symbol Modifier <*> (Syntax.AccessibilityModifier <$> source)

arrayInitializer :: Assignment
arrayInitializer = makeTerm <$> symbol ArrayInitializer <*> (Literal.Array <$> many expression)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

-- constantDeclaration :: Assignment
-- constantDeclaration = makeTerm <$> symbol ConstantDeclaration <*>

localVariableDeclaration :: Assignment
localVariableDeclaration = makeDecl <$> symbol LocalVariableDeclaration <*> children ((,) <$> some type' <*> vDeclList)
  where
    makeSingleDecl loc types (target, value) = makeTerm loc (Statement.Assignment types target value)
    makeDecl loc (types, decls) = makeTerm loc $ fmap (makeSingleDecl loc types) decls
    vDeclList = symbol VariableDeclaratorList *> children (some variableDeclarator)
    variableDeclarator = symbol VariableDeclarator *> children ((,) <$> variable_declarator_id <*> expression)

localVariableDeclarationStatement :: Assignment
localVariableDeclarationStatement = symbol LocalVariableDeclarationStatement *> children localVariableDeclaration

unannotatedType :: Assignment
unannotatedType = makeTerm <$> symbol Grammar.ArrayType <*> (Java.Syntax.ArrayType <$> source)

variable_declarator_id :: Assignment
variable_declarator_id = symbol VariableDeclaratorId *> children identifier

-- Literals

-- TODO: Need to disaggregate true/false in treesitter
boolean :: Assignment
boolean =  makeTerm <$> symbol BooleanLiteral <*> children
          (token Grammar.True $> Literal.true
          <|> token Grammar.False $> Literal.false)

-- *> pure = $>

null' :: Assignment
null' = makeTerm <$> symbol NullLiteral <*> (Literal.Null <$ source)
-- why is this <$?

-- Supports all integer and floating point literals (hex, octal, binary)
integer :: Assignment
integer = makeTerm <$> symbol IntegerLiteral <*> children (Literal.Integer <$> source)

float :: Assignment
float = makeTerm <$> symbol FloatingPointLiteral <*> children (Literal.Float <$> source)

string :: Assignment
string = makeTerm <$> symbol StringLiteral <*> (Literal.TextElement <$> source)

char :: Assignment
char = makeTerm <$> symbol CharacterLiteral <*> (Literal.TextElement <$> source)

class' :: Assignment
class' = makeTerm <$> symbol ClassDeclaration <*> children (Declaration.Class <$> many modifier <*> term identifier <*> pure [] <*> classBody)
  where classBody = makeTerm <$> symbol ClassBody <*> children (manyTerm expression)

-- consolidated with scopedIdentifier
identifier :: Assignment
identifier = makeTerm <$> (symbol Identifier <|> symbol ScopedIdentifier <|> symbol TypeIdentifier) <*> (Syntax.Identifier . name <$> source)

method :: Assignment
method = makeTerm <$> symbol MethodDeclaration <*> children (
             (makeMethod <$> many modifier <* symbol MethodHeader <*> emptyTerm <*> children ((,) <$> type' <* symbol MethodDeclarator <*> children ( (,) <$> identifier <*> manyTerm parameter)) )
          <* symbol MethodBody <*> children (makeTerm <$> symbol Block <*> children (manyTerm expression))
          )
  where makeMethod modifiers receiver (returnType, (name, params)) body = Declaration.Method (returnType : modifiers) receiver name params body
        parameter = makeTerm <$> symbol FormalParameter <*> children (flip Type.Annotation <$> type' <* symbol VariableDeclaratorId <*> children identifier)
-- TODO: re-introduce makeTerm later; matching types as part of the type rule for now.

module' :: Assignment
module' = makeTerm <$> symbol ModuleDeclaration <*> children (Java.Syntax.Module <$> expression <*> many expression)

import' :: Assignment
import' = makeTerm <$> symbol ImportDeclaration <*> children (Java.Syntax.Import <$> some identifier)

interface :: Assignment
interface = makeTerm <$> symbol InterfaceDeclaration <*> children (normal <|> annotationType)
  where
    interfaceBody = makeTerm <$> symbol InterfaceBody <*> children (many expression)
    normal = symbol NormalInterfaceDeclaration *> children (Declaration.InterfaceDeclaration [] <$> identifier <*> interfaceBody)
    annotationType = symbol AnnotationTypeDeclaration *> children (Declaration.InterfaceDeclaration [] <$> identifier <*> annotationTypeBody)
    annotationTypeBody = makeTerm <$> symbol AnnotationTypeBody <*> children (many expression)

package :: Assignment
package = makeTerm <$> symbol PackageDeclaration <*> children (Java.Syntax.Package <$> some identifier)

enum :: Assignment
enum = makeTerm <$> symbol Grammar.EnumDeclaration <*> children (Java.Syntax.EnumDeclaration <$> term identifier <*> manyTerm enumConstant)
    where enumConstant = symbol EnumConstant *> children (term identifier)
-- list of 0 or more
-- Java.Syntax.EnumDeclaration is taking something that has been matched and applying a function over it
-- makeTerm (a function) is not matching, but rather mapping over a matched term
-- makeTerm is lifted into the <$> functor, which is applied to the result of its child assignments
-- <*> apply is used when you've got a function built up on the LHS
-- we don't have a makeTerm, so we don't have a function on the LHS to apply <*>, hence we just match on the symbol EnumConstant, and use it as a marker to descend into children
-- we want the effect, not the result, of symbol because we want to match the EnumConstant node without caring about its range or span
-- we don't care about the range and span because the identifier rule produces a term which already has a range and span
-- show only has one argument, so we don't need to <*> because when we fmap it over a list, it's fully applied
-- term = also accounts for preceding comments
-- (+) <$> [1,2,3] :: Num a => [a -> a] -- it is a function that takes one number and returns another number of the same type

return' :: Assignment
return' = makeTerm <$> symbol ReturnStatement <*> (Statement.Return <$> children (expression))
-- can move the children into or out of the fmap rule because the children expression returns the result of a child
-- so if you fmap over the result of RHS it's equivalent
-- if you f <$> (g <$> a) == f . g <$> a (fusion law)
--   if you have two nested fmaps, same as composing

type' :: Assignment
type' =  choice [
       makeTerm <$> token VoidType <*> pure Type.Void
     , makeTerm <$> token IntegralType <*> pure Type.Int
     , makeTerm <$> token FloatingPointType <*> pure Type.Float
     , makeTerm <$> token BooleanType <*> pure Type.Bool
     , symbol CatchType *> children (term type')
     , identifier
    ]
     -- <|> makeTerm <$> symbol FloatingPointType <*> children (token AnonFloat $> Type.Float <|> token AnonDouble $> Type.Double)
     -- we had to say token with the first 4 because pure don't advance past the first nodes; implies no effect, just produces value
     -- if we want to match a node and consume that node (which we have to do) we need to use token because it has that behavior

-- method expressions

if' :: Assignment
if' = makeTerm <$> symbol IfThenElseStatement <*> children (Statement.If <$> term expression <*> term expression <*> (term expression <|> emptyTerm))

block :: Assignment
block = makeTerm <$> symbol Block <*> children (manyTerm expression)

while :: Assignment
while = makeTerm <$> symbol WhileStatement <*> children (Statement.While <$> term expression <*> term expression)

doWhile :: Assignment
doWhile = makeTerm <$> symbol DoStatement <*> children (flip Statement.DoWhile <$> term expression <*> term expression)
-- flipping so when we match body it goes into second field and when we match condition it goes into the first field

switch :: Assignment
switch = makeTerm <$> symbol SwitchStatement <*> children (Statement.Match <$> term expression <*> switchBlock)
  where
    switchBlock = makeTerm <$> symbol SwitchBlock <*> children (manyTerm switchLabel)
    switchLabel = makeTerm <$> symbol SwitchLabel <*> (Statement.Pattern <$> children (term expression <|> emptyTerm) <*> expressions)
-- not identifier, expression

break :: Assignment
break = makeTerm <$> symbol BreakStatement <*> children (Statement.Break <$> (term expression <|> emptyTerm))
-- manyTerm matches 0 or more and also produces a list
-- term expression <|> emptyTerm accounts for an expression or nothing at all

continue :: Assignment
continue = makeTerm <$> symbol ContinueStatement <*> children (Statement.Continue <$> (term expression <|> emptyTerm))

throw :: Assignment
throw = makeTerm <$> symbol ThrowStatement <*> children (Statement.Throw <$> term expression)

try :: Assignment
try = makeTerm <$> symbol TryStatement <*> children (Statement.Try <$> term expression <*> (append <$> optional catches <*> optional finally))
  where
    catches = symbol Catches *> children (manyTerm catch)
    catch = makeTerm <$> symbol CatchClause <*> children (Statement.Catch <$> catchFormalParameter <*> term expression)
    catchFormalParameter = makeTerm <$> symbol CatchFormalParameter <*> children (flip Type.Annotation <$> type' <* symbol VariableDeclaratorId <*> children identifier)
    finally = makeTerm <$> symbol Finally <*> children (Statement.Finally <$> term expression)
    -- append catches finally =
    append Nothing Nothing = []
    append Nothing (Just a) = [a]
    append (Just a) Nothing = a
    append (Just a) (Just b) = a <> [b]

-- for :: Assignment
-- for = makeTerm <$> symbol For <*> children (Statement.ForEach <$> (makeTerm <$> location <*> manyTermsTill expression (symbol In)) <*> inClause <*> expressions)
--   where inClause = symbol In *> children expression

-- expression

-- infix operators
-- binary :: Assignment
-- binary = makeTerm
