{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.Java.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Assigning.Assignment hiding (Assignment, Error, try)
import Data.Abstract.Name
import Data.Functor (($>))
import Data.List.NonEmpty (some1)
import Data.Record
import Data.Syntax (contextualize, emptyTerm, handleError, infixContext, makeTerm, makeTerm', makeTerm'', makeTerm1, parseError, postContextualize)
import Data.Sum
import Language.Java.Grammar as Grammar
import qualified Language.Java.Syntax as Java.Syntax
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
import Prologue hiding (for, try, This, catches, finally)

type Syntax =
  '[ Comment.Comment
   , Declaration.Class
   , Declaration.InterfaceDeclaration
   , Declaration.Method
   , Declaration.VariableDeclaration
   , Expression.Plus
   , Expression.Minus
   , Expression.Times
   , Expression.DividedBy
   , Expression.Modulo
   , Expression.Power
   , Expression.Negate
   , Expression.FloorDivision
   , Expression.Call
   , Expression.LessThan
   , Expression.LessThanEqual
   , Expression.GreaterThan
   , Expression.GreaterThanEqual
   , Expression.Equal
   , Expression.StrictEqual
   , Expression.Comparison
   , Expression.BOr
   , Expression.BXOr
   , Expression.BAnd
   , Expression.LShift
   , Expression.RShift
   , Expression.UnsignedRShift
   , Expression.Complement
   , Expression.And
   , Expression.Not
   , Expression.Or
   , Expression.XOr
   , Expression.InstanceOf
   , Expression.MemberAccess
   , Expression.Subscript
   , Expression.Member
   , Expression.Super
   , Expression.This
   , Java.Syntax.Annotation
   , Java.Syntax.AnnotationField
   , Java.Syntax.AnnotationTypeElement
   , Java.Syntax.ArrayCreationExpression
   , Java.Syntax.AssertStatement
   , Java.Syntax.Asterisk
   , Java.Syntax.Constructor
   , Java.Syntax.ClassBody
   , Java.Syntax.ClassLiteral
   , Java.Syntax.DefaultValue
   , Java.Syntax.DimsExpr
   , Java.Syntax.EnumDeclaration
   , Java.Syntax.GenericType
   , Java.Syntax.Import
   , Java.Syntax.Lambda
   , Java.Syntax.LambdaBody
   , Java.Syntax.MethodReference
   , Java.Syntax.Module
   , Java.Syntax.New
   , Java.Syntax.NewKeyword
   , Java.Syntax.Package
   , Java.Syntax.SpreadParameter
   , Java.Syntax.StaticInitializer
   , Java.Syntax.Synchronized
   , Java.Syntax.TryWithResources
   , Java.Syntax.TypeParameter
   , Java.Syntax.TypeWithModifiers
   , Java.Syntax.Variable
   , Java.Syntax.Wildcard
   , Java.Syntax.WildcardBounds
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
   , Statement.For
   , Statement.ForEach
   , Statement.If
   , Statement.Match
   , Statement.Pattern
   , Statement.PostIncrement
   , Statement.PostDecrement
   , Statement.PreIncrement
   , Statement.PreDecrement
   , Statement.While
   , Statement.Statements
   , Statement.Throw
   , Statement.Try
   , Syntax.Context
   , Syntax.Empty
   , Syntax.Error
   , Syntax.Identifier
   , Syntax.AccessibilityModifier
   , Type.Array
   , Type.Bool
   , Type.Int
   , Type.Void
   , Type.Float
   , Type.Annotation
   , Statement.Return
   , []
   ]

type Term = Term.Term (Sum Syntax) (Record Location)
type Assignment = Assignment.Assignment [] Grammar

-- | Assignment from AST in Java's grammar onto a program in Java's syntax.
assignment :: Assignment Term
assignment = handleError $ makeTerm <$> symbol Grammar.Program <*> children (Statement.Statements <$> manyTerm expression) <|> parseError

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment Term -> Assignment [Term]
manyTerm term = many (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

-- | Match a series of terms or comments until a delimiter is matched.
manyTermsTill :: Assignment Term
              -> Assignment b
              -> Assignment [Term]
manyTermsTill step = manyTill (step <|> comment)

someTerm :: Assignment Term -> Assignment [Term]
someTerm term = some (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

-- | Match comments before and after the node.
term :: Assignment Term -> Assignment Term
term term = contextualize comment (postContextualize comment term)

-- | Match
expression :: Assignment Term
expression = handleError (choice expressionChoices)

expressions :: Assignment Term
expressions = makeTerm'' <$> location <*> many expression

expressionChoices :: [Assignment Term]
expressionChoices =
  [
    arrayAccess
  , arrayCreationExpression
  , arrayInitializer
  , assert
  , assignment'
  , block
  , binary
  , boolean
  , break
  , castExpression
  , char
  , class'
  , classBody
  , classInstance
  , classLiteral
  , continue
  , constructorDeclaration
  , dimsExpr
  , explicitConstructorInvocation
  -- , TODO: constantDeclaration
  , doWhile
  , fieldAccess
  , fieldDeclaration
  , float
  , for
  , enum
  , if'
  , interface
  , identifier
  , import'
  , integer
  , lambda
  , method
  , methodInvocation
  , methodReference
  , module'
  , null'
  , package
  , return'
  , scopedIdentifier
  , string
  , super
  , switch
  , staticInitializer
  , synchronized
  , ternary
  , this
  , throw
  , try
  , unary
  , update
  , localVariableDeclaration
  , localVariableDeclarationStatement
  , while
  ]

modifier :: Assignment Term
modifier = make <$> symbol Modifier <*> children(Left <$> annotation <|> Right . Syntax.AccessibilityModifier <$> source)
  where
    make loc (Right modifier) = makeTerm loc modifier
    make _ (Left annotation) = annotation

arrayInitializer :: Assignment Term
arrayInitializer = makeTerm <$> symbol ArrayInitializer <*> (Literal.Array <$> many expression)

comment :: Assignment Term
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

localVariableDeclaration :: Assignment Term
localVariableDeclaration = makeTerm <$> symbol LocalVariableDeclaration <*> children ((,) <$> manyTerm modifier <*> type' <**> variableDeclaratorList)

variableDeclaratorList :: Assignment (([Term], Term) -> [Term])
variableDeclaratorList = symbol VariableDeclaratorList *> children (makeDecl <$> some variableDeclarator)
  where
    variableDeclarator = symbol VariableDeclarator *> children ((,) <$> variableDeclaratorId <*> optional expression)
    makeDecl decls (modifiers, type') = map (makeSingleDecl modifiers type') decls
    makeSingleDecl modifiers type' (target, Nothing) = makeTerm1 (Java.Syntax.Variable modifiers type' target)
    makeSingleDecl modifiers type' (target, Just value) = makeTerm1 (Statement.Assignment [] (makeTerm1 (Java.Syntax.Variable modifiers type' target)) value)

-- variable declarator -> variable initializer -> expression -> primary -> array creation expression
arrayCreationExpression :: Assignment Term
arrayCreationExpression = makeTerm <$> symbol Grammar.ArrayCreationExpression <*> children (Java.Syntax.ArrayCreationExpression <$> (new *> type') <*> many dimsExpr)
  where new = token AnonNew *> pure Java.Syntax.NewKeyword

localVariableDeclarationStatement :: Assignment Term
localVariableDeclarationStatement = symbol LocalVariableDeclarationStatement *> children localVariableDeclaration

variableDeclaratorId :: Assignment Term
variableDeclaratorId = symbol VariableDeclaratorId *> children identifier

-- Literals
boolean :: Assignment Term
boolean =  makeTerm <$> symbol BooleanLiteral <*> children
          (token Grammar.True $> Literal.true
          <|> token Grammar.False $> Literal.false)

null' :: Assignment Term
null' = makeTerm <$> symbol NullLiteral <*> (Literal.Null <$ source)

-- Integer supports all integer and floating point literals (hex, octal, binary)
integer :: Assignment Term
integer = makeTerm <$> symbol IntegerLiteral <*> children (Literal.Integer <$> source)

float :: Assignment Term
float = makeTerm <$> symbol FloatingPointLiteral <*> children (Literal.Float <$> source)

string :: Assignment Term
string = makeTerm <$> symbol StringLiteral <*> (Literal.TextElement <$> source)

char :: Assignment Term
char = makeTerm <$> symbol CharacterLiteral <*> (Literal.TextElement <$> source)

-- Identifiers
identifier :: Assignment Term
identifier = makeTerm <$> (symbol Identifier) <*> (Syntax.Identifier . name <$> source)

typeIdentifier :: Assignment Term
typeIdentifier = makeTerm <$> symbol TypeIdentifier <*> (Syntax.Identifier . name <$> source)

identifier' :: Assignment Name
identifier' = (symbol Identifier <|> symbol TypeIdentifier <|> symbol Identifier') *> (name <$> source)
-- we want a name and not a full term wrapping the same, so we match the same stuff as identifier but we just produce the name

scopedIdentifier :: Assignment Term
scopedIdentifier = makeTerm <$> symbol ScopedIdentifier <*> children (Expression.MemberAccess <$> term expression <*> identifier')

superInterfaces :: Assignment [Term]
superInterfaces = symbol SuperInterfaces *> children (symbol InterfaceTypeList *> children(manyTerm type'))

-- Declarations
class' :: Assignment Term
class' = makeTerm <$> symbol ClassDeclaration <*> children (makeClass <$> many modifier <*> term identifier <*> (typeParameters <|> pure []) <*> optional superClass <*> (superInterfaces <|> pure []) <*> classBody)
  where
    makeClass modifiers identifier typeParams superClass superInterfaces = Declaration.Class (modifiers <> typeParams) identifier (maybeToList superClass <> superInterfaces) -- not doing an assignment, just straight up function
    -- classBody = makeTerm <$> symbol ClassBody <*> children (manyTerm expression)
    superClass = symbol Superclass *> children type'

classBody :: Assignment Term
classBody = makeTerm <$> symbol Grammar.ClassBody <*> children (manyTerm expression)

staticInitializer :: Assignment Term
staticInitializer = makeTerm <$> symbol Grammar.StaticInitializer <*> children (Java.Syntax.StaticInitializer <$> block)

fieldDeclaration :: Assignment Term
fieldDeclaration = makeTerm <$> symbol FieldDeclaration <*> children ((,) <$> manyTerm modifier <*> type' <**> variableDeclaratorList)

method :: Assignment Term
method = makeTerm <$> symbol MethodDeclaration <*> children (makeMethod <$> many modifier <*> emptyTerm <*> methodHeader <*> methodBody)
  where
    methodBody = symbol MethodBody *> children (term expression <|> emptyTerm)
    methodDeclarator = symbol MethodDeclarator *> children ( (,) <$> identifier <*> formalParameters)
    methodHeader = symbol MethodHeader *> children ((,,,,) <$> (typeParameters <|> pure []) <*> manyTerm annotation <*> type' <*> methodDeclarator <*> (throws <|> pure []))
    makeMethod modifiers receiver (typeParams, annotations, returnType, (name, params), throws) = Declaration.Method (returnType : modifiers <> typeParams <> annotations <> throws) receiver name params
-- methodHeader needs to include typeParameters (it does)

generic :: Assignment Term
generic = makeTerm <$> symbol Grammar.GenericType <*> children(Java.Syntax.GenericType <$> term type' <*> manyTerm type')

methodInvocation :: Assignment Term
methodInvocation = makeTerm <$> symbol MethodInvocation <*> children (uncurry Expression.Call <$> (callFunction <$> expression <*> optional ((,) <$ optional (token AnonRParen) <* token AnonDot <*> manyTerm typeArgument <*> identifier')) <*> (argumentList <|> pure []) <*> emptyTerm)
  where
    callFunction a (Just (typeArguments, b)) = (typeArguments, makeTerm1 (Expression.MemberAccess a b))
    callFunction a Nothing = ([], a)
    -- optional produces a Maybe type (takes a Maybe a and returns a rule that produces a Maybe a)

methodReference :: Assignment Term
methodReference = makeTerm <$> symbol Grammar.MethodReference <*> children (Java.Syntax.MethodReference <$> term type' <*> manyTerm typeArgument <*> (new <|> term identifier))
  where new = makeTerm <$> token AnonNew <*> pure Java.Syntax.NewKeyword
-- can't do term identifier' because identifier' returns a name, not a term, and we want a term
-- <*> - left assoc so when you have a token that you need to match but not retain,
-- manyTerm or alternation with pure, but not bowf

explicitConstructorInvocation :: Assignment Term
explicitConstructorInvocation = makeTerm <$> symbol ExplicitConstructorInvocation <*> children (uncurry Expression.Call <$> (callFunction <$> term expression <*> optional ((,) <$ optional (token AnonRParen) <* token AnonDot <*> manyTerm type' <*> identifier')) <*> argumentList <*> emptyTerm)
  where
    callFunction a (Just (typeArguments, b)) = (typeArguments, makeTerm1 (Expression.MemberAccess a b))
    callFunction a Nothing = ([], a)

module' :: Assignment Term
module' = makeTerm <$> symbol ModuleDeclaration <*> children (Java.Syntax.Module <$> expression <*> many expression)

import' :: Assignment Term
import' = makeTerm <$> symbol ImportDeclaration <*> children (Java.Syntax.Import <$> someTerm (expression <|> asterisk))
  where asterisk = makeTerm <$> token Grammar.Asterisk <*> pure Java.Syntax.Asterisk

interface :: Assignment Term
interface = makeTerm <$> symbol InterfaceDeclaration <*> children (normal <|> annotationType)
  where
    interfaceBody = makeTerm <$> symbol InterfaceBody <*> children (manyTerm interfaceMemberDeclaration)
    normal = symbol NormalInterfaceDeclaration *> children (makeInterface <$> manyTerm modifier <*> identifier <*> (typeParameters <|> pure []) <*> (extends <|> pure []) <*> interfaceBody)
    makeInterface modifiers identifier typeParams = Declaration.InterfaceDeclaration (modifiers ++ typeParams) identifier
    annotationType = symbol AnnotationTypeDeclaration *> children (Declaration.InterfaceDeclaration [] <$> identifier <*> pure [] <*> annotationTypeBody)
    annotationTypeBody = symbol AnnotationTypeBody *> children (annotationTypeMember)
    annotationTypeMember = symbol AnnotationTypeMemberDeclaration *> children (class' <|> interface <|> constant)
    annotationTypeElement = makeTerm <$> symbol AnnotationTypeElementDeclaration <*> children (Java.Syntax.AnnotationTypeElement <$> many modifier <*> identifier <*> (dims <|> pure []) <*> (defaultValue <|> emptyTerm))
    defaultValue = makeTerm <$> symbol DefaultValue <*> children (Java.Syntax.DefaultValue <$> elementValue)
    elementValue = symbol ElementValue *> children (term expression) -- pull this to top level l8r
    interfaceMemberDeclaration = symbol InterfaceMemberDeclaration *> children (term expression)
    extends = symbol ExtendsInterfaces *> children (symbol InterfaceTypeList *> children (manyTerm type'))

constant :: Assignment Term
constant = makeTerm <$> symbol ConstantDeclaration <*> children ((,) <$> pure [] <*> typeIdentifier <**> variableDeclaratorList)

package :: Assignment Term
package = makeTerm <$> symbol PackageDeclaration <*> children (Java.Syntax.Package <$> someTerm expression)

enum :: Assignment Term
enum = makeTerm <$> symbol Grammar.EnumDeclaration <*> children (Java.Syntax.EnumDeclaration <$> manyTerm modifier <*> term identifier <*> (superInterfaces <|> pure []) <*> manyTerm enumConstant <*> (enumBodyDeclarations <|> pure []))
    where
      enumConstant = symbol EnumConstant *> children (term identifier)
      enumBodyDeclarations = symbol EnumBodyDeclarations *> children (manyTerm expression)

return' :: Assignment Term
return' = makeTerm <$> symbol ReturnStatement <*> (Statement.Return <$> children (expression <|> emptyTerm))

-- method expressions
dims :: Assignment [Term]
dims = symbol Dims *> children (many (emptyTerm <* token AnonLBracket <* token AnonRBracket))

-- not sure why we did <* token with the dims (possibly because it's the only thing happening?)
-- will define with manyTerm annotation <*> manyTerm expression and then revisit whether or not I need brackets
dimsExpr :: Assignment Term
dimsExpr = makeTerm <$> symbol Grammar.DimsExpr <*> children (Java.Syntax.DimsExpr <$> manyTerm annotation <*> manyTerm expression)

type' :: Assignment Term
type' =  choice [
       makeTerm <$> symbol VoidType <*> pure Type.Void
     , makeTerm <$> symbol IntegralType <*> children (token AnonInt *> pure Type.Int)
     , makeTerm <$> symbol FloatingPointType <*> pure Type.Float
     , makeTerm <$> symbol BooleanType <*> pure Type.Bool
     , symbol ArrayType *> children (array <$> type' <*> dims) -- type rule recurs into itself
     , symbol CatchType *> children (term type')
     , symbol ExceptionType *> children (term type')
     , makeTerm <$> symbol ScopedTypeIdentifier <*> children (Expression.MemberAccess <$> term type' <*> identifier')
     , wildcard
     , identifier
     , typeIdentifier
     , generic
     , typeArgument
    ]
    where array = foldl (\into each -> makeTerm1 (Type.Array (Just each) into))

typeArgument :: Assignment Term
typeArgument = symbol TypeArgument *> children (term type')

wildcard :: Assignment Term
wildcard = makeTerm <$> symbol Grammar.Wildcard <*> children (Java.Syntax.Wildcard <$> manyTerm annotation <*> optional (super <|> extends))
  where
    super = makeTerm <$> token Super <*> (Java.Syntax.WildcardBoundSuper <$> type')
    extends = makeTerm1 <$> (Java.Syntax.WildcardBoundExtends <$> type')

if' :: Assignment Term
if' = makeTerm <$> symbol IfThenElseStatement <*> children (Statement.If <$> term expression <*> term expression <*> (term expression <|> emptyTerm))

block :: Assignment Term
block = makeTerm <$> symbol Block <*> children (manyTerm expression)

while :: Assignment Term
while = makeTerm <$> symbol WhileStatement <*> children (Statement.While <$> term expression <*> term expression)

doWhile :: Assignment Term
doWhile = makeTerm <$> symbol DoStatement <*> children (flip Statement.DoWhile <$> term expression <*> term expression)

switch :: Assignment Term
switch = makeTerm <$> symbol SwitchStatement <*> children (Statement.Match <$> term expression <*> switchBlock)
  where
    switchBlock = makeTerm <$> symbol SwitchBlock <*> children (manyTerm switchLabel)
    switchLabel = makeTerm <$> symbol SwitchLabel <*> (Statement.Pattern <$> children (term expression <|> emptyTerm) <*> expressions)

break :: Assignment Term
break = makeTerm <$> symbol BreakStatement <*> children (Statement.Break <$> (term expression <|> emptyTerm))

continue :: Assignment Term
continue = makeTerm <$> symbol ContinueStatement <*> children (Statement.Continue <$> (term expression <|> emptyTerm))

throw :: Assignment Term
throw = makeTerm <$> symbol ThrowStatement <*> children (Statement.Throw <$> term expression)

try :: Assignment Term
try = symbol TryStatement *> children tryWithResources <|> makeTerm <$> symbol TryStatement <*> children (Statement.Try <$> term expression <*> (append <$> optional catches <*> optional finally))

catches :: Assignment [Term]
catches = symbol Catches *> children (manyTerm catch)
  where
    catch = makeTerm <$> symbol CatchClause <*> children (Statement.Catch <$> catchFormalParameter <*> term expression)
    catchFormalParameter = makeTerm <$> symbol CatchFormalParameter <*> children (flip Type.Annotation <$> type' <* symbol VariableDeclaratorId <*> children identifier)

finally :: Assignment Term
finally = makeTerm <$> symbol Finally <*> children (Statement.Finally <$> term expression)

append :: Maybe [a] -> Maybe a -> [a]
append Nothing Nothing = []
append Nothing (Just a) = [a]
append (Just a) Nothing = a
append (Just a) (Just b) = a <> [b]

tryWithResources :: Assignment Term
tryWithResources = makeTerm <$> symbol TryWithResourcesStatement <*> children (Java.Syntax.TryWithResources <$> resourceSpecification <*> block <*> (append <$> optional catches <*> optional finally))
  where
    resourceSpecification = symbol ResourceSpecification *> children (manyTerm resource)
    resource = symbol Resource *> children variableAccess <|> makeTerm <$> symbol Resource <*> children (makeSingleDecl <$> many modifier <*> type' <*> variableDeclaratorId <*> term expression)
    variableAccess = symbol VariableAccess *> children (identifier <|> fieldAccess)
    makeSingleDecl modifiers type' target value = Statement.Assignment [] (makeTerm1 (Java.Syntax.Variable modifiers type' target)) value

for :: Assignment Term
for = symbol ForStatement *> children (basicFor <|> enhancedFor)

basicFor :: Assignment Term
basicFor = makeTerm <$> symbol BasicForStatement <*> children (Statement.For <$ token AnonFor <* token AnonLParen <*> (token AnonSemicolon *> emptyTerm <|> forInit <* token AnonSemicolon) <*> (token AnonSemicolon *> emptyTerm <|> term expression <* token AnonSemicolon) <*> forStep <*> term expression)
  where
    forInit = symbol ForInit *> children (term expression)
    forStep = makeTerm <$> location <*> manyTermsTill expression (token AnonRParen)

enhancedFor :: Assignment Term
enhancedFor = makeTerm <$> symbol EnhancedForStatement <*> children (Statement.ForEach <$> (variable <$> manyTerm modifier <*> type' <*> variableDeclaratorId) <*> term expression <*> term expression)
  where variable modifiers type' variableDeclaratorId = makeTerm1 (Java.Syntax.Variable modifiers type' variableDeclaratorId)

assert :: Assignment Term
assert = makeTerm <$> symbol Grammar.AssertStatement <*> children (Java.Syntax.AssertStatement <$> term expression <*> optional (term expression))

-- TODO: instanceOf
binary :: Assignment Term
binary = makeTerm' <$> symbol BinaryExpression <*> children (infixTerm expressionAndParens expressionAndParens
  [ (inject .) . Expression.LessThan         <$ symbol AnonLAngle
  , (inject .) . Expression.GreaterThan      <$ symbol AnonRAngle
  , (inject .) . Expression.Equal            <$ symbol AnonEqualEqual
  , (inject .) . Expression.GreaterThanEqual <$ symbol AnonRAngleEqual
  , (inject .) . Expression.LessThanEqual    <$ symbol AnonLAngleEqual
  , (inject .) . invert Expression.Equal     <$ symbol AnonBangEqual
  , (inject .) . Expression.And              <$ symbol AnonAmpersandAmpersand
  , (inject .) . Expression.Or               <$ symbol AnonPipePipe
  , (inject .) . Expression.BAnd             <$ symbol AnonAmpersand
  , (inject .) . Expression.BOr              <$ symbol AnonPipe
  , (inject .) . Expression.BXOr             <$ symbol AnonCaret
  , (inject .) . Expression.Modulo           <$ symbol AnonPercent
  , (inject .) . Expression.LShift           <$ symbol AnonLAngleLAngle
  , (inject .) . Expression.RShift           <$ symbol AnonRAngleRAngle
  , (inject .) . Expression.UnsignedRShift   <$ symbol AnonRAngleRAngleRAngle
  , (inject .) . Expression.Plus             <$ symbol AnonPlus
  , (inject .) . Expression.Minus            <$ symbol AnonMinus
  , (inject .) . Expression.Times            <$ symbol AnonStar
  , (inject .) . Expression.DividedBy        <$ symbol AnonSlash
  , (inject .) . Expression.InstanceOf       <$ symbol AnonInstanceof
  ])
  where
    invert cons a b = Expression.Not (makeTerm1 (cons a b))
    expressionAndParens = token AnonLParen *> expressionAndParens <* token AnonRParen <|> expression
    -- TODO: expressionAndParens is a hack that accommodates Java's nested parens case but
    -- altering the TreeSitter Java grammar is a better longer term goal.

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment Term
          -> Assignment Term
          -> [Assignment (Term -> Term -> Sum Syntax Term)]
          -> Assignment (Sum Syntax Term)
infixTerm = infixContext comment

assignment' :: Assignment Term
assignment' = makeTerm' <$> symbol AssignmentExpression <*> children (infixTerm lhs expression
                [ (inject .) . Statement.Assignment [] <$ symbol AnonEqual
                , assign Expression.Plus            <$ symbol AnonPlusEqual
                , assign Expression.Minus           <$ symbol AnonMinusEqual
                , assign Expression.Times           <$ symbol AnonStarEqual
                , assign Expression.DividedBy       <$ symbol AnonSlashEqual
                , assign Expression.BOr             <$ symbol AnonPipeEqual
                , assign Expression.BAnd            <$ symbol AnonAmpersandEqual
                , assign Expression.Modulo          <$ symbol AnonPercentEqual
                , assign Expression.RShift          <$ symbol AnonRAngleRAngleEqual
                , assign Expression.UnsignedRShift  <$ symbol AnonRAngleRAngleRAngleEqual
                , assign Expression.LShift          <$ symbol AnonLAngleLAngleEqual
                , assign Expression.BXOr            <$ symbol AnonCaretEqual
                ])
  where
    assign :: (f :< Syntax) => (Term -> Term -> f Term) -> Term -> Term -> Sum Syntax Term
    assign c l r = inject (Statement.Assignment [] l (makeTerm1 (c l r)))
    lhs = symbol Lhs *> children (term expression)

data UnaryType
  = UPlus
  | UMinus
  | UBang
  | UTilde

unary :: Assignment Term
unary = make <$> symbol UnaryExpression <*> children ((,) <$> operator <*> term expression)
  where
    make _ (UPlus, operand) = operand
    make loc (UMinus, operand) = makeTerm loc (Expression.Negate operand)
    make loc (UBang, operand) = makeTerm loc (Expression.Not operand)
    make loc (UTilde, operand) = makeTerm loc (Expression.Complement operand)
    operator = token AnonPlus  $> UPlus
           <|> token AnonMinus $> UMinus
           <|> token AnonBang  $> UBang
           <|> token AnonTilde $> UTilde

update :: Assignment Term
update = makeTerm' <$> symbol UpdateExpression <*> children (
      inject . Statement.PreIncrement  <$ token AnonPlusPlus <*> term expression
  <|> inject . Statement.PreDecrement  <$ token AnonMinusMinus <*> term expression
  <|> inject . Statement.PostIncrement <$> term expression <* token AnonPlusPlus
  <|> inject . Statement.PostDecrement <$> term expression <* token AnonMinusMinus)

ternary :: Assignment Term
ternary = makeTerm <$> symbol TernaryExpression <*> children (Statement.If <$> term expression <*> term expression <*> term expression)

synchronized :: Assignment Term
synchronized = makeTerm <$> symbol SynchronizedStatement <*> children (Java.Syntax.Synchronized <$> term expression <*> term expression)

classInstance :: Assignment Term
classInstance = makeTerm <$> symbol ClassInstanceCreationExpression <*> children unqualified
  where
    unqualified = symbol UnqualifiedClassInstanceCreationExpression *> children (Java.Syntax.New <$> type' <*> (argumentList <|> pure []) <*> optional classBody)

classLiteral :: Assignment Term
classLiteral = makeTerm <$> symbol Grammar.ClassLiteral <*> children (Java.Syntax.ClassLiteral <$> type')

argumentList :: Assignment [Term]
argumentList = symbol ArgumentList *> children (manyTerm expression)
-- this takes care of expression, expression, ..., expression
-- but does this take care of parenthesized argumentList (it's a separate rule in TS)
-- I think methodReference being a top-level expression now will make manyTerm expression recognize this

super :: Assignment Term
super = makeTerm <$> token Super <*> pure Expression.Super
-- INCORRECT: super = makeTerm <$> token Super $> Expression.Super
-- Take partially applied function and replace it instead of applying

this :: Assignment Term
this = makeTerm <$> token This <*> pure Expression.This

constructorDeclaration :: Assignment Term
constructorDeclaration = makeTerm <$> symbol ConstructorDeclaration <*> children (
  constructor <$> manyTerm modifier <*> constructorDeclarator <*> (throws <|> pure []) <*> constructorBody)
    where
      constructorDeclarator = symbol ConstructorDeclarator *> children ((,,) <$> (typeParameters <|> pure []) <*> term identifier <*> formalParameters)
      constructorBody = makeTerm <$> symbol ConstructorBody <*> children (manyTerm expression) -- wrapping list of terms up in single node
      constructor modifiers (typeParameters, identifier, formalParameters) = Java.Syntax.Constructor modifiers typeParameters identifier formalParameters -- let partial application do its thing

typeParameters :: Assignment [Term]
typeParameters = symbol TypeParameters *> children (manyTerm typeParam)
-- not making a term, just matching children and returning the whole list
-- unpacking the TypeParameters node
  where
    typeParam = makeTerm <$> symbol Grammar.TypeParameter <*> children (Java.Syntax.TypeParameter <$> manyTerm annotation <*> term identifier <*> (typeBound <|> pure []))
    typeBound = symbol TypeBound *> children (manyTerm type')


annotation :: Assignment Term
annotation = makeTerm <$> symbol NormalAnnotation <*> children (Java.Syntax.Annotation <$> term expression <*> (elementValuePairList <|> pure []))
         <|> makeTerm <$> symbol MarkerAnnotation <*> children (Java.Syntax.Annotation <$> term expression <*> pure [])
         <|> makeTerm <$> symbol SingleElementAnnotation <*> children (Java.Syntax.Annotation <$> term expression <*> (pure <$> term elementValue))
         where
           elementValuePairList = symbol ElementValuePairList *> children (manyTerm elementValuePair)
           elementValuePair = makeTerm <$> symbol ElementValuePair <*> children (Java.Syntax.AnnotationField <$> term expression <*> term elementValue)
           elementValue = symbol ElementValue *> children (term expression)

throws :: Assignment [Term]
throws = symbol Throws *> children (symbol ExceptionTypeList *> children(manyTerm type'))

formalParameters :: Assignment [Term]
formalParameters = manyTerm (parameter <|> spreadParameter)
  where
    parameter = makeTerm <$> symbol FormalParameter <*> children (makeAnnotation <$> manyTerm modifier <*> type' <* symbol VariableDeclaratorId <*> children identifier)
    makeAnnotation [] type' variableName = Type.Annotation variableName type'
    makeAnnotation modifiers type' variableName = Type.Annotation variableName (makeTerm1 (Java.Syntax.TypeWithModifiers modifiers type'))

castExpression :: Assignment Term
castExpression = makeTerm <$> symbol CastExpression <*> children (flip Type.Annotation <$> type' <*> term expression)

fieldAccess :: Assignment Term
fieldAccess = makeTerm <$> symbol FieldAccess <*> children (Expression.MemberAccess <$> term expression <*> identifier')

spreadParameter :: Assignment Term
spreadParameter = makeTerm <$> symbol Grammar.SpreadParameter <*> children (Java.Syntax.SpreadParameter <$> (makeSingleDecl <$> manyTerm modifier <*> type' <*> variableDeclarator))
  where
    variableDeclarator = symbol VariableDeclarator *> children ((,) <$> variableDeclaratorId <*> optional expression)
    makeSingleDecl modifiers type' (target, Nothing) = makeTerm1 (Java.Syntax.Variable modifiers type' target)
    makeSingleDecl modifiers type' (target, Just value) = makeTerm1 (Statement.Assignment [] (makeTerm1 (Java.Syntax.Variable modifiers type' target)) value)

arrayAccess :: Assignment Term
arrayAccess = makeTerm <$> symbol ArrayAccess <*> children (Expression.Subscript <$> term expression <*> manyTerm expression)

lambda :: Assignment Term
lambda = makeTerm <$> symbol LambdaExpression <*> children (Java.Syntax.Lambda <$> manyTerm expression <*> lambdaBody)
  where
    lambdaBody = makeTerm <$> symbol Grammar.LambdaBody <*> children (Java.Syntax.LambdaBody <$> manyTerm expression)
