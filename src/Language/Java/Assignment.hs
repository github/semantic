{-# LANGUAGE DataKinds, RankNTypes, TypeOperators, KindSignatures #-}
module Language.Java.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Assigning.Assignment hiding (Assignment, Error, while, try)
import Data.Abstract.FreeVariables
import Data.Functor (($>))
import Data.List.NonEmpty (some1)
import Data.Record
import Data.Syntax (contextualize, emptyTerm, handleError, infixContext, makeTerm, makeTerm', makeTerm'', makeTerm1, parseError, postContextualize)
import Data.Sum
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
import Prologue hiding (for, try, This)
import GHC.TypeLits -- this is just to make sense of the Data kind (len :: Nat) example

-- data Vec a (len :: Nat)

type Syntax =
  '[ Comment.Comment
   , Declaration.Class
   , Declaration.InterfaceDeclaration
   , Declaration.Method
   , Declaration.VariableDeclaration
   , Expression.Arithmetic
   , Expression.Call
   , Expression.Comparison
   , Expression.Bitwise
   , Expression.Boolean
   , Expression.InstanceOf
   , Expression.MemberAccess
   , Expression.Super
   , Expression.This
   , Java.Syntax.Annotation
   , Java.Syntax.AnnotationField
   , Java.Syntax.Asterisk
   , Java.Syntax.Constructor
   , Java.Syntax.EnumDeclaration
   , Java.Syntax.GenericType
   , Java.Syntax.Import
   , Java.Syntax.Module
   , Java.Syntax.New
   , Java.Syntax.Package
   , Java.Syntax.Synchronized
   , Java.Syntax.TypeParameter
   , Java.Syntax.TypeWithModifiers
   , Java.Syntax.Variable
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
   , Statement.Throw
   , Statement.Try
   , Syntax.Context
   , Syntax.Empty
   , Syntax.Error
   , Syntax.Identifier
   , Syntax.AccessibilityModifier
   , Syntax.Program
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
type Assignment = HasCallStack => Assignment.Assignment [] Grammar Term

-- | Assignment from AST in Java's grammar onto a program in Java's syntax.
assignment :: Assignment
assignment = handleError $ makeTerm <$> symbol Grammar.Program <*> children (Syntax.Program <$> manyTerm expression) <|> parseError

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
manyTerm term = many (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

-- | Match a series of terms or comments until a delimiter is matched.
manyTermsTill :: Assignment.Assignment [] Grammar Term
              -> Assignment.Assignment [] Grammar b
              -> Assignment.Assignment [] Grammar [Term]
manyTermsTill step end = manyTill (step <|> comment) end

someTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
someTerm term = some (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

-- | Match comments before and after the node.
term :: Assignment -> Assignment
term term = contextualize comment (postContextualize comment term)

-- | Match
expression :: Assignment
expression = handleError (choice expressionChoices)

expressions :: Assignment
expressions = makeTerm'' <$> location <*> many expression

expressionChoices :: [Assignment.Assignment [] Grammar Term]
expressionChoices =
  [
    arrayInitializer
  , assignment'
  , block
  , binary
  , boolean
  , break
  , castExpression
  , char
  , class'
  , classInstance
  , continue
  , constructorDeclaration
  , explicitConstructorInvocation
  -- , constantDeclaration
  , doWhile
  , fieldDeclaration
  , float
  , for
  , enum
  -- , hexadecimal
  , if'
  , interface
  , identifier
  , import'
  , integer
  , method
  , methodInvocation
  , module'
  , null'
  , package
  , return'
  , scopedIdentifier
  , string
  , super
  , switch
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

modifier :: Assignment
modifier = make <$> symbol Modifier <*> children(Left <$> annotation <|> Right . Syntax.AccessibilityModifier <$> source)
  where
    make loc (Right modifier) = makeTerm loc modifier
    make _ (Left annotation) = annotation

arrayInitializer :: Assignment
arrayInitializer = makeTerm <$> symbol ArrayInitializer <*> (Literal.Array <$> many expression)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

localVariableDeclaration :: Assignment
localVariableDeclaration = makeTerm <$> symbol LocalVariableDeclaration <*> children ((,) <$> manyTerm modifier <*> type' <**> variableDeclaratorList)

variableDeclaratorList :: Assignment.Assignment [] Grammar (([Term], Term) -> [Term])
variableDeclaratorList = symbol VariableDeclaratorList *> children (makeDecl <$> some variableDeclarator)
  where
    variableDeclarator = symbol VariableDeclarator *> children ((,) <$> variableDeclaratorId <*> optional expression)
    makeDecl decls (modifiers, type') = map (makeSingleDecl modifiers type') decls
    makeSingleDecl modifiers type' (target, Nothing) = makeTerm1 (Java.Syntax.Variable modifiers type' target)
    makeSingleDecl modifiers type' (target, Just value) = makeTerm1 (Statement.Assignment [] (makeTerm1 (Java.Syntax.Variable modifiers type' target)) value)

localVariableDeclarationStatement :: Assignment
localVariableDeclarationStatement = symbol LocalVariableDeclarationStatement *> children localVariableDeclaration

variableDeclaratorId :: Assignment
variableDeclaratorId = symbol VariableDeclaratorId *> children identifier

-- Literals
boolean :: Assignment
boolean =  makeTerm <$> symbol BooleanLiteral <*> children
          (token Grammar.True $> Literal.true
          <|> token Grammar.False $> Literal.false)

null' :: Assignment
null' = makeTerm <$> symbol NullLiteral <*> (Literal.Null <$ source)

-- Integer supports all integer and floating point literals (hex, octal, binary)
integer :: Assignment
integer = makeTerm <$> symbol IntegerLiteral <*> children (Literal.Integer <$> source)

float :: Assignment
float = makeTerm <$> symbol FloatingPointLiteral <*> children (Literal.Float <$> source)

string :: Assignment
string = makeTerm <$> symbol StringLiteral <*> (Literal.TextElement <$> source)

char :: Assignment
char = makeTerm <$> symbol CharacterLiteral <*> (Literal.TextElement <$> source)

-- Identifiers
identifier :: Assignment
identifier = makeTerm <$> (symbol Identifier <|> symbol TypeIdentifier) <*> (Syntax.Identifier . name <$> source)

scopedIdentifier :: Assignment
scopedIdentifier = makeTerm <$> symbol ScopedIdentifier <*> children (Expression.MemberAccess <$> term expression <*> term expression)

superInterfaces :: Assignment.Assignment [] Grammar [Term]
superInterfaces = symbol SuperInterfaces *> children (symbol InterfaceTypeList *> children(manyTerm type'))
-- a *> b
-- both of these are impure
-- getLine *> getLine
-- in half apply, they're both monadic impure actions
-- :t (<$)
-- :t (*>)

-- what does it mean to say monadic action? more precise term: sequence-able
-- a sequence of applicative actions can be executed left to right
-- applicative computations can't do branch and control flow; applicative computations can only compute in a direct line, monadic can compute arbitrary branches

-- Declarations
class' :: Assignment
class' = makeTerm <$> symbol ClassDeclaration <*> children (makeClass <$> many modifier <*> term identifier <*> (typeParameters <|> pure []) <*> optional superClass <*> (superInterfaces <|> pure []) <*> classBody)
  where
    makeClass modifiers identifier typeParams superClass superInterfaces classBody = Declaration.Class (modifiers ++ typeParams) identifier (maybeToList superClass ++ superInterfaces) classBody -- not doing an assignment, just straight up function
    classBody = makeTerm <$> symbol ClassBody <*> children (manyTerm expression)
    superClass = symbol Superclass *> children type'
    -- matching term expression won't work since there is no node for that; it's AnonExtends
    -- superClass = makeTerm <$> symbol SuperClass <*> children (Java.Syntax.SuperClass <$> term expression <*> type')
    -- We'd still like to match the SuperClass node, but we don't need to create a syntax to make a term
    -- Do you lose info by omitting the superclass term? No...
    -- Don't need to make a term since we're not using syntax
    -- what's the difference between using tokens: AnonExtends GenericType?
    -- optional: when something can or can't exist and you want to produce a Maybe
-- TODO: superclass -- need to match the superclass node when it exists (which will be a rule, similar to how the type params rule matches the typeparams node when it exists)
-- optional, when we have a single term
-- superInterfaces is also optional but since it produces a list, lists already have an empty value so we don't need to wrap it up in a maybe to get an empty value

-- define this at the top level, we may change TS grammar so that if someone wants to write a Java snippet we could assign
-- it correctly; fieldDeclaration is standalone (compared to a type, which doesn't say anything by itself)
fieldDeclaration :: Assignment
fieldDeclaration = makeTerm <$> symbol FieldDeclaration <*> children ((,) <$> manyTerm modifier <*> type' <**> variableDeclaratorList)


method :: Assignment
method = makeTerm <$> symbol MethodDeclaration <*> children (makeMethod <$> many modifier <*> emptyTerm <*> methodHeader <*> methodBody)
  where
    methodBody = symbol MethodBody *> children (term expression <|> emptyTerm)
    methodDeclarator = symbol MethodDeclarator *> children ( (,) <$> identifier <*> formalParameters)
    methodHeader = symbol MethodHeader *> children ((,,,,) <$> (typeParameters <|> pure []) <*> manyTerm annotation <*> type' <*> methodDeclarator <*> (throws <|> pure []))
    makeMethod modifiers receiver (typeParams, annotations, returnType, (name, params), throws) body = Declaration.Method (returnType : modifiers ++ typeParams ++ annotations ++ throws) receiver name params body

-- TODO: add genericType
-- Question: should this genericType be part of type or not? Its own type because it's different structurally

generic :: Assignment
generic = makeTerm <$> symbol Grammar.GenericType <*> children(Java.Syntax.GenericType <$> term type' <*> manyTerm type')
-- when do we make a term again? - if we want to wrap something in a syntax constructor, because each piece of syntax
-- will be populated by further terms inside it. in this case, we wrap two terms in a piece of syntax.
-- Q to help decide: do we lose anything by omitting the term?

methodInvocation :: Assignment
methodInvocation = makeTerm <$> symbol MethodInvocation <*> children (Expression.Call [] <$> (callFunction <$> term expression <*> optional (token AnonDot *> term expression)) <*> argumentList <*> emptyTerm)
  where
    callFunction a (Just b) = makeTerm1 (Expression.MemberAccess a b)
    callFunction a Nothing = a

explicitConstructorInvocation :: Assignment
explicitConstructorInvocation = makeTerm <$> symbol ExplicitConstructorInvocation <*> children (Expression.Call [] <$> (callFunction <$> term expression <*> optional (token AnonDot *> term expression)) <*> argumentList <*> emptyTerm)
  where
    callFunction a (Just b) = makeTerm1 (Expression.MemberAccess a b)
    callFunction a Nothing = a

module' :: Assignment
module' = makeTerm <$> symbol ModuleDeclaration <*> children (Java.Syntax.Module <$> expression <*> many expression)

import' :: Assignment
import' = makeTerm <$> symbol ImportDeclaration <*> children (Java.Syntax.Import <$> someTerm (expression <|> asterisk))
  where asterisk = makeTerm <$> token Grammar.Asterisk <*> pure Java.Syntax.Asterisk

interface :: Assignment
interface = makeTerm <$> symbol InterfaceDeclaration <*> children (normal <|> annotationType)
  where
    interfaceBody = makeTerm <$> symbol InterfaceBody <*> children (manyTerm interfaceMemberDeclaration)
    normal = symbol NormalInterfaceDeclaration *> children (makeInterface <$> manyTerm modifier <*> identifier <*> (typeParameters <|> pure []) <*> interfaceBody)
    makeInterface modifiers identifier typeParams interfaceBody = Declaration.InterfaceDeclaration (modifiers ++ typeParams) identifier interfaceBody
    annotationType = symbol AnnotationTypeDeclaration *> children (Declaration.InterfaceDeclaration [] <$> identifier <*> annotationTypeBody)
    annotationTypeBody = makeTerm <$> symbol AnnotationTypeBody <*> children (many expression)
    interfaceMemberDeclaration = symbol InterfaceMemberDeclaration *> children (term expression)
    -- we won't make a term because we have a choice of a bunch of things

package :: Assignment
package = makeTerm <$> symbol PackageDeclaration <*> children (Java.Syntax.Package <$> someTerm expression)

enum :: Assignment
enum = makeTerm <$> symbol Grammar.EnumDeclaration <*> children (Java.Syntax.EnumDeclaration <$> term identifier <*> manyTerm enumConstant)
    where enumConstant = symbol EnumConstant *> children (term identifier)

return' :: Assignment
return' = makeTerm <$> symbol ReturnStatement <*> (Statement.Return <$> children (expression))

-- method expressions
dims :: Assignment.Assignment [] Grammar [Term]
dims = symbol Dims *> children (many (emptyTerm <* token AnonLBracket <* token AnonRBracket))

type' :: Assignment
type' =  choice [
       makeTerm <$> token VoidType <*> pure Type.Void
     , makeTerm <$> token IntegralType <*> pure Type.Int
     , makeTerm <$> token FloatingPointType <*> pure Type.Float
     , makeTerm <$> token BooleanType <*> pure Type.Bool
     , symbol ArrayType *> children (array <$> type' <*> dims) -- type rule recurs into itself
     , symbol CatchType *> children (term type')
     , symbol ExceptionType *> children (term type')
     , symbol TypeArgument *> children (term type')
     -- , symbol WildCard *> children (term type')
     , identifier
     , generic
    ]
    where array type' = foldl (\into each -> makeTerm1 (Type.Array (Just each) into)) type'

if' :: Assignment
if' = makeTerm <$> symbol IfThenElseStatement <*> children (Statement.If <$> term expression <*> term expression <*> (term expression <|> emptyTerm))

block :: Assignment
block = makeTerm <$> symbol Block <*> children (manyTerm expression)

while :: Assignment
while = makeTerm <$> symbol WhileStatement <*> children (Statement.While <$> term expression <*> term expression)

doWhile :: Assignment
doWhile = makeTerm <$> symbol DoStatement <*> children (flip Statement.DoWhile <$> term expression <*> term expression)

switch :: Assignment
switch = makeTerm <$> symbol SwitchStatement <*> children (Statement.Match <$> term expression <*> switchBlock)
  where
    switchBlock = makeTerm <$> symbol SwitchBlock <*> children (manyTerm switchLabel)
    switchLabel = makeTerm <$> symbol SwitchLabel <*> (Statement.Pattern <$> children (term expression <|> emptyTerm) <*> expressions)

break :: Assignment
break = makeTerm <$> symbol BreakStatement <*> children (Statement.Break <$> (term expression <|> emptyTerm))

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

for :: Assignment
for = symbol ForStatement *> children (basicFor <|> enhancedFor)

basicFor :: Assignment
basicFor = makeTerm <$> symbol BasicForStatement <*> children (Statement.For <$ token AnonFor <* token AnonLParen <*> (token AnonSemicolon *> emptyTerm <|> forInit <* token AnonSemicolon) <*> (token AnonSemicolon *> emptyTerm <|> term expression <* token AnonSemicolon) <*> forStep <*> term expression)
  where
    forInit = symbol ForInit *> children (term expression)
    forStep = makeTerm <$> location <*> manyTermsTill expression (token AnonRParen)

enhancedFor :: Assignment
enhancedFor = makeTerm <$> symbol EnhancedForStatement <*> children (Statement.ForEach <$> (variable <$> manyTerm modifier <*> type' <*> variableDeclaratorId) <*> term expression <*> term expression)
  where variable modifiers type' variableDeclaratorId = makeTerm1 (Java.Syntax.Variable modifiers type' variableDeclaratorId)

-- TODO: instanceOf
binary :: Assignment
binary = makeTerm' <$> symbol BinaryExpression <*> children (infixTerm expression expression
  [ (injectSum .) . Expression.LessThan         <$ symbol AnonLAngle
  , (injectSum .) . Expression.GreaterThan      <$ symbol AnonRAngle
  , (injectSum .) . Expression.Equal            <$ symbol AnonEqualEqual
  , (injectSum .) . Expression.GreaterThanEqual <$ symbol AnonRAngleEqual
  , (injectSum .) . Expression.LessThanEqual    <$ symbol AnonLAngleEqual
  , (injectSum .) . invert Expression.Equal     <$ symbol AnonBangEqual
  , (injectSum .) . Expression.And              <$ symbol AnonAmpersandAmpersand
  , (injectSum .) . Expression.Or               <$ symbol AnonPipePipe
  , (injectSum .) . Expression.BAnd             <$ symbol AnonAmpersand
  , (injectSum .) . Expression.BOr              <$ symbol AnonPipe
  , (injectSum .) . Expression.BXOr             <$ symbol AnonCaret
  , (injectSum .) . Expression.Modulo           <$ symbol AnonPercent
  , (injectSum .) . Expression.LShift           <$ symbol AnonLAngleLAngle
  , (injectSum .) . Expression.RShift           <$ symbol AnonRAngleRAngle
  , (injectSum .) . Expression.UnsignedRShift   <$ symbol AnonRAngleRAngleRAngle
  , (injectSum .) . Expression.Plus             <$ symbol AnonPlus
  , (injectSum .) . Expression.Minus            <$ symbol AnonMinus
  , (injectSum .) . Expression.Times            <$ symbol AnonStar
  , (injectSum .) . Expression.DividedBy        <$ symbol AnonSlash
  , (injectSum .) . Expression.InstanceOf       <$ symbol AnonInstanceof
  ])
  where invert cons a b = Expression.Not (makeTerm1 (cons a b))

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: HasCallStack
          => Assignment
          -> Assignment
          -> [Assignment.Assignment [] Grammar (Term -> Term -> Sum Syntax Term)]
          -> Assignment.Assignment [] Grammar (Sum Syntax Term)
infixTerm = infixContext comment

assignment' :: Assignment
assignment' = makeTerm' <$> symbol AssignmentExpression <*> children (infixTerm lhs expression
                [ (injectSum .) . Statement.Assignment [] <$ symbol AnonEqual
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
    assign c l r = injectSum (Statement.Assignment [] l (makeTerm1 (c l r)))
    lhs = symbol Lhs *> children (term expression)

data UnaryType
  = UPlus
  | UMinus
  | UBang
  | UTilde

unary :: Assignment
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

update :: Assignment
update = makeTerm' <$> symbol UpdateExpression <*> children (
      injectSum . Statement.PreIncrement  <$ token AnonPlusPlus <*> term expression
  <|> injectSum . Statement.PreDecrement  <$ token AnonMinusMinus <*> term expression
  <|> injectSum . Statement.PostIncrement <$> term expression <* token AnonPlusPlus
  <|> injectSum . Statement.PostDecrement <$> term expression <* token AnonMinusMinus)

ternary :: Assignment
ternary = makeTerm <$> symbol TernaryExpression <*> children (Statement.If <$> term expression <*> term expression <*> term expression)

synchronized :: Assignment
synchronized = makeTerm <$> symbol SynchronizedStatement <*> children (Java.Syntax.Synchronized <$> term expression <*> term expression)

classInstance :: Assignment
classInstance = makeTerm <$> symbol ClassInstanceCreationExpression <*> children unqualified
  where
    unqualified = symbol UnqualifiedClassInstanceCreationExpression *> children (Java.Syntax.New <$> type' <*> (argumentList <|> pure []))

argumentList :: Assignment.Assignment [] Grammar [Term]
argumentList = symbol ArgumentList *> children (manyTerm expression)

super :: Assignment
super = makeTerm <$> token Super <*> pure Expression.Super

this :: Assignment
this = makeTerm <$> token This <*> pure Expression.This

constructorDeclaration :: Assignment
constructorDeclaration = makeTerm <$> symbol ConstructorDeclaration <*> children (
  constructor <$> manyTerm modifier <*> constructorDeclarator <*> (throws <|> pure []) <*> constructorBody)
    where
      constructorDeclarator = symbol ConstructorDeclarator *> children ((,,) <$> (typeParameters <|> pure []) <*> term identifier <*> formalParameters)
      constructorBody = makeTerm <$> symbol ConstructorBody <*> children (manyTerm expression) -- wrapping list of terms up in single node
      constructor modifiers (typeParameters, identifier, formalParameters) = Java.Syntax.Constructor modifiers typeParameters identifier formalParameters -- let partial application do its thing

typeParameters :: Assignment.Assignment [] Grammar [Term]
typeParameters = symbol TypeParameters *> children (manyTerm typeParam) -- this produces a list, which is what we need to return given by the type definition
  where
    typeParam = makeTerm <$> symbol Grammar.TypeParameter <*> children (Java.Syntax.TypeParameter <$> manyTerm annotation <*> term identifier <*> (typeBound <|> pure [])) -- wrapping up all three of those fields so we need to makeTerm (producing a term here)
    typeBound = symbol TypeBound *> children (manyTerm type')
    -- manyTerm typeParam made sense because each type Parameter was wrapped up into a Grammar.TypeParameter node, dissimilar
    -- to superInterfaces

annotation :: Assignment
annotation = makeTerm <$> symbol NormalAnnotation <*> children (Java.Syntax.Annotation <$> term expression <*> (elementValuePairList <|> pure []))
         <|> makeTerm <$> symbol MarkerAnnotation <*> children (Java.Syntax.Annotation <$> term expression <*> pure [])
         <|> makeTerm <$> symbol SingleElementAnnotation <*> children (Java.Syntax.Annotation <$> term expression <*> (pure <$> term elementValue))
         where
           elementValuePairList = symbol ElementValuePairList *> children (manyTerm elementValuePair)
           elementValuePair = makeTerm <$> symbol ElementValuePair <*> children (Java.Syntax.AnnotationField <$> term expression <*> term elementValue)
           elementValue = symbol ElementValue *> children (term expression)

throws :: Assignment.Assignment [] Grammar [Term]
throws = symbol Throws *> children (symbol ExceptionTypeList *> children(manyTerm type'))

formalParameters :: Assignment.Assignment [] Grammar [Term]
formalParameters = manyTerm parameter
  where
    parameter = makeTerm <$> symbol FormalParameter <*> children (makeAnnotation <$> manyTerm modifier <*> type' <* symbol VariableDeclaratorId <*> children identifier)
    makeAnnotation [] type' variableName = Type.Annotation variableName type'
    makeAnnotation modifiers type' variableName = Type.Annotation variableName (makeTerm1 (Java.Syntax.TypeWithModifiers modifiers type'))
-- know when we are in a functor context and fmap is all gravy
-- we're just wrapping stuff up in data, we aren't building a pattern (assignment) so we aren't in an applicative context
-- when in an applicative context, we're also in a functor context (ie., defining how fmap will work over it)
-- sometimes it is nice to be able to say you're in an applicative context without refering to any particular applicative instance

-- constantDeclaration :: Assignment
-- constantDeclaration = makeTerm <$> symbol ConstantDeclaration <*>

castExpression :: Assignment
castExpression = makeTerm <$> symbol CastExpression <*> children (flip Type.Annotation <$> type' <*> term expression)
-- term expression, because we can deal with comments
