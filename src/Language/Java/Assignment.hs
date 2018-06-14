{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.Java.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Assigning.Assignment hiding (Assignment, Error, while, try)
import Data.Abstract.Name
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
   , Expression.Subscript
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
   , Java.Syntax.SpreadParameter
   , Java.Syntax.Synchronized
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
type Assignment = HasCallStack => Assignment.Assignment [] Grammar Term

-- | Assignment from AST in Java's grammar onto a program in Java's syntax.
assignment :: Assignment
assignment = handleError $ makeTerm <$> symbol Grammar.Program <*> children (Statement.Statements <$> manyTerm expression) <|> parseError

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
manyTerm term = many (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

-- | Match a series of terms or comments until a delimiter is matched.
manyTermsTill :: Assignment.Assignment [] Grammar Term
              -> Assignment.Assignment [] Grammar b
              -> Assignment.Assignment [] Grammar [Term]
manyTermsTill step = manyTill (step <|> comment)

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
  , arrayAccess
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
boolean = toTerm (branchNode BooleanLiteral
  (   leafNode Grammar.True  $> Literal.true
  <|> leafNode Grammar.False $> Literal.false))

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

identifier' :: Assignment.Assignment [] Grammar Name
identifier' = (symbol Identifier <|> symbol TypeIdentifier) *> (name <$> source)

scopedIdentifier :: Assignment
scopedIdentifier = makeTerm <$> symbol ScopedIdentifier <*> children (Expression.MemberAccess <$> term expression <*> identifier')

superInterfaces :: Assignment.Assignment [] Grammar [Term]
superInterfaces = symbol SuperInterfaces *> children (symbol InterfaceTypeList *> children(manyTerm type'))

-- Declarations
class' :: Assignment
class' = makeTerm <$> symbol ClassDeclaration <*> children (makeClass <$> many modifier <*> term identifier <*> (typeParameters <|> pure []) <*> optional superClass <*> (superInterfaces <|> pure []) <*> classBody)
  where
    makeClass modifiers identifier typeParams superClass superInterfaces = Declaration.Class (modifiers ++ typeParams) identifier (maybeToList superClass ++ superInterfaces) -- not doing an assignment, just straight up function
    classBody = makeTerm <$> symbol ClassBody <*> children (manyTerm expression)
    superClass = symbol Superclass *> children type'
-- TODO: superclass
--       need to match the superclass node when it exists (which will be a rule, similar to how the type params rule matches the typeparams node when it exists)
--       optional, when we have a single term
--       superInterfaces is also optional but since it produces a list, lists already have an empty value so we don't need to wrap it up in a maybe to get an empty value

fieldDeclaration :: Assignment
fieldDeclaration = makeTerm <$> symbol FieldDeclaration <*> children ((,) <$> manyTerm modifier <*> type' <**> variableDeclaratorList)

method :: Assignment
method = makeTerm <$> symbol MethodDeclaration <*> children (makeMethod <$> many modifier <*> emptyTerm <*> methodHeader <*> methodBody)
  where
    methodBody = symbol MethodBody *> children (term expression <|> emptyTerm)
    methodDeclarator = symbol MethodDeclarator *> children ( (,) <$> identifier <*> formalParameters)
    methodHeader = symbol MethodHeader *> children ((,,,,) <$> (typeParameters <|> pure []) <*> manyTerm annotation <*> type' <*> methodDeclarator <*> (throws <|> pure []))
    makeMethod modifiers receiver (typeParams, annotations, returnType, (name, params), throws) = Declaration.Method (returnType : modifiers ++ typeParams ++ annotations ++ throws) receiver name params
-- methodHeader needs to include typeParameters (it does)

generic :: Assignment
generic = makeTerm <$> symbol Grammar.GenericType <*> children(Java.Syntax.GenericType <$> term type' <*> manyTerm type')

methodInvocation :: Assignment
methodInvocation = makeTerm <$> symbol MethodInvocation <*> children (uncurry Expression.Call <$> (callFunction <$> expression <*> optional ((,) <$ token AnonDot <*> manyTerm typeArgument <*> identifier')) <*> (argumentList <|> pure []) <*> emptyTerm)
  where
    callFunction a (Just (typeArguments, b)) = (typeArguments, makeTerm1 (Expression.MemberAccess a b))
    callFunction a Nothing = ([], a)

explicitConstructorInvocation :: Assignment
explicitConstructorInvocation = makeTerm <$> symbol ExplicitConstructorInvocation <*> children (uncurry Expression.Call <$> (callFunction <$> term expression <*> optional ((,) <$ token AnonDot <*> manyTerm type' <*> identifier')) <*> argumentList <*> emptyTerm)
  where
    callFunction a (Just (typeArguments, b)) = (typeArguments, makeTerm1 (Expression.MemberAccess a b))
    callFunction a Nothing = ([], a)

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
    makeInterface modifiers identifier typeParams = Declaration.InterfaceDeclaration (modifiers ++ typeParams) identifier
    annotationType = symbol AnnotationTypeDeclaration *> children (Declaration.InterfaceDeclaration [] <$> identifier <*> annotationTypeBody)
    annotationTypeBody = makeTerm <$> symbol AnnotationTypeBody <*> children (many expression)
    interfaceMemberDeclaration = symbol InterfaceMemberDeclaration *> children (term expression)

package :: Assignment
package = makeTerm <$> symbol PackageDeclaration <*> children (Java.Syntax.Package <$> someTerm expression)

enum :: Assignment
enum = makeTerm <$> symbol Grammar.EnumDeclaration <*> children (Java.Syntax.EnumDeclaration <$> manyTerm modifier <*> term identifier <*> (superInterfaces <|> pure []) <*> manyTerm enumConstant <*> (enumBodyDeclarations <|> pure []))
    where
      enumConstant = symbol EnumConstant *> children (term identifier)
      enumBodyDeclarations = symbol EnumBodyDeclarations *> children (manyTerm expression)

return' :: Assignment
return' = makeTerm <$> symbol ReturnStatement <*> (Statement.Return <$> children (expression <|> emptyTerm))

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
     , wildcard
     , identifier
     , generic
     , typeArgument
    ]
    where array = foldl (\into each -> makeTerm1 (Type.Array (Just each) into))

typeArgument :: Assignment
typeArgument = symbol TypeArgument *> children (term type')

wildcard :: Assignment
wildcard = makeTerm <$> symbol Grammar.Wildcard <*> children (Java.Syntax.Wildcard <$> manyTerm annotation <*> optional (super <|> extends))
  where
    super = makeTerm <$> token Super <*> (Java.Syntax.WildcardBoundSuper <$> type')
    extends = makeTerm1 <$> (Java.Syntax.WildcardBoundExtends <$> type')

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
      inject . Statement.PreIncrement  <$ token AnonPlusPlus <*> term expression
  <|> inject . Statement.PreDecrement  <$ token AnonMinusMinus <*> term expression
  <|> inject . Statement.PostIncrement <$> term expression <* token AnonPlusPlus
  <|> inject . Statement.PostDecrement <$> term expression <* token AnonMinusMinus)

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
-- INCORRECT: super = makeTerm <$> token Super $> Expression.Super
-- Take partially applied function and replace it instead of applying

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
typeParameters = symbol TypeParameters *> children (manyTerm typeParam)
  where
    typeParam = makeTerm <$> symbol Grammar.TypeParameter <*> children (Java.Syntax.TypeParameter <$> manyTerm annotation <*> term identifier <*> (typeBound <|> pure []))
    typeBound = symbol TypeBound *> children (manyTerm type')

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
formalParameters = manyTerm (parameter <|> spreadParameter)
  where
    parameter = makeTerm <$> symbol FormalParameter <*> children (makeAnnotation <$> manyTerm modifier <*> type' <* symbol VariableDeclaratorId <*> children identifier)
    makeAnnotation [] type' variableName = Type.Annotation variableName type'
    makeAnnotation modifiers type' variableName = Type.Annotation variableName (makeTerm1 (Java.Syntax.TypeWithModifiers modifiers type'))

castExpression :: Assignment
castExpression = makeTerm <$> symbol CastExpression <*> children (flip Type.Annotation <$> type' <*> term expression)

fieldAccess :: Assignment
fieldAccess = makeTerm <$> symbol FieldAccess <*> children (Expression.MemberAccess <$> term expression <*> identifier')

spreadParameter :: Assignment
spreadParameter = makeTerm <$> symbol Grammar.SpreadParameter <*> children (Java.Syntax.SpreadParameter <$> (makeSingleDecl <$> manyTerm modifier <*> type' <*> variableDeclarator))
  where
    variableDeclarator = symbol VariableDeclarator *> children ((,) <$> variableDeclaratorId <*> optional expression)
    makeSingleDecl modifiers type' (target, Nothing) = makeTerm1 (Java.Syntax.Variable modifiers type' target)
    makeSingleDecl modifiers type' (target, Just value) = makeTerm1 (Statement.Assignment [] (makeTerm1 (Java.Syntax.Variable modifiers type' target)) value)

arrayAccess :: Assignment
arrayAccess = makeTerm <$> symbol ArrayAccess <*> children (Expression.Subscript <$> term expression <*> manyTerm expression)
