{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.PHP.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Assigning.Assignment hiding (Assignment, Error)
import Data.Record
import Data.Sum
import Data.Syntax
    ( contextualize
    , emptyTerm
    , handleError
    , infixContext
    , makeTerm
    , makeTerm'
    , makeTerm1
    , parseError
    , postContextualize
    )
import Language.PHP.Grammar as Grammar
import qualified Assigning.Assignment as Assignment
import qualified Data.Abstract.Name as Name
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import qualified Data.Term as Term
import qualified Language.PHP.Syntax as Syntax
import Prologue

type Syntax = '[
    Comment.Comment
  , Declaration.Class
  , Declaration.Function
  , Declaration.Method
  , Declaration.VariableDeclaration
  , Expression.Arithmetic
  , Expression.BAnd
  , Expression.BOr
  , Expression.BXOr
  , Expression.LShift
  , Expression.RShift
  , Expression.And
  , Expression.Not
  , Expression.Or
  , Expression.XOr
  , Expression.Call
  , Expression.Cast
  , Expression.Comparison
  , Expression.InstanceOf
  , Expression.MemberAccess
  , Expression.New
  , Expression.SequenceExpression
  , Expression.Subscript
  , Literal.Array
  , Literal.Float
  , Literal.Integer
  , Literal.KeyValue
  , Literal.TextElement
  , Statement.Assignment
  , Statement.Break
  , Statement.Catch
  , Statement.Continue
  , Statement.DoWhile
  , Statement.Else
  , Statement.Finally
  , Statement.For
  , Statement.ForEach
  , Statement.Goto
  , Statement.If
  , Statement.If
  , Statement.Match
  , Statement.Pattern
  , Statement.Return
  , Statement.Statements
  , Statement.Throw
  , Statement.Try
  , Statement.While
  , Statement.Yield
  , Syntax.AliasAs
  , Syntax.ArrayElement
  , Syntax.BaseTypeDeclaration
  , Syntax.CastType
  , Syntax.ClassBaseClause
  , Syntax.ClassConstDeclaration
  , Syntax.ClassInterfaceClause
  , Syntax.ClassModifier
  , Syntax.Clone
  , Syntax.ConstDeclaration
  , Syntax.ConstructorDeclaration
  , Syntax.Context
  , Syntax.Declare
  , Syntax.DeclareDirective
  , Syntax.DestructorDeclaration
  , Syntax.Echo
  , Syntax.Empty
  , Syntax.EmptyIntrinsic
  , Syntax.Error
  , Syntax.ErrorControl
  , Syntax.EvalIntrinsic
  , Syntax.ExitIntrinsic
  , Syntax.GlobalDeclaration
  , Syntax.Identifier
  , Syntax.Include
  , Syntax.IncludeOnce
  , Syntax.InsteadOf
  , Syntax.InterfaceBaseClause
  , Syntax.InterfaceDeclaration
  , Syntax.IssetIntrinsic
  , Syntax.LabeledStatement
  , Syntax.Namespace
  , Syntax.NamespaceAliasingClause
  , Syntax.NamespaceName
  , Syntax.NamespaceUseClause
  , Syntax.NamespaceUseDeclaration
  , Syntax.NamespaceUseGroupClause
  , Syntax.NewVariable
  , Syntax.PrintIntrinsic
  , Syntax.PropertyDeclaration
  , Syntax.PropertyModifier
  , Syntax.QualifiedName
  , Syntax.RelativeScope
  , Syntax.Require
  , Syntax.RequireOnce
  , Syntax.ReturnType
  , Syntax.ScalarType
  , Syntax.ShellCommand
  , Syntax.SimpleVariable
  , Syntax.Static
  , Syntax.Text
  , Syntax.TraitDeclaration
  , Syntax.TraitUseClause
  , Syntax.TraitUseSpecification
  , Syntax.TypeDeclaration
  , Syntax.Unset
  , Syntax.Update
  , Syntax.UseClause
  , Syntax.VariableName
  , Type.Annotation
  , [] ]

type Term = Term.Term (Sum Syntax) (Record Location)
type Assignment = Assignment.Assignment [] Grammar Term

-- | Assignment from AST in PHP's grammar onto a program in PHP's syntax.
assignment :: Assignment
assignment = handleError $ makeTerm <$> symbol Program <*> children (Statement.Statements <$> (bookend <$> (text <|> emptyTerm) <*> manyTerm statement <*> (text <|> emptyTerm))) <|> parseError

text :: Assignment
text = makeTerm <$> symbol Text <*> (Syntax.Text <$> source)

textInterpolation :: Assignment
textInterpolation = makeTerm <$> symbol TextInterpolation <*> (Syntax.Text <$> source)

statement :: Assignment
statement = handleError everything
  where
    everything = choice [
      compoundStatement
      , namedLabelStatement
      , expressionStatement
      , selectionStatement
      , iterationStatement
      , jumpStatement
      , tryStatement
      , declareStatement
      , echoStatement
      , unsetStatement
      , constDeclaration
      , functionDefinition
      , classDeclaration
      , interfaceDeclaration
      , traitDeclaration
      , namespaceDefinition
      , namespaceUseDeclaration
      , globalDeclaration
      , functionStaticDeclaration
      ]

expression :: Assignment
expression = choice [
  assignmentExpression,
  augmentedAssignmentExpression,
  conditionalExpression,
  yieldExpression,
  includeExpression,
  includeOnceExpression,
  requireExpression,
  requireOnceExpression,
  binaryExpression,
  unaryExpression
  ]

unaryExpression :: Assignment
unaryExpression = choice [
  cloneExpression,
  exponentiationExpression,
  unaryOpExpression,
  castExpression,
  primaryExpression
  ]

assignmentExpression :: Assignment
assignmentExpression = makeTerm <$> symbol AssignmentExpression <*> children (Statement.Assignment [] <$> term (variable <|> list <|> arrayCreationExpression) <*> term (expression <|> variable))

augmentedAssignmentExpression :: Assignment
augmentedAssignmentExpression = makeTerm' <$> symbol AugmentedAssignmentExpression <*> children (infixTerm variable (term expression) [
  assign Expression.Power <$ symbol AnonStarStarEqual
  , assign Expression.Times <$ symbol AnonStarEqual
  , assign Expression.Modulo <$ symbol AnonPercentEqual
  , assign Expression.DividedBy <$ symbol AnonSlashEqual
  , assign Expression.Plus <$ symbol AnonPlusEqual
  , assign Expression.Minus <$ symbol AnonMinusEqual
  , assign Expression.Times <$ symbol AnonDotEqual
  , assign Expression.LShift <$ symbol AnonLAngleLAngleEqual
  , assign Expression.RShift <$ symbol AnonRAngleRAngleEqual
  , assign Expression.BAnd <$ symbol AnonAmpersandEqual
  , assign Expression.BXOr <$ symbol AnonCaretEqual
  , assign Expression.BOr <$ symbol AnonPipeEqual ])
  where
    assign c l r = inject (Statement.Assignment [] l (makeTerm1 (c l r)))

binaryExpression  :: Assignment
binaryExpression = makeTerm' <$> symbol BinaryExpression <*> children (infixTerm expression (term (expression <|> classTypeDesignator))
  [ (inject .) . Expression.And                <$ symbol AnonAnd
  , (inject .) . Expression.Or                 <$ symbol AnonOr
  , (inject .) . Expression.XOr                <$ symbol AnonXor
  , (inject .) . Expression.Or                 <$ symbol AnonPipePipe
  , (inject .) . Expression.And                <$ symbol AnonAmpersandAmpersand
  , (inject .) . Expression.BOr                <$ symbol AnonPipe
  , (inject .) . Expression.BXOr               <$ symbol AnonCaret
  , (inject .) . Expression.BAnd               <$ symbol AnonAmpersand
  , (inject .) . Expression.Or                 <$ symbol AnonQuestionQuestion -- Not sure if this is right.
  , (inject .) . Expression.Equal              <$ symbol AnonEqualEqual
  , (inject .) . Expression.StrictEqual        <$ symbol AnonEqualEqualEqual
  , (inject .) . invert Expression.Equal       <$ (symbol AnonBangEqual <|> symbol AnonLAngleRAngle <|> symbol AnonBangEqualEqual)
  , (inject .) . invert Expression.StrictEqual <$ symbol AnonBangEqualEqual
  , (inject .) . Expression.LessThan           <$ symbol AnonLAngle
  , (inject .) . Expression.GreaterThan        <$ symbol AnonRAngle
  , (inject .) . Expression.LessThanEqual      <$ symbol AnonLAngleEqual
  , (inject .) . Expression.GreaterThanEqual   <$ symbol AnonRAngleEqual
  , (inject .) . Expression.Comparison         <$ symbol AnonLAngleEqualRAngle
  , (inject .) . Expression.LShift             <$ symbol AnonLAngleLAngle
  , (inject .) . Expression.RShift             <$ symbol AnonRAngleRAngle
  , (inject .) . Expression.Plus               <$ symbol AnonPlus
  , (inject .) . Expression.Minus              <$ symbol AnonMinus
  , (inject .) . Expression.Times              <$ (symbol AnonStar <|> symbol AnonDot)
  , (inject .) . Expression.DividedBy          <$ symbol AnonSlash
  , (inject .) . Expression.Modulo             <$ symbol AnonPercent
  , (inject .) . Expression.InstanceOf         <$ symbol AnonInstanceof
  ]) where invert cons a b = Expression.Not (makeTerm1 (cons a b))

conditionalExpression :: Assignment
conditionalExpression = makeTerm <$> symbol ConditionalExpression <*> children (Statement.If <$> term (binaryExpression <|> unaryExpression) <*> (term expression <|> emptyTerm) <*> term expression)

list :: Assignment
list = makeTerm <$> symbol ListLiteral <*> children (Literal.Array <$> manyTerm (list <|> variable))

exponentiationExpression :: Assignment
exponentiationExpression = makeTerm <$> symbol ExponentiationExpression <*> children (Expression.Power <$> term (cloneExpression <|> primaryExpression) <*> term (primaryExpression <|> cloneExpression <|> exponentiationExpression))

cloneExpression :: Assignment
cloneExpression = makeTerm <$> symbol CloneExpression <*> children (Syntax.Clone <$> term primaryExpression)

primaryExpression :: Assignment
primaryExpression = choice [
  variable,
  classConstantAccessExpression,
  qualifiedName,
  literal,
  arrayCreationExpression,
  intrinsic,
  anonymousFunctionCreationExpression,
  objectCreationExpression,
  updateExpression,
  shellCommandExpression,
  parenthesizedExpression
  ]

parenthesizedExpression :: Assignment
parenthesizedExpression = symbol ParenthesizedExpression *> children (term expression)

classConstantAccessExpression :: Assignment
classConstantAccessExpression = makeTerm <$> symbol ClassConstantAccessExpression <*> children (Expression.MemberAccess <$> term scopeResolutionQualifier <*> name')

variable :: Assignment
variable = callableVariable <|> scopedPropertyAccessExpression <|> memberAccessExpression <|> castExpression

callableVariable :: Assignment
callableVariable = choice [
  simpleVariable',
  subscriptExpression,
  memberCallExpression,
  scopedCallExpression,
  functionCallExpression
  ]

memberCallExpression :: Assignment
memberCallExpression = makeTerm <$> symbol MemberCallExpression <*> children (Expression.Call [] <$> (makeMemberAccess <$> location <*> term dereferencableExpression <*> memberName') <*> arguments <*> emptyTerm)
  where makeMemberAccess loc expr memberName = makeTerm loc (Expression.MemberAccess expr memberName)

scopedCallExpression :: Assignment
scopedCallExpression = makeTerm <$> symbol ScopedCallExpression <*> children (Expression.Call [] <$> (makeMemberAccess <$> location <*> term scopeResolutionQualifier <*> memberName') <*> arguments <*> emptyTerm)
  where makeMemberAccess loc expr memberName = makeTerm loc (Expression.MemberAccess expr memberName)

functionCallExpression :: Assignment
functionCallExpression = makeTerm <$> symbol FunctionCallExpression <*> children (Expression.Call [] <$> term (qualifiedName <|> callableExpression) <*> arguments <*> emptyTerm)

callableExpression :: Assignment
callableExpression = choice [
  callableVariable,
  expression,
  arrayCreationExpression,
  string
  ]

subscriptExpression :: Assignment
subscriptExpression = makeTerm <$> symbol SubscriptExpression <*> children (Expression.Subscript <$> term dereferencableExpression <*> (pure <$> (term expression <|> emptyTerm)))

memberAccessExpression :: Assignment
memberAccessExpression = makeTerm <$> symbol MemberAccessExpression <*> children (Expression.MemberAccess <$> term dereferencableExpression <*> memberName')

dereferencableExpression :: Assignment
dereferencableExpression = symbol DereferencableExpression *> children (term (variable <|> expression <|> arrayCreationExpression <|> string))

scopedPropertyAccessExpression :: Assignment
scopedPropertyAccessExpression = makeTerm <$> symbol ScopedPropertyAccessExpression <*> children (Expression.MemberAccess <$> term scopeResolutionQualifier <*> simpleVariable'')

scopeResolutionQualifier :: Assignment
scopeResolutionQualifier = choice [
  relativeScope,
  qualifiedName,
  dereferencableExpression
  ]

arrayCreationExpression :: Assignment
arrayCreationExpression = makeTerm <$> symbol ArrayCreationExpression <*> children (Literal.Array <$> manyTerm arrayElementInitializer)

intrinsic :: Assignment
intrinsic = choice [
  emptyIntrinsic,
  evalIntrinsic,
  exitIntrinsic,
  issetIntrinsic,
  printIntrinsic
  ]

emptyIntrinsic :: Assignment
emptyIntrinsic = makeTerm <$> symbol EmptyIntrinsic <*> children (Syntax.EmptyIntrinsic <$> term expression)

evalIntrinsic :: Assignment
evalIntrinsic = makeTerm <$> symbol EvalIntrinsic <*> children (Syntax.EvalIntrinsic <$> term expression)

exitIntrinsic :: Assignment
exitIntrinsic = makeTerm <$> symbol ExitIntrinsic <*> children (Syntax.ExitIntrinsic <$> (term expression <|> emptyTerm))

issetIntrinsic :: Assignment
issetIntrinsic = makeTerm <$> symbol IssetIntrinsic <*> children (Syntax.IssetIntrinsic <$> (makeTerm <$> location <*> someTerm variable))

printIntrinsic :: Assignment
printIntrinsic = makeTerm <$> symbol PrintIntrinsic <*> children (Syntax.PrintIntrinsic <$> term expression)

anonymousFunctionCreationExpression :: Assignment
anonymousFunctionCreationExpression = makeTerm <$> symbol AnonymousFunctionCreationExpression <*> children (makeFunction <$> emptyTerm <*> parameters <*> (term functionUseClause <|> emptyTerm) <*> (term returnType <|> emptyTerm) <*> term compoundStatement)
  where
    makeFunction identifier parameters functionUseClause returnType statement = Declaration.Function [functionUseClause, returnType] identifier parameters statement

parameters :: Assignment.Assignment [] Grammar [Term]
parameters = manyTerm (simpleParameter <|> variadicParameter)

simpleParameter :: Assignment
simpleParameter = makeTerm <$> symbol SimpleParameter <*> children (makeAnnotation <$> (term typeDeclaration <|> emptyTerm) <*> (makeAssignment <$> location <*> term variableName <*> (term defaultArgumentSpecifier <|> emptyTerm)))
  where
    makeAnnotation typeDecl assignment = Type.Annotation assignment typeDecl
    makeAssignment loc name argument = makeTerm loc (Statement.Assignment [] name argument)

defaultArgumentSpecifier :: Assignment
defaultArgumentSpecifier = symbol DefaultArgumentSpecifier *> children (term expression)


variadicParameter :: Assignment
variadicParameter = makeTerm <$> symbol VariadicParameter <*> children (makeTypeAnnotation <$> (term typeDeclaration <|> emptyTerm) <*> term variableName)
  where makeTypeAnnotation ty variableName = Type.Annotation variableName ty

functionUseClause :: Assignment
functionUseClause = makeTerm <$> symbol AnonymousFunctionUseClause <*> children (Syntax.UseClause <$> someTerm variableName)

returnType :: Assignment
returnType = makeTerm <$> symbol ReturnType <*> children (Syntax.ReturnType <$> (term typeDeclaration <|> emptyTerm))

typeDeclaration :: Assignment
typeDeclaration = makeTerm <$> symbol TypeDeclaration <*> children (Syntax.TypeDeclaration <$> term baseTypeDeclaration)

baseTypeDeclaration :: Assignment
baseTypeDeclaration = makeTerm <$> symbol BaseTypeDeclaration <*> children (Syntax.BaseTypeDeclaration <$> term (scalarType <|> qualifiedName <|> emptyTerm))

scalarType :: Assignment
scalarType = makeTerm <$> symbol ScalarType <*> (Syntax.ScalarType <$> source)

compoundStatement :: Assignment
compoundStatement = makeTerm <$> symbol CompoundStatement <*> children (manyTerm statement)

objectCreationExpression :: Assignment
objectCreationExpression = (makeTerm <$> symbol ObjectCreationExpression <*> children (Expression.New <$> ((:) <$> term classTypeDesignator <*> (arguments <|> pure []))))

  <|> (makeTerm <$> symbol ObjectCreationExpression <*> children (makeAnonClass <$ token AnonNew <* token AnonClass <*> emptyTerm <*> (arguments <|> pure []) <*> (term classBaseClause <|> emptyTerm) <*> (term classInterfaceClause <|> emptyTerm) <*> (makeTerm <$> location <*> manyTerm classMemberDeclaration)))
  where makeAnonClass identifier args baseClause interfaceClause declarations = Declaration.Class [] identifier (args <> [baseClause, interfaceClause]) declarations

classMemberDeclaration :: Assignment
classMemberDeclaration = choice [
  classConstDeclaration,
  propertyDeclaration,
  methodDeclaration,
  constructorDeclaration,
  destructorDeclaration,
  traitUseClause
  ]

methodDeclaration :: Assignment
methodDeclaration =  (makeTerm <$> symbol MethodDeclaration <*> children (makeMethod1 <$> manyTerm methodModifier <*> emptyTerm <*> functionDefinitionParts)) <|> makeTerm <$> symbol MethodDeclaration <*> children (makeMethod2 <$> someTerm methodModifier <*> emptyTerm <*> term name <*> parameters <*> term (returnType <|> emptyTerm) <*> emptyTerm)
  where
    functionDefinitionParts = symbol FunctionDefinition *> children ((,,,) <$> term name <*> parameters <*> term (returnType <|> emptyTerm) <*> (term compoundStatement <|> emptyTerm))
    makeMethod1 modifiers receiver (name, params, returnType, compoundStatement) = Declaration.Method (modifiers <> [returnType]) receiver name params compoundStatement
    makeMethod2 modifiers receiver name params returnType compoundStatement = Declaration.Method (modifiers <> [returnType]) receiver name params compoundStatement

classBaseClause :: Assignment
classBaseClause = makeTerm <$> symbol ClassBaseClause <*> children (Syntax.ClassBaseClause <$> term qualifiedName)

classInterfaceClause :: Assignment
classInterfaceClause = makeTerm <$> symbol ClassInterfaceClause <*> children (Syntax.ClassInterfaceClause <$> someTerm qualifiedName)

classConstDeclaration :: Assignment
classConstDeclaration = makeTerm <$> symbol ClassConstDeclaration <*> children (Syntax.ClassConstDeclaration <$> (term visibilityModifier <|> emptyTerm) <*> manyTerm constElement)

visibilityModifier :: Assignment
visibilityModifier = makeTerm <$> symbol VisibilityModifier <*> (Syntax.Identifier . Name.name <$> source)

constElement :: Assignment
constElement = makeTerm <$> symbol ConstElement <*> children (Statement.Assignment [] <$> term name <*> term expression)

arguments :: Assignment.Assignment [] Grammar [Term]
arguments = symbol Arguments *> children (manyTerm (variadicUnpacking <|> expression))

variadicUnpacking :: Assignment
variadicUnpacking = symbol VariadicUnpacking *> children (term expression)

classTypeDesignator :: Assignment
classTypeDesignator = qualifiedName <|> newVariable

newVariable :: Assignment
newVariable = makeTerm <$> symbol NewVariable <*> children (Syntax.NewVariable <$> ((pure <$> term simpleVariable') <|> ((\a b -> [a, b]) <$> term (newVariable <|> qualifiedName <|> relativeScope) <*> term (expression <|> memberName <|> emptyTerm))))

memberName :: Assignment
memberName = name <|> simpleVariable' <|> expression

memberName' :: Assignment.Assignment [] Grammar Name.Name
memberName' = name' <|> simpleVariable''

relativeScope :: Assignment
relativeScope = makeTerm <$> symbol RelativeScope <*> (Syntax.RelativeScope <$> source)

qualifiedName :: Assignment
qualifiedName = makeTerm <$> symbol QualifiedName <*> children (Syntax.QualifiedName <$> (term namespaceNameAsPrefix <|> emptyTerm) <*> term name)

namespaceNameAsPrefix :: Assignment
namespaceNameAsPrefix = symbol NamespaceNameAsPrefix *> children (term namespaceName <|> emptyTerm)

namespaceName :: Assignment
namespaceName = makeTerm <$> symbol NamespaceName <*> children (Syntax.NamespaceName <$> someTerm' name)

updateExpression :: Assignment
updateExpression = makeTerm <$> symbol UpdateExpression <*> children (Syntax.Update <$> term expression)

shellCommandExpression :: Assignment
shellCommandExpression = makeTerm <$> symbol ShellCommandExpression <*> (Syntax.ShellCommand <$> source)

literal :: Assignment
literal = integer <|> float <|> string

float :: Assignment
float = makeTerm <$> symbol Float <*> (Literal.Float <$> source)

integer :: Assignment
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

unaryOpExpression :: Assignment
unaryOpExpression = symbol UnaryOpExpression >>= \ loc ->
  makeTerm loc . Expression.Not <$> children ((symbol AnonTilde <|> symbol AnonBang) *> term expression)
  <|> makeTerm loc . Expression.Negate <$> children ((symbol AnonMinus <|> symbol AnonPlus) *> term expression)
  <|> makeTerm loc . Syntax.ErrorControl <$> children (symbol AnonAt *> term expression)

castExpression :: Assignment
castExpression = makeTerm <$> (symbol CastExpression <|> symbol CastExpression') <*> children (flip Expression.Cast <$> term castType <*> term unaryExpression)

castType :: Assignment
castType = makeTerm <$> symbol CastType <*> (Syntax.CastType <$> source)

expressionStatement :: Assignment
expressionStatement = symbol ExpressionStatement *> children (term expression)

namedLabelStatement :: Assignment
namedLabelStatement = makeTerm <$> symbol NamedLabelStatement <*> children (Syntax.LabeledStatement <$> term name)

selectionStatement :: Assignment
selectionStatement = ifStatement <|> switchStatement

ifStatement :: Assignment
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> term expression <*> (makeTerm <$> location <*> manyTerm statement) <*> (makeTerm <$> location <*> ((\as b -> as <> [b]) <$> manyTerm elseIfClause <*> (term elseClause <|> emptyTerm))))

switchStatement :: Assignment
switchStatement = makeTerm <$> symbol SwitchStatement <*> children (Statement.Match <$> term expression <*> (makeTerm <$> location <*> manyTerm (caseStatement <|> defaultStatement)))

caseStatement :: Assignment
caseStatement = makeTerm <$> symbol CaseStatement <*> children (Statement.Pattern <$> term expression <*> (makeTerm <$> location <*> manyTerm statement))

defaultStatement :: Assignment
defaultStatement = makeTerm <$> symbol DefaultStatement <*> children (Statement.Pattern <$> emptyTerm <*> (makeTerm <$> location <*> manyTerm statement))

elseIfClause :: Assignment
elseIfClause = makeTerm <$> symbol ElseIfClause <*> children (Statement.Else <$> term expression <*> (makeTerm <$> location <*> manyTerm statement))

elseClause :: Assignment
elseClause = makeTerm <$> symbol ElseClause <*> children (Statement.Else <$> emptyTerm <*> (makeTerm <$> location <*> manyTerm statement))

iterationStatement :: Assignment
iterationStatement = choice [
  whileStatement,
  doStatement,
  forStatement,
  foreachStatement
  ]

whileStatement :: Assignment
whileStatement = makeTerm <$> symbol WhileStatement <*> children (Statement.While <$> expression <*> (term (statement <|> (makeTerm <$> location <*> manyTerm statement)) <|> emptyTerm))

doStatement :: Assignment
doStatement = makeTerm <$> symbol DoStatement <*> children (Statement.DoWhile <$> term statement <*> term expression)

forStatement :: Assignment
forStatement = makeTerm <$> symbol ForStatement <*> children (Statement.For <$> (term expressions <|> emptyTerm) <*> (term expressions <|> emptyTerm) <*> (term expressions <|> emptyTerm) <*> (makeTerm <$> location <*> manyTerm statement))

foreachStatement :: Assignment
foreachStatement = makeTerm <$> symbol ForeachStatement <*> children (forEachStatement' <$> term expression <*> term (pair <|> expression <|> list) <*> (makeTerm <$> location <*> manyTerm statement))
  where forEachStatement' array value body = Statement.ForEach value array body

pair :: Assignment
pair = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> term expression <*> term (expression <|> list))

jumpStatement :: Assignment
jumpStatement = choice [
  gotoStatement,
  continueStatement,
  breakStatement,
  returnStatement,
  throwStatement
  ]

gotoStatement :: Assignment
gotoStatement = makeTerm <$> symbol GotoStatement <*> children (Statement.Goto <$> term name)

continueStatement :: Assignment
continueStatement = makeTerm <$> symbol ContinueStatement <*> children (Statement.Continue <$> (term breakoutLevel <|> emptyTerm))

breakoutLevel :: Assignment
breakoutLevel = integer

breakStatement :: Assignment
breakStatement = makeTerm <$> symbol BreakStatement <*> children (Statement.Break <$> (term breakoutLevel <|> emptyTerm))

returnStatement :: Assignment
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> (term expression <|> emptyTerm))

throwStatement :: Assignment
throwStatement = makeTerm <$> symbol ThrowStatement <*> children (Statement.Throw <$> term expression)


tryStatement :: Assignment
tryStatement = makeTerm <$> symbol TryStatement <*> children (Statement.Try <$> term compoundStatement <*> (((\as b -> as <> [b]) <$> someTerm catchClause <*> term finallyClause) <|>  someTerm catchClause <|> someTerm finallyClause))

catchClause :: Assignment
catchClause = makeTerm <$> symbol CatchClause <*> children (Statement.Catch <$> (makeTerm <$> location <*> ((\a b -> [a, b]) <$> term qualifiedName <*> term variableName)) <*> term compoundStatement)

finallyClause :: Assignment
finallyClause = makeTerm <$> symbol FinallyClause <*> children (Statement.Finally <$> term compoundStatement)

declareStatement :: Assignment
declareStatement = makeTerm <$> symbol DeclareStatement <*> children (Syntax.Declare <$> term declareDirective <*> (makeTerm <$> location <*> manyTerm statement))


-- | TODO: Figure out how to parse assignment token
declareDirective :: Assignment
declareDirective = makeTerm <$> symbol DeclareDirective <*> children (Syntax.DeclareDirective <$> literal)


echoStatement :: Assignment
echoStatement = makeTerm <$> symbol EchoStatement <*> children (Syntax.Echo <$> term expressions)

unsetStatement :: Assignment
unsetStatement = makeTerm <$> symbol UnsetStatement <*> children (Syntax.Unset <$> (makeTerm <$> location <*> someTerm variable))

expressions :: Assignment
expressions = expression <|> sequenceExpression

sequenceExpression :: Assignment
sequenceExpression = makeTerm <$> symbol SequenceExpression <*> children (Expression.SequenceExpression <$> term expression <*> term expressions)

constDeclaration :: Assignment
constDeclaration = makeTerm <$> symbol ConstDeclaration <*> children (Syntax.ConstDeclaration <$> someTerm constElement)

functionDefinition :: Assignment
functionDefinition = makeTerm <$> symbol FunctionDefinition <*> children (makeFunction <$> term name <*> parameters <*> (term returnType <|> emptyTerm) <*> term compoundStatement)
  where
    makeFunction identifier parameters returnType statement = Declaration.Function [returnType] identifier parameters statement

classDeclaration :: Assignment
classDeclaration = makeTerm <$> symbol ClassDeclaration <*> children (makeClass <$> (term classModifier <|> emptyTerm) <*> term name <*> (term classBaseClause <|> emptyTerm) <*> (term classInterfaceClause <|> emptyTerm) <*> (makeTerm <$> location <*> manyTerm classMemberDeclaration))
  where
    makeClass modifier name baseClause interfaceClause declarations = Declaration.Class [modifier] name [baseClause, interfaceClause] declarations

interfaceDeclaration :: Assignment
interfaceDeclaration = makeTerm <$> symbol InterfaceDeclaration <*> children (Syntax.InterfaceDeclaration <$> term name <*> (term interfaceBaseClause <|> emptyTerm) <*> manyTerm interfaceMemberDeclaration)

interfaceBaseClause :: Assignment
interfaceBaseClause = makeTerm <$> symbol InterfaceBaseClause <*> children (Syntax.InterfaceBaseClause <$> someTerm qualifiedName)

interfaceMemberDeclaration :: Assignment
interfaceMemberDeclaration = methodDeclaration <|> classConstDeclaration

traitDeclaration :: Assignment
traitDeclaration = makeTerm <$> symbol TraitDeclaration <*> children (Syntax.TraitDeclaration <$> term name <*> manyTerm traitMemberDeclaration)

traitMemberDeclaration :: Assignment
traitMemberDeclaration = choice [
  propertyDeclaration,
  methodDeclaration,
  constructorDeclaration,
  destructorDeclaration,
  traitUseClause
  ]

propertyDeclaration :: Assignment
propertyDeclaration = makeTerm <$> symbol PropertyDeclaration <*> children (Syntax.PropertyDeclaration <$> term propertyModifier <*> someTerm propertyElement)

propertyModifier :: Assignment
propertyModifier = (makeTerm <$> symbol PropertyModifier <*> children (Syntax.PropertyModifier <$> (term visibilityModifier <|> emptyTerm) <*> (term staticModifier <|> emptyTerm))) <|> term (makeTerm <$> symbol PropertyModifier <*> (Syntax.Identifier . Name.name <$> source))

propertyElement :: Assignment
propertyElement = makeTerm <$> symbol PropertyElement <*> children (Statement.Assignment [] <$> term variableName <*> term propertyInitializer) <|> (symbol PropertyElement *> children (term variableName))
  where propertyInitializer = symbol PropertyInitializer *> children (term expression)

constructorDeclaration :: Assignment
constructorDeclaration = makeTerm <$> symbol ConstructorDeclaration <*> children (Syntax.ConstructorDeclaration <$> someTerm methodModifier <*> parameters <*> term compoundStatement)

destructorDeclaration :: Assignment
destructorDeclaration = makeTerm <$> symbol DestructorDeclaration <*> children (Syntax.DestructorDeclaration <$> someTerm methodModifier <*> term compoundStatement)

methodModifier :: Assignment
methodModifier = choice [
  visibilityModifier,
  classModifier,
  staticModifier
  ]

staticModifier :: Assignment
staticModifier = makeTerm <$> symbol StaticModifier <*> (Syntax.Static <$> source)

classModifier :: Assignment
classModifier = makeTerm <$> symbol ClassModifier <*> (Syntax.ClassModifier <$> source)

traitUseClause :: Assignment
traitUseClause = makeTerm <$> symbol TraitUseClause <*> children (Syntax.TraitUseClause <$> someTerm qualifiedName <*> (term traitUseSpecification <|> emptyTerm))

traitUseSpecification :: Assignment
traitUseSpecification = makeTerm <$> symbol TraitUseSpecification <*> children (Syntax.TraitUseSpecification <$> manyTerm traitSelectAndAliasClause)

traitSelectAndAliasClause :: Assignment
traitSelectAndAliasClause = traitSelectInsteadOfClause <|> traitAliasAsClause

traitSelectInsteadOfClause :: Assignment
traitSelectInsteadOfClause = makeTerm <$> symbol TraitSelectInsteadOfClause <*> children (Syntax.InsteadOf <$> term (classConstantAccessExpression <|> name) <*> term name)

traitAliasAsClause :: Assignment
traitAliasAsClause = makeTerm <$> symbol TraitAliasAsClause <*> children (Syntax.AliasAs <$> term (classConstantAccessExpression <|> name) <*> (term visibilityModifier <|> emptyTerm) <*> (term name <|> emptyTerm))

namespaceDefinition :: Assignment
namespaceDefinition = makeTerm <$> symbol NamespaceDefinition <*> children (Syntax.Namespace <$> (term namespaceName <|> emptyTerm) <*> (term compoundStatement <|> emptyTerm))

namespaceUseDeclaration :: Assignment
namespaceUseDeclaration = makeTerm <$> symbol NamespaceUseDeclaration <*> children (Syntax.NamespaceUseDeclaration <$>
  ((mappend <$> (pure <$> (term namespaceFunctionOrConst <|> emptyTerm)) <*> someTerm namespaceUseClause) <|> ((\a b cs -> a : b : cs) <$> term namespaceFunctionOrConst <*> term namespaceName <*> someTerm namespaceUseGroupClause1) <|> ((:) <$> term namespaceName <*> someTerm namespaceUseGroupClause2)))

namespaceUseClause :: Assignment
namespaceUseClause = makeTerm <$> symbol NamespaceUseClause <*> children (fmap Syntax.NamespaceUseClause $ (\a b -> [a, b]) <$> term qualifiedName <*> (term namespaceAliasingClause <|> emptyTerm))

namespaceUseGroupClause1 :: Assignment
namespaceUseGroupClause1 = makeTerm <$> symbol NamespaceUseGroupClause_1 <*> children (fmap Syntax.NamespaceUseGroupClause $ (\a b -> [a, b]) <$> term namespaceName <*> (term namespaceAliasingClause <|> emptyTerm))

namespaceUseGroupClause2 :: Assignment
namespaceUseGroupClause2 = makeTerm <$> symbol NamespaceUseGroupClause_2 <*> children (fmap Syntax.NamespaceUseGroupClause $ (\a b c -> [a, b, c]) <$> (term namespaceFunctionOrConst <|> emptyTerm) <*> term namespaceName <*> (term namespaceAliasingClause <|> emptyTerm))

namespaceAliasingClause :: Assignment
namespaceAliasingClause = makeTerm <$> symbol NamespaceAliasingClause <*> children (Syntax.NamespaceAliasingClause <$> term name)

-- | TODO Do something better than Identifier
namespaceFunctionOrConst :: Assignment
namespaceFunctionOrConst = makeTerm <$> symbol NamespaceFunctionOrConst <*> (Syntax.Identifier . Name.name <$> source)

globalDeclaration :: Assignment
globalDeclaration = makeTerm <$> symbol GlobalDeclaration <*> children (Syntax.GlobalDeclaration <$> manyTerm simpleVariable')

simpleVariable :: Assignment
simpleVariable = makeTerm <$> symbol SimpleVariable <*> children (Syntax.SimpleVariable <$> term (simpleVariable' <|> expression))

simpleVariable' :: Assignment
simpleVariable' = choice [simpleVariable, variableName]

simpleVariable'' :: Assignment.Assignment [] Grammar Name.Name
simpleVariable'' = variableName'


yieldExpression :: Assignment
yieldExpression = makeTerm <$> symbol YieldExpression <*> children (Statement.Yield <$> term (arrayElementInitializer <|> expression))

arrayElementInitializer :: Assignment
arrayElementInitializer = makeTerm <$> symbol ArrayElementInitializer <*> children (Literal.KeyValue <$> term expression <*> term expression) <|> (symbol ArrayElementInitializer *> children (term expression))

includeExpression :: Assignment
includeExpression = makeTerm <$> symbol IncludeExpression <*> children (Syntax.Include <$> term expression)


includeOnceExpression :: Assignment
includeOnceExpression = makeTerm <$> symbol IncludeOnceExpression <*> children (Syntax.IncludeOnce <$> term expression)

requireExpression :: Assignment
requireExpression = makeTerm <$> symbol RequireExpression <*> children (Syntax.Require <$> term expression)


requireOnceExpression :: Assignment
requireOnceExpression = makeTerm <$> symbol RequireOnceExpression <*> children (Syntax.RequireOnce <$> term expression)

variableName :: Assignment
variableName = makeTerm <$> symbol VariableName <*> children (Syntax.VariableName <$> term name)

variableName' :: Assignment.Assignment [] Grammar Name.Name
variableName' = symbol VariableName *> children name'

name :: Assignment
name = makeTerm <$> (symbol Name <|> symbol Name') <*> (Syntax.Identifier . Name.name <$> source)

name' :: Assignment.Assignment [] Grammar Name.Name
name' = (symbol Name <|> symbol Name') *> (Name.name <$> source)

functionStaticDeclaration :: Assignment
functionStaticDeclaration = makeTerm <$> symbol FunctionStaticDeclaration <*> children (Declaration.VariableDeclaration <$> manyTerm staticVariableDeclaration)

staticVariableDeclaration :: Assignment
staticVariableDeclaration = makeTerm <$> symbol StaticVariableDeclaration <*> children (Statement.Assignment [] <$> term variableName <*> (term expression <|> emptyTerm))

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

string :: Assignment
string = makeTerm <$> (symbol Grammar.String <|> symbol Heredoc) <*> (Literal.TextElement <$> source)


-- Helpers

append :: a -> [a] -> [a]
append x xs = xs <> [x]

bookend :: a -> [a] -> a -> [a]
bookend head_ list last_ = head_ : append last_ list

term :: Assignment -> Assignment
term term = contextualize (comment <|> textInterpolation) (postContextualize (comment <|> textInterpolation) term)

commentedTerm :: Assignment -> Assignment
commentedTerm term = contextualize (comment <|> textInterpolation) term <|> makeTerm1 <$> (Syntax.Context <$> some1 (comment <|> textInterpolation) <*> emptyTerm)

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
manyTerm = many . commentedTerm

someTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
someTerm = fmap NonEmpty.toList . someTerm'

someTerm' :: Assignment -> Assignment.Assignment [] Grammar (NonEmpty Term)
someTerm' = NonEmpty.some1 . commentedTerm

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment
          -> Assignment
          -> [Assignment.Assignment [] Grammar (Term -> Term -> Sum Syntax Term)]
          -> Assignment.Assignment [] Grammar (Sum Syntax Term)
infixTerm = infixContext (comment <|> textInterpolation)

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
