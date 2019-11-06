{-# LANGUAGE DataKinds, FlexibleContexts, RankNTypes, TypeFamilies, TypeOperators #-}
module Language.PHP.Assignment
( assignment
, PHP.Syntax
, Grammar
, PHP.Term(..)
) where

import Prologue

import           Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import qualified Data.Abstract.Name as Name
import qualified Data.Abstract.ScopeGraph as ScopeGraph (AccessControl(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Syntax
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
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import qualified Language.PHP.Syntax as Syntax
import           Language.PHP.Term as PHP
import           TreeSitter.PHP as Grammar

type Assignment = Assignment.Assignment [] Grammar

-- | Assignment from AST in PHP's grammar onto a program in PHP's syntax.
assignment :: Assignment (Term Loc)
assignment = handleError $ makeTerm <$> symbol Program <*> children (Statement.Statements <$> (bookend <$> (text <|> emptyTerm) <*> manyTerm statement <*> (text <|> emptyTerm))) <|> parseError

text :: Assignment (Term Loc)
text = makeTerm <$> symbol Text <*> (Syntax.Text <$> source)

textInterpolation :: Assignment (Term Loc)
textInterpolation = makeTerm <$> symbol TextInterpolation <*> (Syntax.Text <$> source)

statement :: Assignment (Term Loc)
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

expression :: Assignment (Term Loc)
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

unaryExpression :: Assignment (Term Loc)
unaryExpression = choice [
  cloneExpression,
  exponentiationExpression,
  unaryOpExpression,
  castExpression,
  primaryExpression
  ]

assignmentExpression :: Assignment (Term Loc)
assignmentExpression = makeTerm <$> symbol AssignmentExpression <*> children (Statement.Assignment [] <$> term (variable <|> list <|> arrayCreationExpression) <*> term (expression <|> variable))

augmentedAssignmentExpression :: Assignment (Term Loc)
augmentedAssignmentExpression = makeTerm' <$> symbol AugmentedAssignmentExpression <*> children (infixTerm variable (term expression) [
  assign Expression.Power <$ symbol AnonStarStarEqual
  , assign Expression.Times <$ symbol AnonStarEqual
  , assign Expression.Modulo <$ symbol AnonPercentEqual
  , assign Expression.DividedBy <$ symbol AnonSlashEqual
  , assign Expression.Plus <$ symbol AnonPlusEqual
  , assign Expression.Minus <$ symbol AnonMinusEqual
  , assign Syntax.Concat <$ symbol AnonDotEqual
  , assign Expression.LShift <$ symbol AnonLAngleLAngleEqual
  , assign Expression.RShift <$ symbol AnonRAngleRAngleEqual
  , assign Expression.BAnd <$ symbol AnonAmpersandEqual
  , assign Expression.BXOr <$ symbol AnonCaretEqual
  , assign Expression.BOr <$ symbol AnonPipeEqual ])
  where
    assign c l r = inject (Statement.Assignment [] l (makeTerm1 (c l r)))

binaryExpression  :: Assignment (Term Loc)
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
  , (inject .) . invert Expression.Equal       <$ (symbol AnonBangEqual <|> symbol AnonLAngleRAngle)
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
  , (inject .) . Expression.Times              <$ symbol AnonStar
  , (inject .) . Syntax.Concat                 <$ symbol AnonDot
  , (inject .) . Expression.DividedBy          <$ symbol AnonSlash
  , (inject .) . Expression.Modulo             <$ symbol AnonPercent
  , (inject .) . Expression.InstanceOf         <$ symbol AnonInstanceof
  ]) where invert cons a b = Expression.Not (makeTerm1 (cons a b))

conditionalExpression :: Assignment (Term Loc)
conditionalExpression = makeTerm <$> symbol ConditionalExpression <*> children (Statement.If <$> term (binaryExpression <|> unaryExpression) <*> (term expression <|> emptyTerm) <*> term expression)

list :: Assignment (Term Loc)
list = makeTerm <$> symbol ListLiteral <*> children (Literal.Array <$> manyTerm (list <|> variable))

exponentiationExpression :: Assignment (Term Loc)
exponentiationExpression = makeTerm <$> symbol ExponentiationExpression <*> children (Expression.Power <$> term (cloneExpression <|> primaryExpression) <*> term (primaryExpression <|> cloneExpression <|> exponentiationExpression))

cloneExpression :: Assignment (Term Loc)
cloneExpression = makeTerm <$> symbol CloneExpression <*> children (Syntax.Clone <$> term primaryExpression)

primaryExpression :: Assignment (Term Loc)
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

parenthesizedExpression :: Assignment (Term Loc)
parenthesizedExpression = symbol ParenthesizedExpression *> children (term expression)

classConstantAccessExpression :: Assignment (Term Loc)
classConstantAccessExpression = makeTerm <$> symbol ClassConstantAccessExpression <*> children (Expression.MemberAccess <$> term scopeResolutionQualifier <*> name)

variable :: Assignment (Term Loc)
variable = callableVariable <|> scopedPropertyAccessExpression <|> memberAccessExpression <|> castExpression

callableVariable :: Assignment (Term Loc)
callableVariable = choice [
  simpleVariable',
  subscriptExpression,
  memberCallExpression,
  scopedCallExpression,
  functionCallExpression
  ]

memberCallExpression :: Assignment (Term Loc)
memberCallExpression = makeTerm <$> symbol MemberCallExpression <*> children (Expression.Call [] <$> (makeMemberAccess <$> location <*> term dereferencableExpression <*> memberName) <*> arguments <*> emptyTerm)
  where makeMemberAccess loc expr memberName = makeTerm loc (Expression.MemberAccess expr memberName)

scopedCallExpression :: Assignment (Term Loc)
scopedCallExpression = makeTerm <$> symbol ScopedCallExpression <*> children (Expression.Call [] <$> (makeMemberAccess <$> location <*> term scopeResolutionQualifier <*> memberName) <*> arguments <*> emptyTerm)
  where makeMemberAccess loc expr memberName = makeTerm loc (Expression.MemberAccess expr memberName)

functionCallExpression :: Assignment (Term Loc)
functionCallExpression = makeTerm <$> symbol FunctionCallExpression <*> children (Expression.Call [] <$> term (qualifiedName <|> callableExpression) <*> arguments <*> emptyTerm)

callableExpression :: Assignment (Term Loc)
callableExpression = choice [
  callableVariable,
  expression,
  arrayCreationExpression,
  string
  ]

subscriptExpression :: Assignment (Term Loc)
subscriptExpression = makeTerm <$> symbol SubscriptExpression <*> children (Expression.Subscript <$> term dereferencableExpression <*> (pure <$> (term expression <|> emptyTerm)))

memberAccessExpression :: Assignment (Term Loc)
memberAccessExpression = makeTerm <$> symbol MemberAccessExpression <*> children (Expression.MemberAccess <$> term dereferencableExpression <*> memberName)

dereferencableExpression :: Assignment (Term Loc)
dereferencableExpression = symbol DereferencableExpression *> children (term (variable <|> expression <|> arrayCreationExpression <|> string))

scopedPropertyAccessExpression :: Assignment (Term Loc)
scopedPropertyAccessExpression = makeTerm <$> symbol ScopedPropertyAccessExpression <*> children (Expression.MemberAccess <$> term scopeResolutionQualifier <*> simpleVariable')

scopeResolutionQualifier :: Assignment (Term Loc)
scopeResolutionQualifier = choice [
  relativeScope,
  qualifiedName,
  dereferencableExpression
  ]

arrayCreationExpression :: Assignment (Term Loc)
arrayCreationExpression = makeTerm <$> symbol ArrayCreationExpression <*> children (Literal.Array <$> manyTerm arrayElementInitializer)

intrinsic :: Assignment (Term Loc)
intrinsic = choice [
  emptyIntrinsic,
  evalIntrinsic,
  exitIntrinsic,
  issetIntrinsic,
  printIntrinsic
  ]

emptyIntrinsic :: Assignment (Term Loc)
emptyIntrinsic = makeTerm <$> symbol EmptyIntrinsic <*> children (Syntax.EmptyIntrinsic <$> term expression)

evalIntrinsic :: Assignment (Term Loc)
evalIntrinsic = makeTerm <$> symbol EvalIntrinsic <*> children (Syntax.EvalIntrinsic <$> term expression)

exitIntrinsic :: Assignment (Term Loc)
exitIntrinsic = makeTerm <$> symbol ExitIntrinsic <*> children (Syntax.ExitIntrinsic <$> (term expression <|> emptyTerm))

issetIntrinsic :: Assignment (Term Loc)
issetIntrinsic = makeTerm <$> symbol IssetIntrinsic <*> children (Syntax.IssetIntrinsic <$> (makeTerm <$> location <*> someTerm variable))

printIntrinsic :: Assignment (Term Loc)
printIntrinsic = makeTerm <$> symbol PrintIntrinsic <*> children (Syntax.PrintIntrinsic <$> term expression)

anonymousFunctionCreationExpression :: Assignment (Term Loc)
anonymousFunctionCreationExpression = makeTerm <$> symbol AnonymousFunctionCreationExpression <*> children (makeFunction <$> emptyTerm <*> parameters <*> (term functionUseClause <|> emptyTerm) <*> (term returnType <|> emptyTerm) <*> term compoundStatement)
  where
    makeFunction identifier parameters functionUseClause returnType statement = Declaration.Function [functionUseClause, returnType] identifier parameters statement

parameters :: Assignment [Term Loc]
parameters = symbol FormalParameters *> children (manyTerm (simpleParameter <|> variadicParameter))

simpleParameter :: Assignment (Term Loc)
simpleParameter = makeTerm <$> symbol SimpleParameter <*> children (makeAnnotation <$> (term typeDeclaration <|> emptyTerm) <*> (makeAssignment <$> location <*> term variableName <*> (term defaultArgumentSpecifier <|> emptyTerm)))
  where
    makeAnnotation typeDecl assignment = Type.Annotation assignment typeDecl
    makeAssignment loc name argument = makeTerm loc (Statement.Assignment [] name argument)

defaultArgumentSpecifier :: Assignment (Term Loc)
defaultArgumentSpecifier = symbol DefaultArgumentSpecifier *> children (term expression)


variadicParameter :: Assignment (Term Loc)
variadicParameter = makeTerm <$> symbol VariadicParameter <*> children (makeTypeAnnotation <$> (term typeDeclaration <|> emptyTerm) <*> term variableName)
  where makeTypeAnnotation ty variableName = Type.Annotation variableName ty

functionUseClause :: Assignment (Term Loc)
functionUseClause = makeTerm <$> symbol AnonymousFunctionUseClause <*> children (Syntax.UseClause <$> someTerm variableName)

returnType :: Assignment (Term Loc)
returnType = makeTerm <$> symbol ReturnType <*> children (Syntax.ReturnType <$> (term typeDeclaration <|> emptyTerm))

typeDeclaration :: Assignment (Term Loc)
typeDeclaration = makeTerm <$> symbol TypeDeclaration <*> children (Syntax.TypeDeclaration <$> term baseTypeDeclaration)

baseTypeDeclaration :: Assignment (Term Loc)
baseTypeDeclaration = makeTerm <$> symbol BaseTypeDeclaration <*> children (Syntax.BaseTypeDeclaration <$> term (scalarType <|> qualifiedName <|> emptyTerm))

scalarType :: Assignment (Term Loc)
scalarType = makeTerm <$> symbol ScalarType <*> (Syntax.ScalarType <$> source)

compoundStatement :: Assignment (Term Loc)
compoundStatement = makeTerm <$> symbol CompoundStatement <*> children (manyTerm statement)

objectCreationExpression :: Assignment (Term Loc)
objectCreationExpression = makeTerm <$> symbol ObjectCreationExpression <*> children (Expression.New <$> term classTypeDesignator <*> emptyTerm <*> (arguments <|> pure []))

  <|> (makeTerm <$> symbol ObjectCreationExpression <*> children (makeAnonClass <$ token AnonNew <* token AnonClass <*> emptyTerm <*> (arguments <|> pure []) <*> (term classBaseClause <|> emptyTerm) <*> (term classInterfaceClause <|> emptyTerm) <*> (makeTerm <$> location <*> manyTerm classMemberDeclaration)))
  where makeAnonClass identifier args baseClause interfaceClause declarations = Declaration.Class [] identifier (args <> [baseClause, interfaceClause]) declarations

classMemberDeclaration :: Assignment (Term Loc)
classMemberDeclaration = choice [
  classConstDeclaration,
  propertyDeclaration,
  methodDeclaration,
  constructorDeclaration,
  destructorDeclaration,
  traitUseClause
  ]

publicAccessControl :: ScopeGraph.AccessControl
publicAccessControl = ScopeGraph.Public

-- TODO: Update to check for AccessControl.
methodDeclaration :: Assignment (Term Loc)
methodDeclaration =  (makeTerm <$> symbol MethodDeclaration <*> children (makeMethod1 publicAccessControl <$> manyTerm methodModifier <*> emptyTerm <*> functionDefinitionParts))
                 <|> makeTerm <$> symbol MethodDeclaration <*> children (makeMethod2 publicAccessControl <$> someTerm methodModifier <*> emptyTerm <*> term name <*> parameters <*> term (returnType <|> emptyTerm) <*> emptyTerm)
  where
    functionDefinitionParts = symbol FunctionDefinition *> children ((,,,) <$> term name <*> parameters <*> term (returnType <|> emptyTerm) <*> (term compoundStatement <|> emptyTerm))
    makeMethod1 accessControl modifiers receiver (name, params, returnType, compoundStatement) = Declaration.Method (modifiers <> [returnType]) receiver name params compoundStatement accessControl
    makeMethod2 accessControl modifiers receiver name params returnType compoundStatement      = Declaration.Method (modifiers <> [returnType]) receiver name params compoundStatement accessControl

classBaseClause :: Assignment (Term Loc)
classBaseClause = makeTerm <$> symbol ClassBaseClause <*> children (Syntax.ClassBaseClause <$> term qualifiedName)

classInterfaceClause :: Assignment (Term Loc)
classInterfaceClause = makeTerm <$> symbol ClassInterfaceClause <*> children (Syntax.ClassInterfaceClause <$> someTerm qualifiedName)

classConstDeclaration :: Assignment (Term Loc)
classConstDeclaration = makeTerm <$> symbol ClassConstDeclaration <*> children (Syntax.ClassConstDeclaration <$> (term accessControlModifier <|> emptyTerm) <*> manyTerm constElement)

-- TODO: Update to ScopeGraph.AccessControl
accessControlModifier :: Assignment (Term Loc)
accessControlModifier = makeTerm <$> symbol VisibilityModifier <*> (Syntax.Identifier . Name.name <$> source)

constElement :: Assignment (Term Loc)
constElement = makeTerm <$> symbol ConstElement <*> children (Statement.Assignment [] <$> term name <*> term expression)

arguments :: Assignment [Term Loc]
arguments = symbol Arguments *> children (manyTerm (variadicUnpacking <|> expression))

variadicUnpacking :: Assignment (Term Loc)
variadicUnpacking = symbol VariadicUnpacking *> children (term expression)

classTypeDesignator :: Assignment (Term Loc)
classTypeDesignator = qualifiedName <|> newVariable

newVariable :: Assignment (Term Loc)
newVariable = makeTerm <$> symbol NewVariable <*> children (Syntax.NewVariable <$> ((pure <$> term simpleVariable') <|> ((\a b -> [a, b]) <$> term (newVariable <|> qualifiedName <|> relativeScope) <*> term (expression <|> memberName <|> emptyTerm))))

memberName :: Assignment (Term Loc)
memberName = name <|> simpleVariable' <|> expression

relativeScope :: Assignment (Term Loc)
relativeScope = makeTerm <$> symbol RelativeScope <*> (Syntax.RelativeScope <$> source)

qualifiedName :: Assignment (Term Loc)
qualifiedName = makeTerm <$> symbol QualifiedName <*> children (Syntax.QualifiedName <$> (term namespaceNameAsPrefix <|> emptyTerm) <*> term name)

namespaceNameAsPrefix :: Assignment (Term Loc)
namespaceNameAsPrefix = symbol NamespaceNameAsPrefix *> children (term namespaceName <|> emptyTerm)

namespaceName :: Assignment (Term Loc)
namespaceName = makeTerm <$> symbol NamespaceName <*> children (Syntax.NamespaceName <$> someTerm' name)

namespaceName' :: Assignment (NonEmpty (Term Loc))
namespaceName' = symbol NamespaceName *> children (someTerm' name)

updateExpression :: Assignment (Term Loc)
updateExpression = makeTerm <$> symbol UpdateExpression <*> children (Syntax.Update <$> term expression)

shellCommandExpression :: Assignment (Term Loc)
shellCommandExpression = makeTerm <$> symbol ShellCommandExpression <*> (Syntax.ShellCommand <$> source)

literal :: Assignment (Term Loc)
literal = integer <|> float <|> string

float :: Assignment (Term Loc)
float = makeTerm <$> symbol Float <*> (Literal.Float <$> source)

integer :: Assignment (Term Loc)
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

unaryOpExpression :: Assignment (Term Loc)
unaryOpExpression = symbol UnaryOpExpression >>= \ loc ->
  makeTerm loc . Expression.Not <$> children ((symbol AnonTilde <|> symbol AnonBang) *> term expression)
  <|> makeTerm loc . Expression.Negate <$> children ((symbol AnonMinus <|> symbol AnonPlus) *> term expression)
  <|> makeTerm loc . Syntax.ErrorControl <$> children (symbol AnonAt *> term expression)

castExpression :: Assignment (Term Loc)
castExpression = makeTerm <$> (symbol CastExpression <|> symbol CastExpression') <*> children (flip Expression.Cast <$> term castType <*> term unaryExpression)

castType :: Assignment (Term Loc)
castType = makeTerm <$> symbol CastType <*> (Syntax.CastType <$> source)

expressionStatement :: Assignment (Term Loc)
expressionStatement = symbol ExpressionStatement *> children (term expression)

namedLabelStatement :: Assignment (Term Loc)
namedLabelStatement = makeTerm <$> symbol NamedLabelStatement <*> children (Syntax.LabeledStatement <$> term name)

selectionStatement :: Assignment (Term Loc)
selectionStatement = ifStatement <|> switchStatement

ifStatement :: Assignment (Term Loc)
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> term expression <*> (makeTerm <$> location <*> manyTerm statement) <*> (makeTerm <$> location <*> ((\as b -> as <> [b]) <$> manyTerm elseIfClause <*> (term elseClause <|> emptyTerm))))

switchStatement :: Assignment (Term Loc)
switchStatement = makeTerm <$> symbol SwitchStatement <*> children (Statement.Match <$> term expression <*> (makeTerm <$> location <*> manyTerm (caseStatement <|> defaultStatement)))

caseStatement :: Assignment (Term Loc)
caseStatement = makeTerm <$> symbol CaseStatement <*> children (Statement.Pattern <$> term expression <*> (makeTerm <$> location <*> manyTerm statement))

defaultStatement :: Assignment (Term Loc)
defaultStatement = makeTerm <$> symbol DefaultStatement <*> children (Statement.Pattern <$> emptyTerm <*> (makeTerm <$> location <*> manyTerm statement))

elseIfClause :: Assignment (Term Loc)
elseIfClause = makeTerm <$> symbol ElseIfClause <*> children (Statement.Else <$> term expression <*> (makeTerm <$> location <*> manyTerm statement))

elseClause :: Assignment (Term Loc)
elseClause = makeTerm <$> symbol ElseClause <*> children (Statement.Else <$> emptyTerm <*> (makeTerm <$> location <*> manyTerm statement))

iterationStatement :: Assignment (Term Loc)
iterationStatement = choice [
  whileStatement,
  doStatement,
  forStatement,
  foreachStatement
  ]

whileStatement :: Assignment (Term Loc)
whileStatement = makeTerm <$> symbol WhileStatement <*> children (Statement.While <$> expression <*> (term (statement <|> (makeTerm <$> location <*> manyTerm statement)) <|> emptyTerm))

doStatement :: Assignment (Term Loc)
doStatement = makeTerm <$> symbol DoStatement <*> children (Statement.DoWhile <$> term statement <*> term expression)

forStatement :: Assignment (Term Loc)
forStatement = makeTerm <$> symbol ForStatement <*> children (Statement.For <$> (term expressions <|> emptyTerm) <*> (term expressions <|> emptyTerm) <*> (term expressions <|> emptyTerm) <*> (makeTerm <$> location <*> manyTerm statement))

foreachStatement :: Assignment (Term Loc)
foreachStatement = makeTerm <$> symbol ForeachStatement <*> children (forEachStatement' <$> term expression <*> term (pair <|> expression <|> list) <*> (makeTerm <$> location <*> manyTerm statement))
  where forEachStatement' array value body = Statement.ForEach value array body

pair :: Assignment (Term Loc)
pair = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> term expression <*> term (expression <|> list))

jumpStatement :: Assignment (Term Loc)
jumpStatement = choice [
  gotoStatement,
  continueStatement,
  breakStatement,
  returnStatement,
  throwStatement
  ]

gotoStatement :: Assignment (Term Loc)
gotoStatement = makeTerm <$> symbol GotoStatement <*> children (Statement.Goto <$> term name)

continueStatement :: Assignment (Term Loc)
continueStatement = makeTerm <$> symbol ContinueStatement <*> children (Statement.Continue <$> (term breakoutLevel <|> emptyTerm))

breakoutLevel :: Assignment (Term Loc)
breakoutLevel = integer

breakStatement :: Assignment (Term Loc)
breakStatement = makeTerm <$> symbol BreakStatement <*> children (Statement.Break <$> (term breakoutLevel <|> emptyTerm))

returnStatement :: Assignment (Term Loc)
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> (term expression <|> emptyTerm))

throwStatement :: Assignment (Term Loc)
throwStatement = makeTerm <$> symbol ThrowStatement <*> children (Statement.Throw <$> term expression)


tryStatement :: Assignment (Term Loc)
tryStatement = makeTerm <$> symbol TryStatement <*> children (Statement.Try <$> term compoundStatement <*> (((\as b -> as <> [b]) <$> someTerm catchClause <*> term finallyClause) <|>  someTerm catchClause <|> someTerm finallyClause))

catchClause :: Assignment (Term Loc)
catchClause = makeTerm <$> symbol CatchClause <*> children (Statement.Catch <$> (makeTerm <$> location <*> ((\a b -> [a, b]) <$> term qualifiedName <*> term variableName)) <*> term compoundStatement)

finallyClause :: Assignment (Term Loc)
finallyClause = makeTerm <$> symbol FinallyClause <*> children (Statement.Finally <$> term compoundStatement)

declareStatement :: Assignment (Term Loc)
declareStatement = makeTerm <$> symbol DeclareStatement <*> children (Syntax.Declare <$> term declareDirective <*> (makeTerm <$> location <*> manyTerm statement))


-- | TODO: Figure out how to parse assignment token
declareDirective :: Assignment (Term Loc)
declareDirective = makeTerm <$> symbol DeclareDirective <*> children (Syntax.DeclareDirective <$> literal)


echoStatement :: Assignment (Term Loc)
echoStatement = makeTerm <$> symbol EchoStatement <*> children (Syntax.Echo <$> term expressions)

unsetStatement :: Assignment (Term Loc)
unsetStatement = makeTerm <$> symbol UnsetStatement <*> children (Syntax.Unset <$> (makeTerm <$> location <*> someTerm variable))

expressions :: Assignment (Term Loc)
expressions = expression <|> sequenceExpression

sequenceExpression :: Assignment (Term Loc)
sequenceExpression = makeTerm <$> symbol SequenceExpression <*> children (Expression.SequenceExpression <$> term expression <*> term expressions)

constDeclaration :: Assignment (Term Loc)
constDeclaration = makeTerm <$> symbol ConstDeclaration <*> children (Syntax.ConstDeclaration <$> someTerm constElement)

functionDefinition :: Assignment (Term Loc)
functionDefinition = makeTerm <$> symbol FunctionDefinition <*> children (makeFunction <$> term name <*> parameters <*> (term returnType <|> emptyTerm) <*> term compoundStatement)
  where
    makeFunction identifier parameters returnType statement = Declaration.Function [returnType] identifier parameters statement

classDeclaration :: Assignment (Term Loc)
classDeclaration = makeTerm <$> symbol ClassDeclaration <*> children (makeClass <$> (term classModifier <|> emptyTerm) <*> term name <*> (term classBaseClause <|> emptyTerm) <*> (term classInterfaceClause <|> emptyTerm) <*> (makeTerm <$> location <*> manyTerm classMemberDeclaration))
  where
    makeClass modifier name baseClause interfaceClause declarations = Declaration.Class [modifier] name [baseClause, interfaceClause] declarations

interfaceDeclaration :: Assignment (Term Loc)
interfaceDeclaration = makeTerm <$> symbol InterfaceDeclaration <*> children (Syntax.InterfaceDeclaration <$> term name <*> (term interfaceBaseClause <|> emptyTerm) <*> manyTerm interfaceMemberDeclaration)

interfaceBaseClause :: Assignment (Term Loc)
interfaceBaseClause = makeTerm <$> symbol InterfaceBaseClause <*> children (Syntax.InterfaceBaseClause <$> someTerm qualifiedName)

interfaceMemberDeclaration :: Assignment (Term Loc)
interfaceMemberDeclaration = methodDeclaration <|> classConstDeclaration

traitDeclaration :: Assignment (Term Loc)
traitDeclaration = makeTerm <$> symbol TraitDeclaration <*> children (Syntax.TraitDeclaration <$> term name <*> manyTerm traitMemberDeclaration)

traitMemberDeclaration :: Assignment (Term Loc)
traitMemberDeclaration = choice [
  propertyDeclaration,
  methodDeclaration,
  constructorDeclaration,
  destructorDeclaration,
  traitUseClause
  ]

propertyDeclaration :: Assignment (Term Loc)
propertyDeclaration = makeTerm <$> symbol PropertyDeclaration <*> children (Syntax.PropertyDeclaration <$> term propertyModifier <*> someTerm propertyElement)

propertyModifier :: Assignment (Term Loc)
propertyModifier = (makeTerm <$> symbol PropertyModifier <*> children (Syntax.PropertyModifier <$> (term accessControlModifier <|> emptyTerm) <*> (term staticModifier <|> emptyTerm))) <|> term (makeTerm <$> symbol PropertyModifier <*> (Syntax.Identifier . Name.name <$> source))

propertyElement :: Assignment (Term Loc)
propertyElement = makeTerm <$> symbol PropertyElement <*> children (Statement.Assignment [] <$> term variableName <*> term propertyInitializer) <|> (symbol PropertyElement *> children (term variableName))
  where propertyInitializer = symbol PropertyInitializer *> children (term expression)

constructorDeclaration :: Assignment (Term Loc)
constructorDeclaration = makeTerm <$> symbol ConstructorDeclaration <*> children (Syntax.ConstructorDeclaration <$> someTerm methodModifier <*> parameters <*> term compoundStatement)

destructorDeclaration :: Assignment (Term Loc)
destructorDeclaration = makeTerm <$> symbol DestructorDeclaration <*> children (Syntax.DestructorDeclaration <$> someTerm methodModifier <*> term compoundStatement)

methodModifier :: Assignment (Term Loc)
methodModifier = choice [
  accessControlModifier,
  classModifier,
  staticModifier
  ]

staticModifier :: Assignment (Term Loc)
staticModifier = makeTerm <$> symbol StaticModifier <*> (Syntax.Static <$> source)

classModifier :: Assignment (Term Loc)
classModifier = makeTerm <$> symbol ClassModifier <*> (Syntax.ClassModifier <$> source)

traitUseClause :: Assignment (Term Loc)
traitUseClause = makeTerm <$> symbol TraitUseClause <*> children (Syntax.TraitUseClause <$> someTerm qualifiedName <*> (term traitUseSpecification <|> emptyTerm))

traitUseSpecification :: Assignment (Term Loc)
traitUseSpecification = makeTerm <$> symbol TraitUseSpecification <*> children (Syntax.TraitUseSpecification <$> manyTerm traitSelectAndAliasClause)

traitSelectAndAliasClause :: Assignment (Term Loc)
traitSelectAndAliasClause = traitSelectInsteadOfClause <|> traitAliasAsClause

traitSelectInsteadOfClause :: Assignment (Term Loc)
traitSelectInsteadOfClause = makeTerm <$> symbol TraitSelectInsteadOfClause <*> children (Syntax.InsteadOf <$> term (classConstantAccessExpression <|> name) <*> term name)

traitAliasAsClause :: Assignment (Term Loc)
traitAliasAsClause = makeTerm <$> symbol TraitAliasAsClause <*> children (Syntax.AliasAs <$> term (classConstantAccessExpression <|> name) <*> (term accessControlModifier <|> emptyTerm) <*> (term name <|> emptyTerm))

namespaceDefinition :: Assignment (Term Loc)
namespaceDefinition = makeTerm <$> symbol NamespaceDefinition <*> children (Syntax.Namespace <$> (toList <$> namespaceName' <|> pure []) <*> (term compoundStatement <|> emptyTerm))

namespaceUseDeclaration :: Assignment (Term Loc)
namespaceUseDeclaration = makeTerm <$> symbol NamespaceUseDeclaration <*> children (Syntax.NamespaceUseDeclaration <$>
  ((mappend <$> (pure <$> (term namespaceFunctionOrConst <|> emptyTerm)) <*> someTerm namespaceUseClause) <|> ((\a b cs -> a : b : cs) <$> term namespaceFunctionOrConst <*> term namespaceName <*> someTerm namespaceUseGroupClause1) <|> ((:) <$> term namespaceName <*> someTerm namespaceUseGroupClause2)))

namespaceUseClause :: Assignment (Term Loc)
namespaceUseClause = makeTerm <$> symbol NamespaceUseClause <*> children (fmap Syntax.NamespaceUseClause $ (\a b -> [a, b]) <$> term qualifiedName <*> (term namespaceAliasingClause <|> emptyTerm))

namespaceUseGroupClause1 :: Assignment (Term Loc)
namespaceUseGroupClause1 = makeTerm <$> symbol NamespaceUseGroupClause_1 <*> children (fmap Syntax.NamespaceUseGroupClause $ (\a b -> [a, b]) <$> term namespaceName <*> (term namespaceAliasingClause <|> emptyTerm))

namespaceUseGroupClause2 :: Assignment (Term Loc)
namespaceUseGroupClause2 = makeTerm <$> symbol NamespaceUseGroupClause_2 <*> children (fmap Syntax.NamespaceUseGroupClause $ (\a b c -> [a, b, c]) <$> (term namespaceFunctionOrConst <|> emptyTerm) <*> term namespaceName <*> (term namespaceAliasingClause <|> emptyTerm))

namespaceAliasingClause :: Assignment (Term Loc)
namespaceAliasingClause = makeTerm <$> symbol NamespaceAliasingClause <*> children (Syntax.NamespaceAliasingClause <$> term name)

-- | TODO Do something better than Identifier
namespaceFunctionOrConst :: Assignment (Term Loc)
namespaceFunctionOrConst = makeTerm <$> symbol NamespaceFunctionOrConst <*> (Syntax.Identifier . Name.name <$> source)

globalDeclaration :: Assignment (Term Loc)
globalDeclaration = makeTerm <$> symbol GlobalDeclaration <*> children (Syntax.GlobalDeclaration <$> manyTerm simpleVariable')

simpleVariable :: Assignment (Term Loc)
simpleVariable = makeTerm <$> symbol SimpleVariable <*> children (Syntax.SimpleVariable <$> term (simpleVariable' <|> expression))

simpleVariable' :: Assignment (Term Loc)
simpleVariable' = choice [simpleVariable, variableName]


yieldExpression :: Assignment (Term Loc)
yieldExpression = makeTerm <$> symbol YieldExpression <*> children (Statement.Yield <$> term (arrayElementInitializer <|> expression))

arrayElementInitializer :: Assignment (Term Loc)
arrayElementInitializer = makeTerm <$> symbol ArrayElementInitializer <*> children (Literal.KeyValue <$> term expression <*> term expression) <|> (symbol ArrayElementInitializer *> children (term expression))

includeExpression :: Assignment (Term Loc)
includeExpression = makeTerm <$> symbol IncludeExpression <*> children (Syntax.Include <$> term expression)


includeOnceExpression :: Assignment (Term Loc)
includeOnceExpression = makeTerm <$> symbol IncludeOnceExpression <*> children (Syntax.IncludeOnce <$> term expression)

requireExpression :: Assignment (Term Loc)
requireExpression = makeTerm <$> symbol RequireExpression <*> children (Syntax.Require <$> term expression)


requireOnceExpression :: Assignment (Term Loc)
requireOnceExpression = makeTerm <$> symbol RequireOnceExpression <*> children (Syntax.RequireOnce <$> term expression)

variableName :: Assignment (Term Loc)
variableName = makeTerm <$> symbol VariableName <*> children (Syntax.VariableName <$> term name)

name :: Assignment (Term Loc)
name = makeTerm <$> (symbol Name <|> symbol Name') <*> (Syntax.Identifier . Name.name <$> source)

functionStaticDeclaration :: Assignment (Term Loc)
functionStaticDeclaration = makeTerm <$> symbol FunctionStaticDeclaration <*> children (Declaration.VariableDeclaration <$> manyTerm staticVariableDeclaration)

staticVariableDeclaration :: Assignment (Term Loc)
staticVariableDeclaration = makeTerm <$> symbol StaticVariableDeclaration <*> children (Statement.Assignment [] <$> term variableName <*> (term expression <|> emptyTerm))

comment :: Assignment (Term Loc)
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

string :: Assignment (Term Loc)
string = makeTerm <$> (symbol Grammar.String <|> symbol Heredoc) <*> (Literal.TextElement <$> source)


-- Helpers

append :: a -> [a] -> [a]
append x xs = xs <> [x]

bookend :: a -> [a] -> a -> [a]
bookend head_ list last_ = head_ : append last_ list

term :: Assignment (Term Loc) -> Assignment (Term Loc)
term term = contextualize (comment <|> textInterpolation) (postContextualize (comment <|> textInterpolation) term)

commentedTerm :: Assignment (Term Loc) -> Assignment (Term Loc)
commentedTerm term = contextualize (comment <|> textInterpolation) term <|> makeTerm1 <$> (Syntax.Context <$> some1 (comment <|> textInterpolation) <*> emptyTerm)

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment (Term Loc) -> Assignment [Term Loc]
manyTerm = many . commentedTerm

someTerm :: Assignment (Term Loc) -> Assignment [Term Loc]
someTerm = fmap NonEmpty.toList . someTerm'

someTerm' :: Assignment (Term Loc) -> Assignment (NonEmpty (Term Loc))
someTerm' = NonEmpty.some1 . commentedTerm

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment (Term Loc)
          -> Assignment (Term Loc)
          -> [Assignment (Term Loc -> Term Loc -> Sum PHP.Syntax (Term Loc))]
          -> Assignment (Sum PHP.Syntax (Term Loc))
infixTerm = infixContext (comment <|> textInterpolation)
