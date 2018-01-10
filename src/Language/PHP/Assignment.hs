{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.PHP.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Data.Union
import Data.Record
import qualified Data.Term as Term
import Data.Syntax (emptyTerm, handleError, parseError, infixContext, makeTerm, makeTerm', makeTerm1, contextualize, postContextualize)
import qualified Data.Syntax as Syntax
import qualified Language.PHP.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import Language.PHP.Grammar as Grammar
import Data.List.NonEmpty (some1)

type Syntax = '[
    Literal.TextElement
  , Comment.Comment
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Context
  , Syntax.Program
  , Syntax.Text
  , Statement.Assignment
  , Declaration.VariableDeclaration
  , Syntax.Identifier
  , Syntax.VariableName
  , Syntax.IncludeOnce
  , Syntax.Include
  , Syntax.RequireOnce
  , Syntax.Require
  , Statement.Yield
  , Syntax.SimpleVariable
  , Syntax.GlobalDeclaration
  , Syntax.ArrayElement
  , Syntax.CastType
  , Expression.Cast
  , Syntax.ErrorControl
  , Expression.Arithmetic
  , Expression.Boolean
  , Syntax.Clone
  , Literal.Integer
  , Literal.Float
  , Syntax.ShellCommand
  , Syntax.Update
  , Syntax.NamespaceName
  , Syntax.QualifiedName
  , Syntax.RelativeScope
  , Syntax.NewVariable
  , Syntax.ClassConstDeclaration
  , Syntax.ClassInterfaceClause
  , Declaration.Class
  , Syntax.ClassBaseClause
  , Syntax.ScalarType
  , Syntax.BaseTypeDeclaration
  , Syntax.TypeDeclaration
  , Syntax.ReturnType
  , Syntax.UseClause
  , Syntax.PrintIntrinsic
  , Syntax.IssetIntrinsic
  , Syntax.EvalIntrinsic
  , Syntax.EmptyIntrinsic
  , Syntax.ExitIntrinsic
  , Type.Annotation
  , Declaration.Function
  , Expression.New
  , Literal.Array
  , Expression.MemberAccess
  , Expression.Subscript
  , Expression.Call
  , Statement.If
  , Expression.InstanceOf
  , Expression.Comparison
  , Expression.Bitwise
  , Syntax.NamespaceAliasingClause
  , Syntax.NamespaceUseGroupClause
  , Syntax.NamespaceUseClause
  , Syntax.NamespaceUseDeclaration
  , Syntax.Namespace
  , Syntax.AliasAs
  , Syntax.InsteadOf
  , Syntax.TraitUseSpecification
  , Syntax.TraitUseClause
  , Syntax.ClassModifier
  , Syntax.Static
  , Syntax.DestructorDeclaration
  , Syntax.ConstructorDeclaration
  , Syntax.TraitDeclaration
  , [] ]

type Term = Term.Term (Data.Union.Union Syntax) (Record Location)
type Assignment = Assignment.Assignment [] Grammar Term

-- | Assignment from AST in TypeScript’s grammar onto a program in TypeScript’s syntax.
assignment :: Assignment
assignment = handleError $ makeTerm <$> symbol Program <*> children (Syntax.Program <$> ((\a b c -> a : b ++ [c]) <$> (text <|> emptyTerm) <*> manyTerm statement <*> (text <|> emptyTerm))) <|> parseError

term :: Assignment -> Assignment
term term = contextualize comment (postContextualize comment term)

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
manyTerm term = many (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

someTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
someTerm term = some (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

text :: Assignment
text = makeTerm <$> symbol Text <*> (Syntax.Text <$> source)

statement :: Assignment
statement = handleError everything
  where
    everything = choice [
      compoundStatement
  -- , namedLabelStatement
  -- , expressionStatement
  -- , selectionStatement
  -- , jumpStatement
  -- , tryStatement
  -- , declareStatement
  -- , echoStatement
  -- , constDeclaration
  -- , functionDefinition
  -- , classDeclaration
  -- , interfaceDeclaration
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
  unaryExpression,
  binaryExpression,
  includeExpression,
  includeOnceExpression,
  requireExpression,
  requireOnceExpression
  ]

unaryExpression :: Assignment
unaryExpression = choice [
  cloneExpression,
  primaryExpression,
  exponentiationExpression,
  unaryOpExpression,
  castExpression
  ]

assignmentExpression :: Assignment
assignmentExpression = makeTerm <$> symbol AssignmentExpression <*> children (Statement.Assignment [] <$> (variable <|> list) <*> (expression <|> variable))

augmentedAssignmentExpression :: Assignment
augmentedAssignmentExpression = makeTerm' <$> symbol AugmentedAssignmentExpression <*> children (infixTerm variable expression [
  assign Expression.Power <$ symbol AnonStarStarEqual
  , assign Expression.Times <$ symbol AnonStarEqual
  , assign Expression.DividedBy <$ symbol AnonSlashEqual
  , assign Expression.Plus <$ symbol AnonPlusEqual
  , assign Expression.Times <$ symbol AnonDotEqual
  , assign Expression.LShift <$ symbol AnonLAngleLAngleEqual
  , assign Expression.RShift <$ symbol AnonRAngleRAngleEqual
  , assign Expression.BAnd <$ symbol AnonAmpersandEqual
  , assign Expression.BXOr <$ symbol AnonCaretEqual
  , assign Expression.BOr <$ symbol AnonPipeEqual ])
  where
    assign c l r = inj (Statement.Assignment [] l (makeTerm1 (c l r)))

binaryExpression  :: Assignment
binaryExpression = makeTerm' <$> symbol BinaryExpression <*> children (infixTerm (expression <|> unaryExpression) (term (expression <|> classTypeDesignator))
  [ (inj .) . Expression.And              <$ symbol AnonAnd
  , (inj .) . Expression.Or               <$ symbol AnonOr
  , (inj .) . Expression.XOr              <$ symbol AnonXor
  , (inj .) . Expression.Or               <$ symbol AnonPipePipe
  , (inj .) . Expression.And              <$ symbol AnonAmpersandAmpersand
  , (inj .) . Expression.BOr              <$ symbol AnonPipe
  , (inj .) . Expression.BXOr             <$ symbol AnonCaret
  , (inj .) . Expression.BAnd             <$ symbol AnonAmpersand
  , (inj .) . Expression.Or               <$ symbol AnonQuestionQuestion -- Not sure if this is right.
  , (inj .) . Expression.Equal            <$ (symbol AnonEqualEqual <|> symbol AnonEqualEqualEqual)
  , (inj .) . invert Expression.Equal     <$ (symbol AnonBangEqual <|> symbol AnonLAngleRAngle <|> symbol AnonBangEqualEqual)
  , (inj .) . Expression.LessThan         <$ symbol AnonLAngle
  , (inj .) . Expression.GreaterThan      <$ symbol AnonRAngle
  , (inj .) . Expression.LessThanEqual    <$ symbol AnonLAngleEqual
  , (inj .) . Expression.GreaterThanEqual <$ symbol AnonRAngleEqual
  , (inj .) . Expression.Comparison       <$ symbol AnonLAngleEqualRAngle
  , (inj .) . Expression.LShift           <$ symbol AnonLAngleLAngle
  , (inj .) . Expression.RShift           <$ symbol AnonRAngleRAngle
  , (inj .) . Expression.Plus             <$ symbol AnonPlus
  , (inj .) . Expression.Minus            <$ symbol AnonMinus
  , (inj .) . Expression.Times            <$ (symbol AnonStar <|> symbol AnonDot)
  , (inj .) . Expression.DividedBy        <$ symbol AnonSlash
  , (inj .) . Expression.Modulo           <$ symbol AnonPercent
  , (inj .) . Expression.InstanceOf       <$ symbol AnonInstanceof
  ]) where invert cons a b = Expression.Not (makeTerm1 (cons a b))

conditionalExpression :: Assignment
conditionalExpression = makeTerm <$> symbol ConditionalExpression <*> children (Statement.If <$> (binaryExpression <|> unaryExpression) <*> (expression <|> emptyTerm) <*> expression)

list :: Assignment
list = makeTerm <$> symbol ListLiteral <*> children (Literal.Array <$> manyTerm (list <|> variable))

exponentiationExpression :: Assignment
exponentiationExpression = makeTerm <$> symbol ExponentiationExpression <*> children (Expression.Power <$> (cloneExpression <|> primaryExpression) <*> (primaryExpression <|> cloneExpression <|> exponentiationExpression))

cloneExpression :: Assignment
cloneExpression = makeTerm <$> symbol CloneExpression <*> children (Syntax.Clone <$> primaryExpression)

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
  expression
  ]

classConstantAccessExpression :: Assignment
classConstantAccessExpression = makeTerm <$> symbol ClassConstantAccessExpression <*> children (Expression.MemberAccess <$> scopeResolutionQualifier <*> name)

variable :: Assignment
variable = callableVariable <|> scopedPropertyAccessExpression <|> memberAccessExpression

callableVariable :: Assignment
callableVariable = choice [
  simpleVariable,
  subscriptExpression,
  memberCallExpression,
  scopedCallExpression,
  functionCallExpression
  ]

memberCallExpression :: Assignment
memberCallExpression = makeTerm <$> symbol MemberCallExpression <*> children (Expression.Call [] <$> (makeMemberAccess <$> location <*> dereferencableExpression <*> memberName) <*> arguments <*> emptyTerm)
  where makeMemberAccess loc expr memberName = makeTerm loc (Expression.MemberAccess expr memberName)

scopedCallExpression :: Assignment
scopedCallExpression = makeTerm <$> symbol ScopedCallExpression <*> children (Expression.Call [] <$> (makeMemberAccess <$> location <*> dereferencableExpression <*> memberName) <*> arguments <*> emptyTerm)
  where makeMemberAccess loc expr memberName = makeTerm loc (Expression.MemberAccess expr memberName)

functionCallExpression :: Assignment
functionCallExpression = makeTerm <$> symbol FunctionCallExpression <*> children (Expression.Call [] <$> (qualifiedName <|> callableExpression) <*> arguments <*> emptyTerm)

callableExpression :: Assignment
callableExpression = choice [
  callableVariable,
  expression,
  arrayCreationExpression,
  string
  ]

subscriptExpression :: Assignment
subscriptExpression = makeTerm <$> symbol SubscriptExpression <*> children (Expression.Subscript <$> dereferencableExpression <*> (pure <$> (expression <|> emptyTerm)))

memberAccessExpression :: Assignment
memberAccessExpression = makeTerm <$> symbol MemberAccessExpression <*> children (Expression.MemberAccess <$> dereferencableExpression <*> memberName)

dereferencableExpression :: Assignment
dereferencableExpression = symbol DereferencableExpression *> children (variable <|> expression <|> arrayCreationExpression <|> string)

scopedPropertyAccessExpression :: Assignment
scopedPropertyAccessExpression = makeTerm <$> symbol ScopedPropertyAccessExpression <*> children (Expression.MemberAccess <$> scopeResolutionQualifier <*> simpleVariable)

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
emptyIntrinsic = makeTerm <$> symbol EmptyIntrinsic <*> children (Syntax.EmptyIntrinsic <$> expression)

evalIntrinsic :: Assignment
evalIntrinsic = makeTerm <$> symbol EvalIntrinsic <*> children (Syntax.EvalIntrinsic <$> expression)

exitIntrinsic :: Assignment
exitIntrinsic = makeTerm <$> symbol ExitIntrinsic <*> children (Syntax.ExitIntrinsic <$> expression)

issetIntrinsic :: Assignment
issetIntrinsic = makeTerm <$> symbol IssetIntrinsic <*> children (Syntax.IssetIntrinsic <$> expression)

printIntrinsic :: Assignment
printIntrinsic = makeTerm <$> symbol PrintIntrinsic <*> children (Syntax.PrintIntrinsic <$> expression)

anonymousFunctionCreationExpression :: Assignment
anonymousFunctionCreationExpression = makeTerm <$> symbol AnonymousFunctionCreationExpression <*> children (makeFunction <$> emptyTerm <*> parameters <*> functionUseClause <*> returnType <*> compoundStatement)
  where
    makeFunction identifier parameters functionUseClause returnType statement = Declaration.Function [functionUseClause, returnType] identifier parameters statement

parameters :: Assignment.Assignment [] Grammar [Term]
parameters = manyTerm (simpleParameter <|> variadicParameter)

simpleParameter :: Assignment
simpleParameter = makeTerm <$> symbol SimpleParameter <*> children (makeAnnotation <$> (typeDeclaration <|> emptyTerm) <*> (makeAssignment <$> location <*> variableName <*> (defaultArgumentSpecifier <|> emptyTerm)))
  where
    makeAnnotation typeDecl assignment = Type.Annotation assignment typeDecl
    makeAssignment loc name argument = makeTerm loc (Statement.Assignment [] name argument)

defaultArgumentSpecifier :: Assignment
defaultArgumentSpecifier = symbol DefaultArgumentSpecifier *> children expression


variadicParameter :: Assignment
variadicParameter = makeTerm <$> symbol VariadicParameter <*> children (makeTypeAnnotation <$> (typeDeclaration <|> emptyTerm) <*> variableName)
  where makeTypeAnnotation ty variableName = (Type.Annotation variableName ty)

functionUseClause :: Assignment
functionUseClause = makeTerm <$> symbol AnonymousFunctionUseClause <*> children (Syntax.UseClause <$> someTerm variableName)

returnType :: Assignment
returnType = makeTerm <$> symbol ReturnType <*> children (Syntax.ReturnType <$> (typeDeclaration <|> emptyTerm))

typeDeclaration :: Assignment
typeDeclaration = makeTerm <$> symbol TypeDeclaration <*> children (Syntax.TypeDeclaration <$> baseTypeDeclaration)

baseTypeDeclaration :: Assignment
baseTypeDeclaration = makeTerm <$> symbol BaseTypeDeclaration <*> children (Syntax.BaseTypeDeclaration <$> (scalarType <|> qualifiedName <|> emptyTerm))

scalarType :: Assignment
scalarType = makeTerm <$> symbol ScalarType <*> (Syntax.ScalarType <$> source)

compoundStatement :: Assignment
compoundStatement = makeTerm <$> symbol CompoundStatement <*> children (manyTerm statement)

objectCreationExpression :: Assignment
objectCreationExpression = (makeTerm <$> symbol ObjectCreationExpression <*> children (fmap Expression.New $ ((:) <$> classTypeDesignator <*> (arguments <|> pure []))))

  <|> (makeTerm <$> symbol ObjectCreationExpression <*> children (makeAnonClass <$ token AnonNew <* token AnonClass <*> emptyTerm <*> (arguments <|> pure []) <*> (classBaseClause <|> emptyTerm) <*> (classInterfaceClause <|> emptyTerm) <*> (makeTerm <$> location <*> manyTerm classMemberDeclaration)))
  where makeAnonClass identifier args baseClause interfaceClause declarations = (Declaration.Class [] identifier (args ++ [baseClause, interfaceClause]) declarations)

classMemberDeclaration :: Assignment
classMemberDeclaration = choice [
  classConstDeclaration,
  -- propertyDeclaration,
  -- methodDeclaration,
  constructorDeclaration,
  destructorDeclaration,
  traitUseClause
  ]

classBaseClause :: Assignment
classBaseClause = makeTerm <$> symbol ClassBaseClause <*> (Syntax.ClassBaseClause <$> qualifiedName)

classInterfaceClause :: Assignment
classInterfaceClause = makeTerm <$> symbol ClassInterfaceClause <*> (Syntax.ClassInterfaceClause <$> someTerm qualifiedName)

classConstDeclaration :: Assignment
classConstDeclaration = makeTerm <$> symbol ClassConstDeclaration <*> (Syntax.ClassConstDeclaration <$> (visibilityModifier <|> emptyTerm) <*> manyTerm constElement)

visibilityModifier :: Assignment
visibilityModifier = makeTerm <$> symbol VisibilityModifier <*> (Syntax.Identifier <$> source)

constElement :: Assignment
constElement = makeTerm <$> symbol ConstElement <*> (Statement.Assignment [] <$> name <*> expression)

arguments :: Assignment.Assignment [] Grammar [Term]
arguments = symbol Arguments *> children (manyTerm (variadicUnpacking <|> expression))

variadicUnpacking :: Assignment
variadicUnpacking = symbol VariadicUnpacking *> children (term expression)

classTypeDesignator :: Assignment
classTypeDesignator = qualifiedName <|> newVariable

newVariable :: Assignment
newVariable = makeTerm <$> symbol NewVariable <*> children (Syntax.NewVariable <$> ((pure <$> simpleVariable) <|> ((\a b -> [a, b]) <$> (newVariable <|> qualifiedName <|> relativeScope) <*> (expression <|> memberName <|> emptyTerm))))

memberName :: Assignment
memberName = name <|> simpleVariable <|> expression

relativeScope :: Assignment
relativeScope = makeTerm <$> symbol RelativeScope <*> (Syntax.RelativeScope <$> source)

qualifiedName :: Assignment
qualifiedName = makeTerm <$> symbol QualifiedName <*> children (Syntax.QualifiedName <$> (namespaceNameAsPrefix <|> emptyTerm) <*> name)

namespaceNameAsPrefix :: Assignment
namespaceNameAsPrefix = symbol NamespaceNameAsPrefix *> children (namespaceName <|> emptyTerm)

namespaceName :: Assignment
namespaceName = makeTerm <$> symbol NamespaceName <*> children (Syntax.NamespaceName <$> someTerm name)

-- anonymousClass :: Assignment
-- anonymousClass = makeTerm <$> symbol Grammar.AnonymousClass <*> children (Declaration.Class <$> pure [] <*> emptyTerm <*> (classHeritage' <|> pure []) <*> classBodyStatements)

updateExpression :: Assignment
updateExpression = makeTerm <$> symbol UpdateExpression <*> children (Syntax.Update <$> term expression)

shellCommandExpression :: Assignment
shellCommandExpression = makeTerm <$> symbol ShellCommandExpression <*> children (Syntax.ShellCommand <$> source)

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
castExpression = makeTerm <$> symbol CastExpression <*> children (flip Expression.Cast <$> castType <*> unaryExpression)

castType :: Assignment
castType = makeTerm <$> symbol CastType <*> (Syntax.CastType <$> source)

traitDeclaration :: Assignment
traitDeclaration = makeTerm <$> symbol TraitDeclaration <*> children (Syntax.TraitDeclaration <$> name <*> manyTerm traitMemberDeclaration)

traitMemberDeclaration :: Assignment
traitMemberDeclaration = choice [
  -- propertyDeclaration,
  -- methodDeclaration,
  constructorDeclaration,
  destructorDeclaration,
  makeTerm <$> location <*> someTerm traitUseClause
  ]

constructorDeclaration :: Assignment
constructorDeclaration = makeTerm <$> symbol ConstructorDeclaration <*> children (Syntax.ConstructorDeclaration <$> someTerm methodModifier <*> parameters <*> compoundStatement)

destructorDeclaration :: Assignment
destructorDeclaration = makeTerm <$> symbol DestructorDeclaration <*> children (Syntax.DestructorDeclaration <$> someTerm methodModifier <*> compoundStatement)

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
traitUseClause = makeTerm <$> symbol TraitUseClause <*> children (Syntax.TraitUseClause <$> someTerm qualifiedName <*> traitUseSpecification)

traitUseSpecification :: Assignment
traitUseSpecification = makeTerm <$> symbol TraitUseSpecification <*> children (Syntax.TraitUseSpecification <$> manyTerm traitSelectAndAliasClause)

traitSelectAndAliasClause :: Assignment
traitSelectAndAliasClause = traitSelectInsteadOfClause <|> traitAliasAsClause

traitSelectInsteadOfClause :: Assignment
traitSelectInsteadOfClause = makeTerm <$> symbol TraitSelectInsteadOfClause <*> children (Syntax.InsteadOf <$> name <*> name)

traitAliasAsClause :: Assignment
traitAliasAsClause = makeTerm <$> symbol TraitAliasAsClause <*> children (Syntax.AliasAs <$> name <*> (visibilityModifier <|> emptyTerm) <*> (name <|> emptyTerm))

namespaceDefinition :: Assignment
namespaceDefinition = makeTerm <$> symbol NamespaceDefinition <*> children (Syntax.Namespace <$> (name <|> emptyTerm) <*> (compoundStatement <|> emptyTerm))

namespaceUseDeclaration :: Assignment
namespaceUseDeclaration = makeTerm <$> symbol NamespaceUseDeclaration <*> children (Syntax.NamespaceUseDeclaration <$>
  (((++) <$> (pure <$> (namespaceFunctionOrConst <|> emptyTerm)) <*> someTerm namespaceUseClause) <|> ((\a b cs -> a : b : cs) <$> namespaceFunctionOrConst <*> namespaceName <*> someTerm namespaceUseGroupClause1) <|> ((:) <$> namespaceName <*> someTerm namespaceUseGroupClause2)))

namespaceUseClause :: Assignment
namespaceUseClause = makeTerm <$> symbol NamespaceUseClause <*> children (fmap Syntax.NamespaceUseClause $ (\a b -> [a, b]) <$> namespaceName <*> (namespaceAliasingClause <|> emptyTerm))

namespaceUseGroupClause1 :: Assignment
namespaceUseGroupClause1 = makeTerm <$> symbol NamespaceUseGroupClause_1 <*> children (fmap Syntax.NamespaceUseGroupClause $ (\a b -> [a, b]) <$> namespaceName <*> (namespaceAliasingClause <|> emptyTerm))

namespaceUseGroupClause2 :: Assignment
namespaceUseGroupClause2 = makeTerm <$> symbol NamespaceUseGroupClause_2 <*> children (fmap Syntax.NamespaceUseGroupClause $ (\a b c -> [a, b, c]) <$> (namespaceFunctionOrConst <|> emptyTerm) <*> namespaceName <*> (namespaceAliasingClause <|> emptyTerm))

namespaceAliasingClause :: Assignment
namespaceAliasingClause = makeTerm <$> symbol NamespaceAliasingClause <*> children (Syntax.NamespaceAliasingClause <$> name)

-- | TODO Do something better than Identifier
namespaceFunctionOrConst :: Assignment
namespaceFunctionOrConst = makeTerm <$> symbol NamespaceFunctionOrConst <*> (Syntax.Identifier <$> source)

globalDeclaration :: Assignment
globalDeclaration = makeTerm <$> symbol GlobalDeclaration <*> children (Syntax.GlobalDeclaration <$> manyTerm simpleVariable)

simpleVariable :: Assignment
simpleVariable = makeTerm <$> symbol SimpleVariable <*> children (Syntax.SimpleVariable <$> (variableName <|> simpleVariable <|> expression))


yieldExpression :: Assignment
yieldExpression = makeTerm <$> symbol YieldExpression <*> children (Statement.Yield <$> (arrayElementInitializer <|> expression))

arrayElementInitializer :: Assignment
arrayElementInitializer = makeTerm <$> symbol ArrayElementInitializer <*> children (Syntax.ArrayElement <$> (expression
  -- <|> KeyValue <$> expression <*> expression
  ))

includeExpression :: Assignment
includeExpression = makeTerm <$> symbol IncludeExpression <*> children (Syntax.Include <$> expression)


includeOnceExpression :: Assignment
includeOnceExpression = makeTerm <$> symbol IncludeOnceExpression <*> children (Syntax.IncludeOnce <$> expression)

requireExpression :: Assignment
requireExpression = makeTerm <$> symbol RequireExpression <*> children (Syntax.Require <$> expression)


requireOnceExpression :: Assignment
requireOnceExpression = makeTerm <$> symbol RequireOnceExpression <*> children (Syntax.RequireOnce <$> expression)

variableName :: Assignment
variableName = makeTerm <$> symbol VariableName <*> children (Syntax.VariableName <$> name)

name :: Assignment
name = makeTerm <$> symbol Name <*> (Syntax.Identifier <$> source)

functionStaticDeclaration :: Assignment
functionStaticDeclaration = makeTerm <$> symbol FunctionStaticDeclaration <*> children (Declaration.VariableDeclaration . pure <$> staticVariableDeclaration)

staticVariableDeclaration :: Assignment
staticVariableDeclaration = makeTerm <$> symbol StaticVariableDeclaration <*> children (Statement.Assignment <$> pure [] <*> variableName <*> (expression <|> emptyTerm))

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

string :: Assignment
string = makeTerm <$> symbol Grammar.String <*> (Literal.TextElement <$> source)

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment
          -> Assignment
          -> [Assignment.Assignment [] Grammar (Term -> Term -> Data.Union.Union Syntax Term)]
          -> Assignment.Assignment [] Grammar (Data.Union.Union Syntax Term)
infixTerm = infixContext comment
