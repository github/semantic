{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.TypeScript.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Data.Maybe (fromMaybe, catMaybes)
import Data.Record
import Data.Syntax (emptyTerm, handleError, parseError, infixContext, makeTerm, makeTerm', makeTerm1, postContextualize)
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment hiding (Assignment, Error)
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import Data.Union
import GHC.Stack
import Language.TypeScript.Grammar as Grammar
import qualified Language.TypeScript.Syntax as TypeScript.Syntax
import qualified Data.Term as Term
import Data.List.NonEmpty (some1)
import Data.Function (on)
import Data.Foldable (toList)

-- | The type of TypeScript syntax.
type Syntax = '[
    Comment.Comment
  , Declaration.Class
  , Declaration.Function
  , Declaration.Method
  , Declaration.InterfaceDeclaration
  , Declaration.PublicFieldDefinition
  , Declaration.VariableDeclaration
  , Declaration.TypeAliasDeclaration
  , Declaration.Import
  , Declaration.Module
  , Expression.Arithmetic
  , Expression.Bitwise
  , Expression.Boolean
  , Expression.Call
  , Expression.Cast
  , Expression.Comparison
  , Expression.Enumeration
  , Expression.MemberAccess
  , Expression.NonNullExpression
  , Expression.ScopeResolution
  , Expression.SequenceExpression
  , Expression.Subscript
  , Expression.Delete
  , Expression.Void
  , Expression.Typeof
  , Expression.InstanceOf
  , Expression.New
  , Expression.Await
  , Literal.Array
  , Literal.Boolean
  , Literal.Float
  , Literal.Hash
  , Literal.Integer
  , Literal.KeyValue
  , Literal.Null
  , Literal.String
  , Literal.TextElement
  , Literal.Regex
  , Statement.Assignment
  , Statement.Break
  , Statement.Catch
  , Statement.Continue
  , Statement.Else
  , Statement.Finally
  , Statement.For
  , Statement.ForEach
  , Statement.If
  , Statement.Match
  , Statement.Pattern
  , Statement.Retry
  , Statement.Return
  , Statement.ScopeEntry
  , Statement.ScopeExit
  , Statement.Try
  , Statement.While
  , Statement.Yield
  , Statement.Throw
  , Statement.DoWhile
  , Syntax.AccessibilityModifier
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Identifier
  , Syntax.Program
  , Syntax.Context
  , Type.Readonly
  , Type.TypeParameters
  , TypeScript.Syntax.TypeParameter
  , TypeScript.Syntax.Constraint
  , TypeScript.Syntax.ParenthesizedType
  , TypeScript.Syntax.PredefinedType
  , TypeScript.Syntax.TypeIdentifier
  , TypeScript.Syntax.NestedIdentifier
  , TypeScript.Syntax.NestedTypeIdentifier
  , TypeScript.Syntax.GenericType
  , TypeScript.Syntax.TypeArguments
  , TypeScript.Syntax.TypePredicate
  , TypeScript.Syntax.CallSignature
  , TypeScript.Syntax.ConstructSignature
  , TypeScript.Syntax.ArrayType
  , TypeScript.Syntax.FlowMaybeType
  , TypeScript.Syntax.TypeQuery
  , TypeScript.Syntax.IndexTypeQuery
  , TypeScript.Syntax.ThisType
  , TypeScript.Syntax.ExistentialType
  , TypeScript.Syntax.MethodSignature
  , TypeScript.Syntax.IndexSignature
  , TypeScript.Syntax.ObjectType
  , TypeScript.Syntax.LiteralType
  , TypeScript.Syntax.Union
  , TypeScript.Syntax.Intersection
  , TypeScript.Syntax.FunctionType
  , TypeScript.Syntax.Tuple
  , TypeScript.Syntax.Constructor
  , TypeScript.Syntax.TypeAssertion
  , TypeScript.Syntax.ImportAlias
  , TypeScript.Syntax.Debugger
  , TypeScript.Syntax.ShorthandPropertyIdentifier
  , TypeScript.Syntax.InternalModule
  , TypeScript.Syntax.Super
  , TypeScript.Syntax.Undefined
  , TypeScript.Syntax.ClassHeritage
  , TypeScript.Syntax.AbstractClass
  , TypeScript.Syntax.ExtendsClause
  , TypeScript.Syntax.ImplementsClause
  , TypeScript.Syntax.JsxElement
  , TypeScript.Syntax.JsxSelfClosingElement
  , TypeScript.Syntax.JsxOpeningElement
  , TypeScript.Syntax.JsxText
  , TypeScript.Syntax.JsxClosingElement
  , TypeScript.Syntax.JsxExpression
  , TypeScript.Syntax.JsxAttribute
  , TypeScript.Syntax.OptionalParameter
  , TypeScript.Syntax.RequiredParameter
  , TypeScript.Syntax.RestParameter
  , TypeScript.Syntax.PropertySignature
  , TypeScript.Syntax.ImportExportSpecifier
  , TypeScript.Syntax.ExportClause
  , TypeScript.Syntax.Export
  , TypeScript.Syntax.AmbientDeclaration
  , TypeScript.Syntax.EnumDeclaration
  , TypeScript.Syntax.ExtendsClause
  , TypeScript.Syntax.AmbientFunction
  , TypeScript.Syntax.ImportRequireClause
  , TypeScript.Syntax.ImportClause
  , TypeScript.Syntax.LabeledStatement
  , TypeScript.Syntax.NamedImports
  , TypeScript.Syntax.NamespaceImport
  , TypeScript.Syntax.Annotation
  , TypeScript.Syntax.With
  , TypeScript.Syntax.ForOf
  , TypeScript.Syntax.This
  , TypeScript.Syntax.Update
  , TypeScript.Syntax.ComputedPropertyName
  , TypeScript.Syntax.Decorator
  , []
  ]

type Term = Term.Term (Data.Union.Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment [] Grammar Term

-- | Assignment from AST in Ruby’s grammar onto a program in TypeScript’s syntax.
assignment :: Assignment
assignment = makeTerm <$> symbol Program <*> children (Syntax.Program <$> many statement) <|> parseError

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
term :: Assignment -> Assignment
term term = many comment *> term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm)

expression :: Assignment
expression = term (handleError everything)
  where
    everything = choice [
      typeAssertion,
      asExpression,
      nonNullExpression',
      importAlias',
      internalModule,
      super,
      object,
      array,
      jsxElement,
      jsxSelfClosingElement,
      class',
      anonymousClass,
      function,
      arrowFunction,
      assignmentExpression,
      augmentedAssignmentExpression,
      awaitExpression,
      unaryExpression,
      binaryExpression,
      ternaryExpression,
      updateExpression,
      callExpression,
      memberExpression,
      newExpression,
      parenthesizedExpression,
      subscriptExpression,
      yieldExpression,
      thisExpression,
      number,
      string,
      templateString,
      regex,
      true,
      false,
      null',
      undefined',
      identifier
      ]

undefined' :: Assignment
undefined' = makeTerm <$> symbol Grammar.Undefined <*> (TypeScript.Syntax.Undefined <$ source)

assignmentExpression :: Assignment
assignmentExpression = makeTerm <$> symbol AssignmentExpression <*> children (Statement.Assignment [] <$> (memberExpression <|> subscriptExpression <|> identifier <|> destructuringPattern) <*> expression)

augmentedAssignmentExpression :: Assignment
augmentedAssignmentExpression = makeTerm' <$> symbol AugmentedAssignmentExpression <*> children (infixTerm (memberExpression <|> subscriptExpression <|> identifier <|> destructuringPattern) expression [
  assign Expression.Plus <$ symbol AnonPlusEqual
  , assign Expression.Minus <$ symbol AnonMinusEqual
  , assign Expression.Times <$ symbol AnonStarEqual
  , assign Expression.DividedBy <$ symbol AnonSlashEqual
  , assign Expression.Modulo <$ symbol AnonPercentEqual
  , assign Expression.BXOr <$ symbol AnonCaretEqual
  , assign Expression.BAnd <$ symbol AnonAmpersandEqual
  , assign Expression.BOr <$ symbol AnonPipeEqual ])
  where assign :: f :< Syntax => (Term -> Term -> f Term) -> Term -> Term -> Data.Union.Union Syntax Term
        assign c l r = inj (Statement.Assignment [] l (makeTerm1 (c l r)))


awaitExpression :: Assignment
awaitExpression = makeTerm <$> symbol Grammar.AwaitExpression <*> children (Expression.Await <$> expression)

unaryExpression :: Assignment
unaryExpression = symbol Grammar.UnaryExpression >>= \ loc ->
  makeTerm loc . Expression.Not <$> children ((symbol AnonTilde <|> symbol AnonBang) *> expression)
  <|> makeTerm loc . Expression.Negate <$> children ((symbol AnonMinus <|> symbol AnonPlus) *> expression)
  <|> makeTerm loc . Expression.Typeof <$> children (symbol AnonTypeof *> expression)
  <|> makeTerm loc . Expression.Void <$> children (symbol AnonVoid *> expression)
  <|> makeTerm loc . Expression.Delete <$> children (symbol AnonDelete *> expression)

ternaryExpression :: Assignment
ternaryExpression = makeTerm <$> symbol Grammar.TernaryExpression <*> children (Statement.If <$> expression <*> expression <*> expression)

memberExpression :: Assignment
memberExpression = makeTerm <$> (symbol Grammar.MemberExpression <|> symbol Grammar.MemberExpression') <*> children (Expression.MemberAccess <$> postContextualize comment expression <*> propertyIdentifier)

newExpression :: Assignment
newExpression = makeTerm <$> symbol Grammar.NewExpression <*> children (Expression.New <$> expression)

updateExpression :: Assignment
updateExpression = makeTerm <$> symbol Grammar.UpdateExpression <*> children (TypeScript.Syntax.Update <$> expression)

yieldExpression :: Assignment
yieldExpression = makeTerm <$> symbol Grammar.YieldExpression <*> children (Statement.Yield <$> (expression <|> emptyTerm))

thisExpression :: Assignment
thisExpression = makeTerm <$> symbol Grammar.ThisExpression <*> (TypeScript.Syntax.This <$ source)

regex :: Assignment
regex = makeTerm <$> symbol Grammar.Regex <*> (Literal.Regex <$> source)

null' :: Assignment
null' = makeTerm <$> symbol Null <*> (Literal.Null <$ source)

anonymousClass :: Assignment
anonymousClass = makeTerm <$> symbol Grammar.AnonymousClass <*> children (Declaration.Class <$> pure [] <*> emptyTerm <*> (classHeritage' <|> pure []) <*> classBodyStatements)

abstractClass :: Assignment
abstractClass = makeTerm <$> symbol Grammar.AbstractClass <*> (TypeScript.Syntax.AbstractClass <$> identifier <*> (typeParameters <|> emptyTerm) <*> (classHeritage' <|> pure []) <*> classBodyStatements)

classHeritage' :: HasCallStack => Assignment.Assignment [] Grammar [Term]
classHeritage' = symbol Grammar.ClassHeritage *> children (((++) `on` toList) <$> optional extendsClause' <*> optional implementsClause')

extendsClause' :: Assignment
extendsClause' = makeTerm <$> symbol Grammar.ExtendsClause <*> children (TypeScript.Syntax.ExtendsClause <$> many ty)

implementsClause' :: Assignment
implementsClause' = makeTerm <$> symbol Grammar.ImplementsClause <*> children (TypeScript.Syntax.ImplementsClause <$> many ty)

super :: Assignment
super = makeTerm <$> symbol Grammar.Super <*> (TypeScript.Syntax.Super <$ source)

typeAssertion :: Assignment
typeAssertion = makeTerm <$> symbol Grammar.TypeAssertion <*> children (TypeScript.Syntax.TypeAssertion <$> typeArguments' <*> expression)

asExpression :: Assignment
asExpression = makeTerm <$> symbol AsExpression <*> children (Expression.Cast <$> expression <*> (ty <|> templateString))

templateString :: Assignment
templateString = makeTerm <$> symbol TemplateString <*> children (Literal.String <$> many (term templateSubstitution))

templateSubstitution :: Assignment
templateSubstitution = symbol TemplateSubstitution *> children expression

nonNullExpression' :: Assignment
nonNullExpression' = makeTerm <$> symbol Grammar.NonNullExpression <*> children (Expression.NonNullExpression <$> expression)

importAlias' :: Assignment
importAlias' = makeTerm <$> symbol Grammar.ImportAlias <*> children (TypeScript.Syntax.ImportAlias <$> identifier <*> (identifier <|> nestedIdentifier))

number :: Assignment
number = makeTerm <$> symbol Grammar.Number <*> (Literal.Float <$> source)

string :: Assignment
string = makeTerm <$> symbol Grammar.String <*> (Literal.TextElement <$> source)

true :: Assignment
true = makeTerm <$> symbol Grammar.True <*> (Literal.true <$ source)

false :: Assignment
false = makeTerm <$> symbol Grammar.False <*> (Literal.false <$ source)

identifier :: Assignment
identifier = (makeTerm <$> symbol Identifier' <*> (Syntax.Identifier <$> source)) <|> (makeTerm <$> symbol Identifier <*> (Syntax.Identifier <$> source))

class' :: Assignment
class' = makeClass <$> symbol Class <*> children ((,,,,) <$> many (term decorator) <*> identifier <*> ((symbol TypeParameters *> children (many (term typeParameter'))) <|> pure []) <*> (classHeritage' <|> pure []) <*> classBodyStatements)
  where makeClass loc (decorators, expression, typeParams, classHeritage, statements) = makeTerm loc (Declaration.Class (decorators ++ typeParams) expression classHeritage statements)

object :: Assignment
object = makeTerm <$> (symbol Object <|> symbol ObjectPattern) <*> children (Literal.Hash <$> many (term ((pair <|> spreadElement <|> methodDefinition <|> assignmentPattern <|> shorthandPropertyIdentifier))))

array :: Assignment
array = makeTerm <$> (symbol Array <|> symbol ArrayPattern) <*> children (Literal.Array <$> many (term (expression <|> spreadElement)))

jsxElement :: Assignment
jsxElement = makeTerm <$> symbol Grammar.JsxElement <*> children (TypeScript.Syntax.JsxElement <$> jsxOpeningElement' <*> many (term (jsxElement <|> jsxSelfClosingElement <|> jsxExpression' <|> jsxText)) <*> jsxClosingElement')

jsxSelfClosingElement :: Assignment
jsxSelfClosingElement = makeTerm <$> symbol Grammar.JsxSelfClosingElement <*> children (TypeScript.Syntax.JsxSelfClosingElement <$> identifier <*> many (term (jsxAttribute <|> jsxExpression')))

jsxOpeningElement' :: Assignment
jsxOpeningElement' = makeTerm <$> symbol Grammar.JsxOpeningElement <*> children (TypeScript.Syntax.JsxOpeningElement <$> identifier <*> many (term (jsxAttribute <|> jsxExpression')))

jsxExpression' :: Assignment
jsxExpression' = makeTerm <$> symbol Grammar.JsxExpression <*> children (TypeScript.Syntax.JsxExpression <$> (expression <|> sequenceExpression <|> spreadElement))

jsxText :: Assignment
jsxText = makeTerm <$> symbol Grammar.JsxText <*> (TypeScript.Syntax.JsxText <$> source)

jsxClosingElement' :: Assignment
jsxClosingElement' = makeTerm <$> symbol Grammar.JsxClosingElement <*> children (TypeScript.Syntax.JsxClosingElement <$> identifier)

jsxAttribute :: Assignment
jsxAttribute = makeTerm <$> symbol Grammar.JsxAttribute <*> children (TypeScript.Syntax.JsxAttribute <$> propertyIdentifier <*> (number <|> string <|> jsxExpression'))

propertyIdentifier :: Assignment
propertyIdentifier = makeTerm <$> symbol PropertyIdentifier <*> (Syntax.Identifier <$> source)

sequenceExpression :: Assignment
sequenceExpression = makeTerm <$> symbol Grammar.SequenceExpression <*> children (Expression.SequenceExpression <$> expression <*> (sequenceExpression <|> expression))

parameter :: Assignment
parameter =
      requiredParameter
  <|> restParameter
  <|> optionalParameter

accessibilityModifier' :: Assignment
accessibilityModifier' = makeTerm <$> symbol AccessibilityModifier <*> children (Syntax.Identifier <$> source)

destructuringPattern :: Assignment
destructuringPattern = object <|> array

spreadElement :: Assignment
spreadElement = symbol SpreadElement *> children expression

readonly' :: Assignment
readonly' = makeTerm <$> symbol Readonly <*> (Type.Readonly <$ source)

methodDefinition :: Assignment
methodDefinition = makeMethod <$>
  symbol MethodDefinition
  <*> children ((,,,,,) <$> (accessibilityModifier' <|> emptyTerm) <*> (readonly' <|> emptyTerm) <*> emptyTerm <*> propertyName <*> callSignatureParts <*> statementBlock)
  where
    makeMethod loc (modifier, readonly, receiver, propertyName', (typeParameters', params, ty'), statements) = makeTerm loc (Declaration.Method [modifier, readonly, typeParameters', ty'] receiver propertyName' params statements)

callSignatureParts :: HasCallStack => Assignment.Assignment [] Grammar (Term, [Term], Term)
callSignatureParts =  symbol Grammar.CallSignature *> children ((,,) <$> (fromMaybe <$> emptyTerm <*> optional typeParameters) <*> formalParameters <*> (fromMaybe <$> emptyTerm <*> optional typeAnnotation'))

callSignature :: Assignment
callSignature =  makeTerm <$> symbol Grammar.CallSignature <*> children (TypeScript.Syntax.CallSignature <$> (fromMaybe <$> emptyTerm <*> optional typeParameters) <*> formalParameters <*> (fromMaybe <$> emptyTerm <*> optional typeAnnotation'))

constructSignature :: Assignment
constructSignature = makeTerm <$> symbol Grammar.ConstructSignature <*> children (TypeScript.Syntax.ConstructSignature <$> (fromMaybe <$> emptyTerm <*> optional typeParameters) <*> formalParameters <*> (fromMaybe <$> emptyTerm <*> optional typeAnnotation'))

indexSignature :: Assignment
indexSignature = makeTerm <$> symbol Grammar.IndexSignature <*> children (TypeScript.Syntax.IndexSignature <$> (identifier <|> typeAnnotation'))

methodSignature :: Assignment
methodSignature = makeMethodSignature <$> symbol Grammar.MethodSignature <*> children ((,,,) <$> (accessibilityModifier' <|> emptyTerm) <*> (readonly' <|> emptyTerm) <*> propertyName <*> callSignatureParts)
  where makeMethodSignature loc (modifier, readonly, propertyName, (typeParams, params, annotation)) = makeTerm loc (TypeScript.Syntax.MethodSignature [modifier, readonly, typeParams, annotation] propertyName params)

formalParameters :: HasCallStack => Assignment.Assignment [] Grammar [Term]
formalParameters = symbol FormalParameters *> children (concat <$> many ((\as b -> as ++ [b]) <$> many (term decorator) <*> term parameter))

decorator :: Assignment
decorator = makeTerm <$> symbol Grammar.Decorator <*> children (TypeScript.Syntax.Decorator <$> (identifier <|> memberExpression <|> callExpression))

typeParameters :: Assignment
typeParameters = makeTerm <$> symbol TypeParameters <*> children (Type.TypeParameters <$> many (term typeParameter'))

typeAnnotation' :: Assignment
typeAnnotation' = makeTerm <$> symbol TypeAnnotation <*> children (TypeScript.Syntax.Annotation <$> ty)

typeParameter' :: Assignment
typeParameter' = makeTerm <$> symbol Grammar.TypeParameter <*> children (TypeScript.Syntax.TypeParameter <$> identifier <*> (constraint <|> emptyTerm))

constraint :: Assignment
constraint = makeTerm <$> symbol Grammar.Constraint <*> children (TypeScript.Syntax.Constraint <$> ty)

function :: Assignment
function = makeFunction <$> (symbol Grammar.Function <|> symbol Grammar.GeneratorFunction) <*> children ((,,) <$> (identifier <|> emptyTerm) <*> callSignatureParts <*> statementBlock)
  where makeFunction loc (id, (typeParams, params, annotation), statements) = makeTerm loc (Declaration.Function [typeParams, annotation] id params statements)

ambientFunction :: Assignment
ambientFunction = makeAmbientFunction <$> symbol Grammar.AmbientFunction <*> children ((,) <$> identifier <*> callSignatureParts)
  where makeAmbientFunction loc (id, (typeParams, params, annotation)) = makeTerm loc (TypeScript.Syntax.AmbientFunction [typeParams, annotation] id params)

ty :: Assignment
ty = primaryType <|> unionType <|> intersectionType <|> functionTy <|> constructorTy

primaryType :: Assignment
primaryType = parenthesizedTy <|> predefinedTy <|> typeIdentifier <|> nestedTypeIdentifier <|> genericType <|> typePredicate <|> objectType <|> arrayTy <|> tupleType <|> flowMaybeTy <|> typeQuery <|> indexTypeQuery <|> thisType <|> existentialType <|> literalType

parenthesizedTy :: Assignment
parenthesizedTy = makeTerm <$> symbol Grammar.ParenthesizedType <*> children (TypeScript.Syntax.ParenthesizedType <$> ty)

predefinedTy :: Assignment
predefinedTy = makeTerm <$> symbol Grammar.PredefinedType <*> (TypeScript.Syntax.PredefinedType <$> source)

typeIdentifier :: Assignment
typeIdentifier = makeTerm <$> symbol Grammar.TypeIdentifier <*> (TypeScript.Syntax.TypeIdentifier <$> source)

nestedIdentifier :: Assignment
nestedIdentifier = makeTerm <$> symbol Grammar.NestedIdentifier <*> children (TypeScript.Syntax.NestedIdentifier <$> (identifier <|> nestedIdentifier) <*> identifier)

nestedTypeIdentifier :: Assignment
nestedTypeIdentifier = makeTerm <$> symbol Grammar.NestedTypeIdentifier <*> children (TypeScript.Syntax.NestedTypeIdentifier <$> (identifier <|> nestedIdentifier) <*> typeIdentifier)

genericType :: Assignment
genericType = makeTerm <$> symbol Grammar.GenericType <*> children (TypeScript.Syntax.GenericType <$> (typeIdentifier <|> nestedTypeIdentifier) <*> typeArguments')

typeArguments' :: Assignment
typeArguments' = makeTerm <$> symbol Grammar.TypeArguments <*> children (TypeScript.Syntax.TypeArguments <$> some (term ty))

typePredicate :: Assignment
typePredicate = makeTerm <$> symbol Grammar.TypePredicate <*> children (TypeScript.Syntax.TypePredicate <$> identifier <*> ty)

objectType :: Assignment
objectType = makeTerm <$> symbol Grammar.ObjectType <*> children (TypeScript.Syntax.ObjectType <$> many (term (exportStatement <|> propertySignature <|> callSignature <|> constructSignature <|> indexSignature <|> methodSignature)))

arrayTy :: Assignment
arrayTy = makeTerm <$> symbol Grammar.ArrayType <*> children (TypeScript.Syntax.ArrayType <$> ty)

flowMaybeTy :: Assignment
flowMaybeTy = makeTerm <$> symbol Grammar.FlowMaybeType <*> children (TypeScript.Syntax.FlowMaybeType <$> primaryType)

typeQuery :: Assignment
typeQuery = makeTerm <$> symbol Grammar.TypeQuery <*> children (TypeScript.Syntax.TypeQuery <$> (identifier <|> nestedIdentifier))

indexTypeQuery :: Assignment
indexTypeQuery = makeTerm <$> symbol Grammar.IndexTypeQuery <*> children (TypeScript.Syntax.IndexTypeQuery <$> (identifier <|> nestedIdentifier))

thisType :: Assignment
thisType = makeTerm <$> symbol Grammar.ThisType <*> (TypeScript.Syntax.ThisType <$> source)

existentialType :: Assignment
existentialType = makeTerm <$> symbol Grammar.ExistentialType <*> (TypeScript.Syntax.ExistentialType <$> source)

literalType :: Assignment
literalType = makeTerm <$> symbol Grammar.LiteralType <*> children (TypeScript.Syntax.LiteralType <$> (number <|> string <|> true <|> false))

unionType :: Assignment
unionType = makeTerm <$> symbol UnionType <*> children (TypeScript.Syntax.Union <$> ty <*> ty)

intersectionType :: Assignment
intersectionType = makeTerm <$> symbol IntersectionType <*> children (TypeScript.Syntax.Intersection <$> ty <*> ty)

functionTy :: Assignment
functionTy = makeTerm <$> symbol Grammar.FunctionType <*> children (TypeScript.Syntax.FunctionType <$> (fromMaybe <$> emptyTerm <*> optional typeParameters) <*> formalParameters <*> ty)

tupleType :: Assignment
tupleType = makeTerm <$> symbol TupleType <*> children (TypeScript.Syntax.Tuple <$> many (term ty))

constructorTy :: Assignment
constructorTy = makeTerm <$> symbol ConstructorType <*> children (TypeScript.Syntax.Constructor <$> (fromMaybe <$> emptyTerm <*> optional typeParameters) <*> formalParameters <*> ty)

statementBlock :: Assignment
statementBlock = makeTerm <$> symbol StatementBlock <*> children (many statement)

classBodyStatements :: HasCallStack => Assignment.Assignment [] Grammar [Term]
classBodyStatements = symbol ClassBody *> children (concat <$> many ((\as b -> as ++ [b]) <$> many (term decorator) <*> term (methodDefinition <|> publicFieldDefinition <|> methodSignature <|> indexSignature)))

publicFieldDefinition :: Assignment
publicFieldDefinition = makeField <$> symbol Grammar.PublicFieldDefinition <*> children ((,,,,) <$> (accessibilityModifier' <|> emptyTerm) <*> (readonly' <|> emptyTerm) <*> propertyName <*> (typeAnnotation' <|> emptyTerm) <*> (expression <|> emptyTerm))
  where makeField loc (modifier, readonly, propertyName, annotation, expression) = makeTerm loc (Declaration.PublicFieldDefinition [modifier, readonly, annotation] propertyName expression)


statement :: Assignment
statement = term (handleError everything)
  where
    everything = choice [
      exportStatement
      , importStatement
      , debuggerStatement
      , expressionStatement'
      , declaration
      , statementBlock
      , ifStatement
      , switchStatement
      , forStatement
      , forInStatement
      , forOfStatement
      , whileStatement
      , doStatement
      , tryStatement
      , withStatement
      , breakStatement
      , continueStatement
      , returnStatement
      , throwStatement
      , emptyStatement
      , labeledStatement ]

forOfStatement :: Assignment
forOfStatement = makeTerm <$> symbol ForOfStatement <*> children (TypeScript.Syntax.ForOf <$> expression <*> expression <*> statement)

forInStatement :: Assignment
forInStatement = makeTerm <$> symbol ForInStatement <*> children (Statement.ForEach <$> expression <*> expression <*> statement)

doStatement :: Assignment
doStatement = makeTerm <$> symbol DoStatement <*> children (flip Statement.DoWhile <$> statement <*> parenthesizedExpression)

continueStatement :: Assignment
continueStatement = makeTerm <$> symbol ContinueStatement <*> children (Statement.Continue <$> ((symbol StatementIdentifier *> identifier) <|> emptyTerm))

breakStatement :: Assignment
breakStatement = makeTerm <$> symbol BreakStatement <*> children (Statement.Break <$> ((symbol StatementIdentifier *> identifier) <|> emptyTerm))

withStatement :: Assignment
withStatement = makeTerm <$> symbol WithStatement <*> children (TypeScript.Syntax.With <$> parenthesizedExpression <*> statement)

returnStatement :: Assignment
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> (expression <|> sequenceExpression <|> emptyTerm))

throwStatement :: Assignment
throwStatement = makeTerm <$> symbol Grammar.ThrowStatement <*> children (Statement.Throw <$> (expression <|> sequenceExpression))

labeledStatement :: Assignment
labeledStatement = makeTerm <$> symbol Grammar.LabeledStatement <*> children (TypeScript.Syntax.LabeledStatement <$> (symbol StatementIdentifier *> children identifier) <*> statement)

importStatement :: Assignment
importStatement = makeTerm <$> symbol Grammar.ImportStatement <*> children (Declaration.Import <$> (((\a b -> [a, b]) <$> importClause <*> fromClause) <|> (pure <$> (importRequireClause <|> string))))

importClause :: Assignment
importClause = makeTerm <$> symbol Grammar.ImportClause <*> children (TypeScript.Syntax.ImportClause <$> (((\a b -> [a, b]) <$> identifier <*> (namespaceImport <|> namedImports)) <|> (pure <$> (namespaceImport <|> namedImports <|> identifier))))

namedImports :: Assignment
namedImports = makeTerm <$> symbol Grammar.NamedImports <*> children (TypeScript.Syntax.NamedImports <$> many (term importExportSpecifier))

namespaceImport :: Assignment
namespaceImport = makeTerm <$> symbol Grammar.NamespaceImport <*> children (TypeScript.Syntax.NamespaceImport <$> identifier)

importRequireClause :: Assignment
importRequireClause = makeTerm <$> symbol Grammar.ImportRequireClause <*> children (TypeScript.Syntax.ImportRequireClause <$> identifier <*> string)

debuggerStatement :: Assignment
debuggerStatement = makeTerm <$> symbol Grammar.DebuggerStatement <*> (TypeScript.Syntax.Debugger <$ source)

expressionStatement' :: Assignment
expressionStatement' = symbol ExpressionStatement *> children (expression <|> sequenceExpression)

declaration :: Assignment
declaration = everything
  where
    everything = choice [
      exportStatement,
      importAlias',
      function,
      internalModule,
      ambientFunction,
      abstractClass,
      class',
      module',
      variableDeclaration,
      typeAliasDeclaration,
      enumDeclaration,
      interfaceDeclaration,
      ambientDeclaration
      ]

typeAliasDeclaration :: Assignment
typeAliasDeclaration = makeTypeAliasDecl <$> symbol Grammar.TypeAliasDeclaration <*> children ((,,) <$> identifier <*> (typeParameters <|> emptyTerm) <*> ty)
  where makeTypeAliasDecl loc (identifier, typeParams, body) = makeTerm loc (Declaration.TypeAliasDeclaration [typeParams] identifier body)

enumDeclaration :: Assignment
enumDeclaration = makeTerm <$> symbol Grammar.EnumDeclaration <*> children (TypeScript.Syntax.EnumDeclaration <$> identifier <*> many (term (propertyName <|> enumAssignment)))

enumAssignment :: Assignment
enumAssignment = makeTerm <$> symbol Grammar.EnumAssignment <*> children (Statement.Assignment [] <$> propertyName <*> expression)

interfaceDeclaration :: Assignment
interfaceDeclaration = makeInterfaceDecl <$> symbol Grammar.InterfaceDeclaration <*> children ((,,,) <$> identifier <*> (typeParameters <|> emptyTerm) <*> (extendsClause <|> emptyTerm) <*> objectType)
  where makeInterfaceDecl loc (identifier, typeParams, clause, objectType) = makeTerm loc (Declaration.InterfaceDeclaration [typeParams, clause] identifier objectType)

extendsClause :: Assignment
extendsClause = makeTerm <$> symbol Grammar.ExtendsClause <*> children (TypeScript.Syntax.ExtendsClause <$> many (term ty))

ambientDeclaration :: Assignment
ambientDeclaration = makeTerm <$> symbol Grammar.AmbientDeclaration <*> children (TypeScript.Syntax.AmbientDeclaration <$> choice [declaration, statementBlock])

exportStatement :: Assignment
exportStatement = makeTerm <$> symbol Grammar.ExportStatement <*> children (TypeScript.Syntax.Export <$> (((\a b -> [a, b]) <$> exportClause <*> fromClause) <|> ((++) <$> many (term decorator) <*> (pure <$> (fromClause <|> exportClause <|> declaration <|> expression <|> identifier <|> importAlias')))))

fromClause :: Assignment
fromClause = string

exportClause :: Assignment
exportClause = makeTerm <$> symbol Grammar.ExportClause <*> children (TypeScript.Syntax.ExportClause <$> many (term importExportSpecifier))

importExportSpecifier :: Assignment
importExportSpecifier = makeTerm <$> (symbol Grammar.ExportSpecifier <|> symbol Grammar.ImportSpecifier) <*> children (TypeScript.Syntax.ImportExportSpecifier <$> identifier <*> (identifier <|> emptyTerm))

propertySignature :: Assignment
propertySignature = makePropertySignature <$> symbol Grammar.PropertySignature <*> children ((,,,) <$> (accessibilityModifier' <|> emptyTerm) <*> (readonly' <|> emptyTerm) <*> propertyName <*> (typeAnnotation' <|> emptyTerm))
  where makePropertySignature loc (modifier, readonly, propertyName, annotation) = makeTerm loc (TypeScript.Syntax.PropertySignature [modifier, readonly, annotation] propertyName)

propertyName :: Assignment
propertyName = (makeTerm <$> symbol PropertyIdentifier <*> (Syntax.Identifier <$> source)) <|> string <|> number <|> computedPropertyName

computedPropertyName :: Assignment
computedPropertyName = makeTerm <$> symbol Grammar.ComputedPropertyName <*> children (TypeScript.Syntax.ComputedPropertyName <$> expression)

assignmentPattern :: Assignment
assignmentPattern = makeTerm <$> symbol AssignmentPattern <*> children (Statement.Assignment [] <$> shorthandPropertyIdentifier <*> expression)

shorthandPropertyIdentifier :: Assignment
shorthandPropertyIdentifier = makeTerm <$> symbol Grammar.ShorthandPropertyIdentifier <*> (TypeScript.Syntax.ShorthandPropertyIdentifier <$> source)

requiredParameter :: Assignment
requiredParameter = makeRequiredParameter <$> symbol Grammar.RequiredParameter <*> children ((,,,,) <$> (accessibilityModifier' <|> emptyTerm) <*> (readonly' <|> emptyTerm) <*> (identifier <|> destructuringPattern) <*> (typeAnnotation' <|> emptyTerm) <*> (expression <|> emptyTerm))
  where makeRequiredParameter loc (modifier, readonly, identifier, annotation, initializer) = makeTerm loc (TypeScript.Syntax.RequiredParameter [modifier, readonly, annotation] (makeTerm loc (Statement.Assignment [] identifier initializer)))

restParameter :: Assignment
restParameter = makeRestParameter <$> symbol Grammar.RestParameter <*> children ((,) <$> identifier <*> (typeAnnotation' <|> emptyTerm))
  where makeRestParameter loc (identifier, annotation) = makeTerm loc (TypeScript.Syntax.RestParameter [annotation] identifier)

optionalParameter :: Assignment
optionalParameter = makeOptionalParam <$> symbol Grammar.OptionalParameter <*> children ((,,,,) <$> (accessibilityModifier' <|> emptyTerm) <*> (readonly' <|> emptyTerm) <*> (identifier <|> destructuringPattern) <*> (typeAnnotation' <|> emptyTerm) <*> (expression <|> emptyTerm))
  where makeOptionalParam loc (modifier, readonly, subject, annotation, initializer) = makeTerm loc (TypeScript.Syntax.OptionalParameter [modifier, readonly, annotation] (makeTerm loc (Statement.Assignment [] subject initializer)))

internalModule :: Assignment
internalModule = makeTerm <$> symbol Grammar.InternalModule <*> children (TypeScript.Syntax.InternalModule <$> (string <|> identifier <|> nestedIdentifier) <*> statements)

module' :: Assignment
module' = makeTerm <$> symbol Module <*> children (Declaration.Module <$> (string <|> identifier <|> nestedIdentifier) <*> ((symbol StatementBlock *> children (many statement)) <|> pure []))


statements :: HasCallStack => Assignment.Assignment [] Grammar [Term]
statements = symbol StatementBlock *> children (many statement)

arrowFunction :: Assignment
arrowFunction = makeArrowFun <$> symbol ArrowFunction <*> children ((,,) <$> emptyTerm <*> (((\a b c -> (a, [b], c)) <$> emptyTerm <*> identifier <*> emptyTerm) <|> callSignatureParts) <*> (expression <|> statementBlock))
  where makeArrowFun loc (identifier, (typeParams, params, returnTy), body) = makeTerm loc (Declaration.Function [ typeParams, returnTy ] identifier params body)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

ifStatement :: Assignment
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> parenthesizedExpression <*> statement <*> (statement <|> emptyTerm))

whileStatement :: Assignment
whileStatement = makeTerm <$> symbol WhileStatement <*> children (Statement.While <$> expression <*> statement)

forStatement :: Assignment
forStatement = makeTerm <$> symbol ForStatement <*> children (Statement.For <$> (variableDeclaration <|> expressionStatement' <|> emptyStatement) <*> (expressionStatement' <|> emptyStatement) <*> (expression <|> emptyTerm) <*> statement)

variableDeclaration :: Assignment
variableDeclaration = makeTerm <$> (symbol Grammar.VariableDeclaration <|> symbol Grammar.LexicalDeclaration) <*> children (Declaration.VariableDeclaration <$> many (term variableDeclarator))

variableDeclarator :: Assignment
variableDeclarator = makeVarDecl <$> symbol VariableDeclarator <*> children ((,,) <$> (identifier <|> destructuringPattern) <*> (typeAnnotation' <|> emptyTerm) <*> (expression <|> emptyTerm))
  where makeVarDecl loc (subject, annotations, value) = makeTerm loc (Statement.Assignment [annotations] subject value)

parenthesizedExpression :: Assignment
parenthesizedExpression = symbol ParenthesizedExpression *> children (expression <|> sequenceExpression)

switchStatement :: Assignment
switchStatement = makeTerm <$> symbol SwitchStatement <*> children (Statement.Match <$> parenthesizedExpression <*> switchBody)
  where
    switchBody =  symbol SwitchBody *> children (makeTerm <$> location <*> many (term switchCase))
    switchCase = makeTerm <$> (symbol SwitchCase <|> symbol SwitchDefault) <*> children (Statement.Pattern <$> (expression <|> emptyTerm) <*> (makeTerm <$> location <*> many statement))

subscriptExpression :: Assignment
subscriptExpression = makeTerm <$> symbol SubscriptExpression <*> children (Expression.Subscript <$> expression <*> (pure <$> expression))

pair :: Assignment
pair = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> propertyName <*> expression)

callExpression :: Assignment
callExpression = makeCall <$> (symbol CallExpression <|> symbol CallExpression') <*> children ((,,,) <$> (expression <|> super <|> function) <*> (typeArguments <|> pure []) <*> (arguments <|> (pure <$> templateString)) <*> emptyTerm)
  where makeCall loc (subject, typeArgs, args, body) = makeTerm loc (Expression.Call typeArgs subject args body)
        arguments = symbol Arguments *> children (many (term (expression <|> spreadElement)))
        typeArguments = symbol Grammar.TypeArguments *> children (some (term ty))

tryStatement :: Assignment
tryStatement = makeTry <$> symbol TryStatement <*> children ((,,) <$> statementBlock <*> optional catchClause <*> optional finallyClause)
  where
    makeTry loc (statementBlock', catch, finally) = makeTerm loc (Statement.Try statementBlock' (catMaybes [catch, finally]))
    catchClause = makeTerm <$> symbol CatchClause <*> children (Statement.Catch <$> (identifier <|> emptyTerm) <*> statementBlock)
    finallyClause = makeTerm <$> symbol FinallyClause <*> children (Statement.Finally <$> statementBlock)

binaryExpression  :: Assignment
binaryExpression = makeTerm' <$> symbol BinaryExpression <*> children (infixTerm expression expression
  [ (inj .) . Expression.Plus             <$ symbol AnonPlus
  , (inj .) . Expression.Minus            <$ symbol AnonMinus
  , (inj .) . Expression.Times            <$ symbol AnonStar
  , (inj .) . Expression.DividedBy        <$ symbol AnonSlash
  , (inj .) . Expression.Modulo           <$ symbol AnonPercent
  , (inj .) . Expression.Member           <$ symbol AnonIn
  , (inj .) . Expression.And              <$ symbol AnonAmpersandAmpersand
  , (inj .) . Expression.BAnd             <$ symbol AnonAmpersand
  , (inj .) . Expression.Or               <$ symbol AnonPipePipe
  , (inj .) . Expression.BOr              <$ symbol AnonPipe
  , (inj .) . Expression.BXOr             <$ symbol AnonCaret
  , (inj .) . Expression.InstanceOf       <$ symbol AnonInstanceof
  , (inj .) . Expression.Equal            <$ (symbol AnonEqualEqual <|> symbol AnonEqualEqualEqual)
  , (inj .) . invert Expression.Equal     <$ (symbol AnonBangEqual <|> symbol AnonBangEqualEqual)
  , (inj .) . Expression.LShift           <$ symbol AnonLAngleLAngle
  , (inj .) . Expression.RShift           <$ symbol AnonRAngleRAngle
  , (inj .) . Expression.LessThan         <$ symbol AnonLAngle
  , (inj .) . Expression.GreaterThan      <$ symbol AnonRAngle
  , (inj .) . Expression.LessThanEqual    <$ symbol AnonLAngleEqual
  , (inj .) . Expression.GreaterThanEqual <$ symbol AnonRAngleEqual
  ])
  where invert cons a b = Expression.Not (makeTerm1 (cons a b))

emptyStatement :: Assignment
emptyStatement = makeTerm <$> symbol EmptyStatement <*> (Syntax.Empty <$ source <|> pure Syntax.Empty)

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: HasCallStack
          => Assignment
          -> Assignment
          -> [Assignment.Assignment [] Grammar (Term -> Term -> Data.Union.Union Syntax Term)]
          -> Assignment.Assignment [] Grammar (Data.Union.Union Syntax Term)
infixTerm = infixContext comment
