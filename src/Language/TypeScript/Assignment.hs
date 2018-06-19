{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.TypeScript.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Assigning.Assignment hiding (Assignment, Error)
import Data.Abstract.Name (Name, name)
import qualified Assigning.Assignment as Assignment
import Data.Record
import Data.Sum
import Data.Syntax
    ( contextualize
    , emptyTerm
    , handleError
    , infixContext
    , makeTerm
    , makeTerm'
    , makeTerm''
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
import qualified Data.Term as Term
import Language.TypeScript.Grammar as Grammar
import qualified Language.TypeScript.Syntax as TypeScript.Syntax
import Prologue

-- | The type of TypeScript syntax.
type Syntax = '[
    Comment.Comment
  , Comment.HashBang
  , Declaration.Class
  , Declaration.Function
  , Declaration.Method
  , Declaration.MethodSignature
  , Declaration.InterfaceDeclaration
  , Declaration.PublicFieldDefinition
  , Declaration.VariableDeclaration
  , Declaration.TypeAlias
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
  , Statement.DoWhile
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
  , Statement.Statements
  , Statement.Throw
  , Statement.Try
  , Statement.While
  , Statement.Yield
  , Syntax.AccessibilityModifier
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Identifier
  , Syntax.Context
  , Type.Readonly
  , Type.TypeParameters
  , TypeScript.Syntax.TypeParameter
  , TypeScript.Syntax.Constraint
  , TypeScript.Syntax.ParenthesizedType
  , TypeScript.Syntax.DefaultType
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
  , TypeScript.Syntax.LookupType
  , TypeScript.Syntax.FlowMaybeType
  , TypeScript.Syntax.TypeQuery
  , TypeScript.Syntax.IndexTypeQuery
  , TypeScript.Syntax.ThisType
  , TypeScript.Syntax.ExistentialType
  , TypeScript.Syntax.AbstractMethodSignature
  , TypeScript.Syntax.IndexSignature
  , TypeScript.Syntax.ObjectType
  , TypeScript.Syntax.LiteralType
  , TypeScript.Syntax.Union
  , TypeScript.Syntax.Intersection
  , TypeScript.Syntax.Module
  , TypeScript.Syntax.InternalModule
  , TypeScript.Syntax.FunctionType
  , TypeScript.Syntax.Tuple
  , TypeScript.Syntax.Constructor
  , TypeScript.Syntax.TypeAssertion
  , TypeScript.Syntax.ImportAlias
  , TypeScript.Syntax.Debugger
  , TypeScript.Syntax.ShorthandPropertyIdentifier
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
  , TypeScript.Syntax.JsxFragment
  , TypeScript.Syntax.JsxNamespaceName
  , TypeScript.Syntax.OptionalParameter
  , TypeScript.Syntax.RequiredParameter
  , TypeScript.Syntax.RestParameter
  , TypeScript.Syntax.PropertySignature
  , TypeScript.Syntax.AmbientDeclaration
  , TypeScript.Syntax.EnumDeclaration
  , TypeScript.Syntax.ExtendsClause
  , TypeScript.Syntax.AmbientFunction
  , TypeScript.Syntax.ImportRequireClause
  , TypeScript.Syntax.ImportClause
  , TypeScript.Syntax.LabeledStatement
  , TypeScript.Syntax.Annotation
  , TypeScript.Syntax.With
  , TypeScript.Syntax.ForOf
  , TypeScript.Syntax.This
  , TypeScript.Syntax.Update
  , TypeScript.Syntax.ComputedPropertyName
  , TypeScript.Syntax.Decorator
  , TypeScript.Syntax.Import
  , TypeScript.Syntax.QualifiedAliasedImport
  , TypeScript.Syntax.SideEffectImport
  , TypeScript.Syntax.DefaultExport
  , TypeScript.Syntax.QualifiedExport
  , TypeScript.Syntax.QualifiedExportFrom
  , TypeScript.Syntax.JavaScriptRequire
  , []
  ]

type Term = Term.Term (Sum Syntax) (Record Location)
type Assignment = Assignment.Assignment [] Grammar Term

-- | Assignment from AST in TypeScript’s grammar onto a program in TypeScript’s syntax.
assignment :: Assignment
assignment = handleError $ makeTerm <$> symbol Program <*> children (Statement.Statements <$> manyTerm statement) <|> parseError

expression :: Assignment
expression = handleError everything
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
      jsxElement',
      jsxFragment,
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
      this,
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
undefined' = makeTerm <$> symbol Grammar.Undefined <*> (TypeScript.Syntax.Undefined <$ rawSource)

assignmentExpression :: Assignment
assignmentExpression = makeTerm <$> symbol AssignmentExpression <*> children (Statement.Assignment [] <$> term (memberExpression <|> subscriptExpression <|> identifier <|> destructuringPattern) <*> expression)

augmentedAssignmentExpression :: Assignment
augmentedAssignmentExpression = makeTerm' <$> symbol AugmentedAssignmentExpression <*> children (infixTerm (memberExpression <|> subscriptExpression <|> identifier <|> destructuringPattern) (term expression) [
    assign Expression.Plus <$ symbol AnonPlusEqual
  , assign Expression.Minus <$ symbol AnonMinusEqual
  , assign Expression.Times <$ symbol AnonStarEqual
  , assign Expression.DividedBy <$ symbol AnonSlashEqual
  , assign Expression.Modulo <$ symbol AnonPercentEqual
  , assign Expression.BXOr <$ symbol AnonCaretEqual
  , assign Expression.BAnd <$ symbol AnonAmpersandEqual
  , assign Expression.RShift <$ symbol AnonRAngleRAngleEqual
  , assign Expression.LShift <$ symbol AnonLAngleLAngleEqual
  , assign Expression.UnsignedRShift <$ symbol AnonRAngleRAngleRAngleEqual
  , assign Expression.LShift <$ symbol AnonLAngleLAngleEqual
  , assign Expression.BOr <$ symbol AnonPipeEqual ])
  where assign :: (f :< Syntax) => (Term -> Term -> f Term) -> Term -> Term -> Sum Syntax Term
        assign c l r = inject (Statement.Assignment [] l (makeTerm1 (c l r)))


awaitExpression :: Assignment
awaitExpression = makeTerm <$> symbol Grammar.AwaitExpression <*> children (Expression.Await <$> term expression)

unaryExpression :: Assignment
unaryExpression = symbol Grammar.UnaryExpression >>= \ loc ->
  makeTerm loc . Expression.Not <$> children ((symbol AnonTilde <|> symbol AnonBang) *> term expression)
  <|> makeTerm loc . Expression.Negate <$> children ((symbol AnonMinus <|> symbol AnonPlus) *> term expression)
  <|> makeTerm loc . Expression.Typeof <$> children (symbol AnonTypeof *> term expression)
  <|> makeTerm loc . Expression.Void <$> children (symbol AnonVoid *> term expression)
  <|> makeTerm loc . Expression.Delete <$> children (symbol AnonDelete *> term expression)

ternaryExpression :: Assignment
ternaryExpression = makeTerm <$> symbol Grammar.TernaryExpression <*> children (Statement.If <$> term expression <*> term expression <*> term expression)

memberExpression :: Assignment
memberExpression = makeTerm <$> (symbol Grammar.MemberExpression <|> symbol Grammar.MemberExpression') <*> children (Expression.MemberAccess <$> term expression <*> propertyIdentifier')

newExpression :: Assignment
newExpression = makeTerm <$> symbol Grammar.NewExpression <*> children (Expression.New . pure <$> term expression)

updateExpression :: Assignment
updateExpression = makeTerm <$> symbol Grammar.UpdateExpression <*> children (TypeScript.Syntax.Update <$> term expression)

yieldExpression :: Assignment
yieldExpression = makeTerm <$> symbol Grammar.YieldExpression <*> children (Statement.Yield <$> term (expression <|> emptyTerm))

this :: Assignment
this = makeTerm <$> symbol Grammar.This <*> (TypeScript.Syntax.This <$ rawSource)

regex :: Assignment
regex = makeTerm <$> symbol Grammar.Regex <*> (Literal.Regex <$> source)

null' :: Assignment
null' = makeTerm <$> symbol Null <*> (Literal.Null <$ rawSource)

anonymousClass :: Assignment
anonymousClass = makeTerm <$> symbol Grammar.AnonymousClass <*> children (Declaration.Class [] <$> emptyTerm <*> (classHeritage' <|> pure []) <*> classBodyStatements)

abstractClass :: Assignment
abstractClass = makeTerm <$> symbol Grammar.AbstractClass <*> children (TypeScript.Syntax.AbstractClass <$> term identifier <*> (term typeParameters <|> emptyTerm) <*> (classHeritage' <|> pure []) <*> classBodyStatements)

abstractMethodSignature :: Assignment
abstractMethodSignature = makeSignature <$> symbol Grammar.AbstractMethodSignature <*> children ((,,) <$> (term accessibilityModifier' <|> emptyTerm) <*> term propertyName <*> callSignatureParts)
  where makeSignature loc (modifier, propertyName, (typeParams, params, annotation)) = makeTerm loc (TypeScript.Syntax.AbstractMethodSignature [modifier, typeParams, annotation] propertyName params)

classHeritage' :: Assignment.Assignment [] Grammar [Term]
classHeritage' = symbol Grammar.ClassHeritage *> children ((mappend `on` toList) <$> optional (term extendsClause) <*> optional (term implementsClause'))

extendsClause :: Assignment
extendsClause = makeTerm <$> symbol Grammar.ExtendsClause <*> children (TypeScript.Syntax.ExtendsClause <$> manyTerm (typeReference <|> expression))

typeReference :: Assignment
typeReference = typeIdentifier <|> nestedTypeIdentifier <|> genericType

implementsClause' :: Assignment
implementsClause' = makeTerm <$> symbol Grammar.ImplementsClause <*> children (TypeScript.Syntax.ImplementsClause <$> manyTerm ty)

super :: Assignment
super = makeTerm <$> symbol Grammar.Super <*> (TypeScript.Syntax.Super <$ rawSource)

typeAssertion :: Assignment
typeAssertion = makeTerm <$> symbol Grammar.TypeAssertion <*> children (TypeScript.Syntax.TypeAssertion <$> term typeArguments' <*> term expression)

asExpression :: Assignment
asExpression = makeTerm <$> symbol AsExpression <*> children (Expression.Cast <$> term expression <*> term (ty <|> templateString))

templateString :: Assignment
templateString = makeTerm <$> symbol TemplateString <*> children (Literal.String <$> manyTerm templateSubstitution)

templateSubstitution :: Assignment
templateSubstitution = symbol TemplateSubstitution *> children (term expressions)

nonNullExpression' :: Assignment
nonNullExpression' = makeTerm <$> symbol Grammar.NonNullExpression <*> children (Expression.NonNullExpression <$> term expression)

importAlias' :: Assignment
importAlias' = makeTerm <$> symbol Grammar.ImportAlias <*> children (TypeScript.Syntax.ImportAlias <$> term identifier <*> term (identifier <|> nestedIdentifier))

number :: Assignment
number = makeTerm <$> symbol Grammar.Number <*> (Literal.Float <$> source)

string :: Assignment
string = makeTerm <$> symbol Grammar.String <*> (Literal.TextElement <$> source)

true :: Assignment
true = makeTerm <$> symbol Grammar.True <*> (Literal.true <$ rawSource)

false :: Assignment
false = makeTerm <$> symbol Grammar.False <*> (Literal.false <$ rawSource)

identifier :: Assignment
identifier = makeTerm <$> (symbol Identifier <|> symbol Identifier') <*> (Syntax.Identifier . name <$> source)

class' :: Assignment
class' = makeClass <$> symbol Class <*> children ((,,,,) <$> manyTerm decorator <*> term identifier <*> (symbol TypeParameters *> children (manyTerm typeParameter') <|> pure []) <*> (classHeritage' <|> pure []) <*> classBodyStatements)
  where makeClass loc (decorators, expression, typeParams, classHeritage, statements) = makeTerm loc (Declaration.Class (decorators <> typeParams) expression classHeritage statements)

object :: Assignment
object = makeTerm <$> (symbol Object <|> symbol ObjectPattern) <*> children (Literal.Hash <$> manyTerm (pair <|> spreadElement <|> methodDefinition <|> assignmentPattern <|> shorthandPropertyIdentifier))

array :: Assignment
array = makeTerm <$> (symbol Array <|> symbol ArrayPattern) <*> children (Literal.Array <$> manyTerm (expression <|> spreadElement))

jsxElement' :: Assignment
jsxElement' = choice [ jsxElement, jsxSelfClosingElement ]

jsxElement :: Assignment
jsxElement = makeTerm <$> symbol Grammar.JsxElement <*> children (TypeScript.Syntax.JsxElement <$> term jsxOpeningElement' <*> manyTerm jsxChild <*> term jsxClosingElement')

jsxFragment :: Assignment
jsxFragment = makeTerm <$> symbol Grammar.JsxFragment <*> children (TypeScript.Syntax.JsxFragment <$> manyTerm jsxChild)

jsxChild :: Assignment
jsxChild = choice [ jsxElement', jsxExpression', jsxText ]

jsxSelfClosingElement :: Assignment
jsxSelfClosingElement = makeTerm <$> symbol Grammar.JsxSelfClosingElement <*> children (TypeScript.Syntax.JsxSelfClosingElement <$> term jsxElementName <*> manyTerm jsxAttribute')

jsxAttribute' :: Assignment
jsxAttribute' = jsxAttribute <|> jsxExpression'

jsxOpeningElement' :: Assignment
jsxOpeningElement' = makeTerm <$> symbol Grammar.JsxOpeningElement <*> children (TypeScript.Syntax.JsxOpeningElement <$> term jsxElementName <*> manyTerm jsxAttribute')

jsxElementName :: Assignment
jsxElementName = choice [ identifier, nestedIdentifier, jsxNamespaceName ]

jsxNamespaceName :: Assignment
jsxNamespaceName = makeTerm <$> symbol Grammar.JsxNamespaceName <*> children (TypeScript.Syntax.JsxNamespaceName <$> identifier <*> identifier)

jsxExpression' :: Assignment
jsxExpression' = makeTerm <$> symbol Grammar.JsxExpression <*> children (TypeScript.Syntax.JsxExpression <$> term (expressions <|> spreadElement <|> emptyTerm))

jsxText :: Assignment
jsxText = makeTerm <$> symbol Grammar.JsxText <*> (TypeScript.Syntax.JsxText <$> source)

jsxClosingElement' :: Assignment
jsxClosingElement' = makeTerm <$> symbol Grammar.JsxClosingElement <*> children (TypeScript.Syntax.JsxClosingElement <$> term jsxElementName)

jsxAttribute :: Assignment
jsxAttribute = makeTerm <$> symbol Grammar.JsxAttribute <*> children (TypeScript.Syntax.JsxAttribute <$> term (propertyIdentifier <|> jsxNamespaceName) <*> (term jsxAttributeValue <|> emptyTerm))
  where jsxAttributeValue = choice [ string, jsxExpression', jsxElement', jsxFragment ]

propertyIdentifier :: Assignment
propertyIdentifier = makeTerm <$> symbol PropertyIdentifier <*> (Syntax.Identifier . name <$> source)

propertyIdentifier' :: Assignment.Assignment [] Grammar Name
propertyIdentifier' = symbol PropertyIdentifier *> (name <$> source)

sequenceExpression :: Assignment
sequenceExpression = makeTerm <$> symbol Grammar.SequenceExpression <*> children (Expression.SequenceExpression <$> term expression <*> term expressions)

expressions :: Assignment
expressions = expression <|> sequenceExpression

parameter :: Assignment
parameter =
      requiredParameter
  <|> restParameter
  <|> optionalParameter

accessibilityModifier' :: Assignment
accessibilityModifier' = makeTerm <$> symbol AccessibilityModifier <*> children (Syntax.Identifier . name <$> source)

destructuringPattern :: Assignment
destructuringPattern = object <|> array

spreadElement :: Assignment
spreadElement = symbol SpreadElement *> children (term expression)

readonly' :: Assignment
readonly' = makeTerm <$> symbol Readonly <*> (Type.Readonly <$ rawSource)

methodDefinition :: Assignment
methodDefinition = makeMethod <$>
  symbol MethodDefinition
  <*> children ((,,,,,) <$> (term accessibilityModifier' <|> emptyTerm) <*> (term readonly' <|> emptyTerm) <*> emptyTerm <*> term propertyName <*> callSignatureParts <*> term statementBlock)
  where
    makeMethod loc (modifier, readonly, receiver, propertyName', (typeParameters', params, ty'), statements) = makeTerm loc (Declaration.Method [modifier, readonly, typeParameters', ty'] receiver propertyName' params statements)

callSignatureParts :: Assignment.Assignment [] Grammar (Term, [Term], Term)
callSignatureParts = contextualize' <$> Assignment.manyThrough comment (postContextualize'
 <$> (symbol Grammar.CallSignature *> children ((,,) <$> (fromMaybe <$> emptyTerm <*> optional (term typeParameters)) <*> formalParameters <*> (fromMaybe <$> emptyTerm <*> optional (term typeAnnotation')))) <*> many comment)
  where
    contextualize' (cs, (typeParams, formalParams, annotation)) = case nonEmpty cs of
      Just cs -> (makeTerm1 (Syntax.Context cs typeParams), formalParams, annotation)
      Nothing -> (typeParams, formalParams, annotation)
    postContextualize' (typeParams, formalParams, annotation) cs = case nonEmpty cs of
      Just cs -> (typeParams, formalParams, makeTerm1 (Syntax.Context cs annotation))
      Nothing -> (typeParams, formalParams, annotation)

callSignature :: Assignment
callSignature =  makeTerm <$> symbol Grammar.CallSignature <*> children (TypeScript.Syntax.CallSignature <$> (fromMaybe <$> emptyTerm <*> optional (term typeParameters)) <*> formalParameters <*> (fromMaybe <$> emptyTerm <*> optional (term typeAnnotation')))

constructSignature :: Assignment
constructSignature = makeTerm <$> symbol Grammar.ConstructSignature <*> children (TypeScript.Syntax.ConstructSignature <$> (fromMaybe <$> emptyTerm <*> optional (term typeParameters)) <*> formalParameters <*> (fromMaybe <$> emptyTerm <*> optional (term typeAnnotation')))

indexSignature :: Assignment
indexSignature = makeTerm <$> symbol Grammar.IndexSignature <*> children (TypeScript.Syntax.IndexSignature <$> term identifier <*> term typeAnnotation')

methodSignature :: Assignment
methodSignature = makeMethodSignature <$> symbol Grammar.MethodSignature <*> children ((,,,) <$> (term accessibilityModifier' <|> emptyTerm) <*> (term readonly' <|> emptyTerm) <*> term propertyName <*> callSignatureParts)
  where makeMethodSignature loc (modifier, readonly, propertyName, (typeParams, params, annotation)) = makeTerm loc (Declaration.MethodSignature [modifier, readonly, typeParams, annotation] propertyName params)

formalParameters :: Assignment.Assignment [] Grammar [Term]
formalParameters = symbol FormalParameters *> children (contextualize' <$> Assignment.manyThrough comment (postContextualize' <$> (concat <$> many ((\as b -> as <> [b]) <$> manyTerm decorator <*> term parameter)) <*> many comment))
  where
    contextualize' (cs, formalParams) = case nonEmpty cs of
      Just cs -> toList cs <> formalParams
      Nothing -> formalParams
    postContextualize' formalParams cs = case nonEmpty cs of
      Just cs -> formalParams <> toList cs
      Nothing -> formalParams


decorator :: Assignment
decorator = makeTerm <$> symbol Grammar.Decorator <*> children (TypeScript.Syntax.Decorator <$> term (identifier <|> memberExpression <|> callExpression))

typeParameters :: Assignment
typeParameters = makeTerm <$> symbol TypeParameters <*> children (Type.TypeParameters <$> manyTerm typeParameter')

typeAnnotation' :: Assignment
typeAnnotation' = makeTerm <$> symbol TypeAnnotation <*> children (TypeScript.Syntax.Annotation <$> term ty)

typeParameter' :: Assignment
typeParameter' = makeTerm <$> symbol Grammar.TypeParameter <*> children (TypeScript.Syntax.TypeParameter <$> term identifier <*> term (constraint <|> emptyTerm) <*> term (defaultType <|> emptyTerm))

defaultType :: Assignment
defaultType = makeTerm <$> symbol Grammar.DefaultType <*> children (TypeScript.Syntax.DefaultType <$> term ty)

constraint :: Assignment
constraint = makeTerm <$> symbol Grammar.Constraint <*> children (TypeScript.Syntax.Constraint <$> term ty)

function :: Assignment
function = makeFunction <$> (symbol Grammar.Function <|> symbol Grammar.GeneratorFunction) <*> children ((,,) <$> term (identifier <|> emptyTerm) <*> callSignatureParts <*> term statementBlock)
  where makeFunction loc (id, (typeParams, params, annotation), statements) = makeTerm loc (Declaration.Function [typeParams, annotation] id params statements)

-- TODO: FunctionSignatures can, but don't have to be ambient functions.
ambientFunction :: Assignment
ambientFunction = makeAmbientFunction <$> symbol Grammar.FunctionSignature <*> children ((,) <$> term identifier <*> callSignatureParts)
  where makeAmbientFunction loc (id, (typeParams, params, annotation)) = makeTerm loc (TypeScript.Syntax.AmbientFunction [typeParams, annotation] id params)

ty :: Assignment
ty = primaryType <|> unionType <|> intersectionType <|> functionTy <|> constructorTy

primaryType :: Assignment
primaryType = parenthesizedTy <|> predefinedTy <|> typeIdentifier <|> nestedTypeIdentifier <|> genericType <|> typePredicate <|> objectType <|> arrayTy <|> tupleType <|> flowMaybeTy <|> typeQuery <|> indexTypeQuery <|> thisType <|> existentialType <|> literalType <|> lookupType

parenthesizedTy :: Assignment
parenthesizedTy = makeTerm <$> symbol Grammar.ParenthesizedType <*> children (TypeScript.Syntax.ParenthesizedType <$> term ty)

predefinedTy :: Assignment
predefinedTy = makeTerm <$> symbol Grammar.PredefinedType <*> (TypeScript.Syntax.PredefinedType <$> source)

typeIdentifier :: Assignment
typeIdentifier = makeTerm <$> symbol Grammar.TypeIdentifier <*> (TypeScript.Syntax.TypeIdentifier <$> source)

nestedIdentifier :: Assignment
nestedIdentifier = makeTerm <$> symbol Grammar.NestedIdentifier <*> children (TypeScript.Syntax.NestedIdentifier <$> term (identifier <|> nestedIdentifier) <*> term identifier)

nestedTypeIdentifier :: Assignment
nestedTypeIdentifier = makeTerm <$> symbol Grammar.NestedTypeIdentifier <*> children (TypeScript.Syntax.NestedTypeIdentifier <$> term (identifier <|> nestedIdentifier) <*> term typeIdentifier)

genericType :: Assignment
genericType = makeTerm <$> symbol Grammar.GenericType <*> children (TypeScript.Syntax.GenericType <$> term (typeIdentifier <|> nestedTypeIdentifier) <*> term typeArguments')

typeArguments' :: Assignment
typeArguments' = makeTerm <$> symbol Grammar.TypeArguments <*> children (TypeScript.Syntax.TypeArguments <$> some (term ty))

typePredicate :: Assignment
typePredicate = makeTerm <$> symbol Grammar.TypePredicate <*> children (TypeScript.Syntax.TypePredicate <$> term identifier <*> term ty)

objectType :: Assignment
objectType = makeTerm <$> symbol Grammar.ObjectType <*> children (TypeScript.Syntax.ObjectType <$> manyTerm (exportStatement <|> propertySignature <|> callSignature <|> constructSignature <|> indexSignature <|> methodSignature))

arrayTy :: Assignment
arrayTy = makeTerm <$> symbol Grammar.ArrayType <*> children (TypeScript.Syntax.ArrayType <$> term ty)

lookupType :: Assignment
lookupType = makeTerm <$> symbol Grammar.LookupType <*> children (TypeScript.Syntax.LookupType <$> term (identifier <|> nestedTypeIdentifier) <*> term ty)

flowMaybeTy :: Assignment
flowMaybeTy = makeTerm <$> symbol Grammar.FlowMaybeType <*> children (TypeScript.Syntax.FlowMaybeType <$> term primaryType)

typeQuery :: Assignment
typeQuery = makeTerm <$> symbol Grammar.TypeQuery <*> children (TypeScript.Syntax.TypeQuery <$> term (identifier <|> nestedIdentifier))

indexTypeQuery :: Assignment
indexTypeQuery = makeTerm <$> symbol Grammar.IndexTypeQuery <*> children (TypeScript.Syntax.IndexTypeQuery <$> term (identifier <|> nestedIdentifier))

thisType :: Assignment
thisType = makeTerm <$> symbol Grammar.ThisType <*> (TypeScript.Syntax.ThisType <$> source)

existentialType :: Assignment
existentialType = makeTerm <$> symbol Grammar.ExistentialType <*> (TypeScript.Syntax.ExistentialType <$> source)

literalType :: Assignment
literalType = makeTerm <$> symbol Grammar.LiteralType <*> children (TypeScript.Syntax.LiteralType <$> term (number <|> string <|> true <|> false))

unionType :: Assignment
unionType = makeTerm <$> symbol UnionType <*> children (TypeScript.Syntax.Union <$> (term ty <|> emptyTerm) <*> term ty)

intersectionType :: Assignment
intersectionType = makeTerm <$> symbol IntersectionType <*> children (TypeScript.Syntax.Intersection <$> term ty <*> term ty)

functionTy :: Assignment
functionTy = makeTerm <$> symbol Grammar.FunctionType <*> children (TypeScript.Syntax.FunctionType <$> (fromMaybe <$> emptyTerm <*> optional (term typeParameters)) <*> formalParameters <*> term ty)

tupleType :: Assignment
tupleType = makeTerm <$> symbol TupleType <*> children (TypeScript.Syntax.Tuple <$> manyTerm ty)

constructorTy :: Assignment
constructorTy = makeTerm <$> symbol ConstructorType <*> children (TypeScript.Syntax.Constructor <$> (fromMaybe <$> emptyTerm <*> optional (term typeParameters)) <*> formalParameters <*> term ty)

statementBlock :: Assignment
statementBlock = makeTerm <$> symbol StatementBlock <*> children (manyTerm statement)

classBodyStatements :: Assignment
classBodyStatements = makeTerm'' <$> symbol ClassBody <*> children (contextualize' <$> Assignment.manyThrough comment (postContextualize' <$> (concat <$> many ((\as b -> as <> [b]) <$> manyTerm decorator <*> term (methodDefinition <|> publicFieldDefinition <|> methodSignature <|> indexSignature <|> abstractMethodSignature))) <*> many comment))
  where
    contextualize' (cs, formalParams) = case nonEmpty cs of
      Just cs -> toList cs <> formalParams
      Nothing -> formalParams
    postContextualize' formalParams cs = case nonEmpty cs of
      Just cs -> formalParams <> toList cs
      Nothing -> formalParams

publicFieldDefinition :: Assignment
publicFieldDefinition = makeField <$> symbol Grammar.PublicFieldDefinition <*> children ((,,,,) <$> (term accessibilityModifier' <|> emptyTerm) <*> (term readonly' <|> emptyTerm) <*> term propertyName <*> (term typeAnnotation' <|> emptyTerm) <*> (term expression <|> emptyTerm))
  where makeField loc (modifier, readonly, propertyName, annotation, expression) = makeTerm loc (Declaration.PublicFieldDefinition [modifier, readonly, annotation] propertyName expression)


statement :: Assignment
statement = handleError everything
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
      , hashBang
      , emptyStatement
      , labeledStatement ]

forOfStatement :: Assignment
forOfStatement = makeTerm <$> symbol ForOfStatement <*> children (TypeScript.Syntax.ForOf <$> term expression <*> term expressions <*> term statement)

forInStatement :: Assignment
forInStatement = makeTerm <$> symbol ForInStatement <*> children (Statement.ForEach <$> term expression <*> term expression <*> term statement)

doStatement :: Assignment
doStatement = makeTerm <$> symbol DoStatement <*> children (flip Statement.DoWhile <$> term statement <*> term parenthesizedExpression)

continueStatement :: Assignment
continueStatement = makeTerm <$> symbol ContinueStatement <*> children (Statement.Continue <$> (statementIdentifier <|> term emptyTerm))

breakStatement :: Assignment
breakStatement = makeTerm <$> symbol BreakStatement <*> children (Statement.Break <$> (statementIdentifier <|> term emptyTerm))

withStatement :: Assignment
withStatement = makeTerm <$> symbol WithStatement <*> children (TypeScript.Syntax.With <$> term parenthesizedExpression <*> term statement)

returnStatement :: Assignment
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> (term expressions <|> term emptyTerm))

throwStatement :: Assignment
throwStatement = makeTerm <$> symbol Grammar.ThrowStatement <*> children (Statement.Throw <$> term expressions)

hashBang :: Assignment
hashBang = makeTerm <$> symbol HashBangLine <*> (Comment.HashBang <$> source)

labeledStatement :: Assignment
labeledStatement = makeTerm <$> symbol Grammar.LabeledStatement <*> children (TypeScript.Syntax.LabeledStatement <$> statementIdentifier <*> term statement)

statementIdentifier :: Assignment
statementIdentifier = makeTerm <$> symbol StatementIdentifier <*> (Syntax.Identifier . name <$> source)

importStatement :: Assignment
importStatement =   makeImportTerm <$> symbol Grammar.ImportStatement <*> children ((,) <$> importClause <*> fromClause)
                <|> makeTerm' <$> symbol Grammar.ImportStatement <*> children (requireImport <|> sideEffectImport)
  where
    -- `import foo = require "./foo"`
    requireImport = inject <$> (symbol Grammar.ImportRequireClause *> children (TypeScript.Syntax.QualifiedAliasedImport <$> term identifier <*> fromClause))
    -- `import "./foo"`
    sideEffectImport = inject <$> (TypeScript.Syntax.SideEffectImport <$> fromClause)
    -- `import { bar } from "./foo"`
    namedImport = (,) Nothing <$> (symbol Grammar.NamedImports *> children (many importSymbol))
    -- `import defaultMember from "./foo"`
    defaultImport =  (,) Nothing <$> (pure <$> (makeNameAliasPair <$> rawIdentifier <*> pure Nothing))
    -- `import * as name from "./foo"`
    namespaceImport = symbol Grammar.NamespaceImport *> children ((,) . Just <$> term identifier <*> pure [])

    -- Combinations of the above.
    importClause = symbol Grammar.ImportClause *>
      children (
            (pure <$> namedImport)
        <|> (pure <$> namespaceImport)
        <|> ((\a b -> [a, b]) <$> defaultImport <*> (namedImport <|> namespaceImport))
        <|> (pure <$> defaultImport))

    makeImportTerm1 loc from (Just alias, _) = makeTerm loc (TypeScript.Syntax.QualifiedAliasedImport alias from)
    makeImportTerm1 loc from (Nothing, symbols) = makeTerm loc (TypeScript.Syntax.Import symbols from)
    makeImportTerm loc ([x], from) = makeImportTerm1 loc from x
    makeImportTerm loc (xs, from) = makeTerm loc $ fmap (makeImportTerm1 loc from) xs
    importSymbol = symbol Grammar.ImportSpecifier *> children (makeNameAliasPair <$> rawIdentifier <*> ((Just <$> rawIdentifier) <|> pure Nothing))
    rawIdentifier = (symbol Identifier <|> symbol Identifier') *> (name <$> source)
    makeNameAliasPair from (Just alias) = (from, alias)
    makeNameAliasPair from Nothing = (from, from)

    -- TODO: Need to validate that inline comments are still handled with this change in assigning to Path and not a Term.
    fromClause = symbol Grammar.String *> (TypeScript.Syntax.importPath <$> source)

debuggerStatement :: Assignment
debuggerStatement = makeTerm <$> symbol Grammar.DebuggerStatement <*> (TypeScript.Syntax.Debugger <$ rawSource)

expressionStatement' :: Assignment
expressionStatement' = symbol ExpressionStatement *> children (term expressions)

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
typeAliasDeclaration = makeTypeAliasDecl <$> symbol Grammar.TypeAliasDeclaration <*> children ((,,) <$> term identifier <*> (term typeParameters <|> emptyTerm) <*> term ty)
  where makeTypeAliasDecl loc (identifier, typeParams, body) = makeTerm loc (Declaration.TypeAlias [typeParams] identifier body)

enumDeclaration :: Assignment
enumDeclaration = makeTerm <$> symbol Grammar.EnumDeclaration <*> children (TypeScript.Syntax.EnumDeclaration <$> term identifier <*> (symbol EnumBody *> children (manyTerm (propertyName <|> enumAssignment))))

enumAssignment :: Assignment
enumAssignment = makeTerm <$> symbol Grammar.EnumAssignment <*> children (Statement.Assignment [] <$> term propertyName <*> term expression)

interfaceDeclaration :: Assignment
interfaceDeclaration = makeInterfaceDecl <$> symbol Grammar.InterfaceDeclaration <*> children ((,,,) <$> term identifier <*> (term typeParameters <|> emptyTerm) <*> (term extendsClause <|> emptyTerm) <*> term objectType)
  where makeInterfaceDecl loc (identifier, typeParams, clause, objectType) = makeTerm loc (Declaration.InterfaceDeclaration [typeParams, clause] identifier objectType)

ambientDeclaration :: Assignment
ambientDeclaration = makeTerm <$> symbol Grammar.AmbientDeclaration <*> children (TypeScript.Syntax.AmbientDeclaration <$> term (choice [declaration, statementBlock]))

exportStatement :: Assignment
exportStatement = makeTerm <$> symbol Grammar.ExportStatement <*> children (flip TypeScript.Syntax.QualifiedExportFrom <$> exportClause <*> fromClause)
  <|> makeTerm <$> symbol Grammar.ExportStatement <*> children (TypeScript.Syntax.QualifiedExport <$> exportClause)
  <|> makeTerm <$> symbol Grammar.ExportStatement <*> children (TypeScript.Syntax.DefaultExport <$> contextualize decorator (term (declaration <|> expression <|> identifier <|> importAlias')))
  where
    exportClause = symbol Grammar.ExportClause *> children (many exportSymbol)
    exportSymbol = symbol Grammar.ExportSpecifier *> children (makeNameAliasPair <$> rawIdentifier <*> (Just <$> rawIdentifier))
                 <|> symbol Grammar.ExportSpecifier *> children (makeNameAliasPair <$> rawIdentifier <*> pure Nothing)
    makeNameAliasPair from (Just alias) = (from, alias)
    makeNameAliasPair from Nothing = (from, from)
    rawIdentifier = (symbol Identifier <|> symbol Identifier') *> (name <$> source)
    -- TODO: Need to validate that inline comments are still handled with this change in assigning to Path and not a Term.
    fromClause = symbol Grammar.String *> (TypeScript.Syntax.importPath <$> source)

propertySignature :: Assignment
propertySignature = makePropertySignature <$> symbol Grammar.PropertySignature <*> children ((,,,) <$> (term accessibilityModifier' <|> emptyTerm) <*> (term readonly' <|> emptyTerm) <*> term propertyName <*> (term typeAnnotation' <|> emptyTerm))
  where makePropertySignature loc (modifier, readonly, propertyName, annotation) = makeTerm loc (TypeScript.Syntax.PropertySignature [modifier, readonly, annotation] propertyName)

propertyName :: Assignment
propertyName = (makeTerm <$> symbol PropertyIdentifier <*> (Syntax.Identifier . name <$> source)) <|> term string <|> term number <|> term computedPropertyName

computedPropertyName :: Assignment
computedPropertyName = makeTerm <$> symbol Grammar.ComputedPropertyName <*> children (TypeScript.Syntax.ComputedPropertyName <$> term expression)

assignmentPattern :: Assignment
assignmentPattern = makeTerm <$> symbol AssignmentPattern <*> children (Statement.Assignment [] <$> term shorthandPropertyIdentifier <*> term expression)

shorthandPropertyIdentifier :: Assignment
shorthandPropertyIdentifier = makeTerm <$> symbol Grammar.ShorthandPropertyIdentifier <*> (TypeScript.Syntax.ShorthandPropertyIdentifier <$> source)

requiredParameter :: Assignment
requiredParameter = makeRequiredParameter <$> symbol Grammar.RequiredParameter <*> children ((,,,,) <$> (term accessibilityModifier' <|> emptyTerm) <*> (term readonly' <|> emptyTerm) <*> term (identifier <|> destructuringPattern <|> this) <*> (term typeAnnotation' <|> emptyTerm) <*> (term expression <|> emptyTerm))
  where makeRequiredParameter loc (modifier, readonly, identifier, annotation, initializer) = makeTerm loc (TypeScript.Syntax.RequiredParameter [modifier, readonly, annotation] (makeTerm loc (Statement.Assignment [] identifier initializer)))

restParameter :: Assignment
restParameter = makeRestParameter <$> symbol Grammar.RestParameter <*> children ((,) <$> term identifier <*> (term typeAnnotation' <|> emptyTerm))
  where makeRestParameter loc (identifier, annotation) = makeTerm loc (TypeScript.Syntax.RestParameter [annotation] identifier)

optionalParameter :: Assignment
optionalParameter = makeOptionalParam <$> symbol Grammar.OptionalParameter <*> children ((,,,,) <$> (term accessibilityModifier' <|> emptyTerm) <*> (term readonly' <|> emptyTerm) <*> (term identifier <|> destructuringPattern) <*> (term typeAnnotation' <|> emptyTerm) <*> (term expression <|> emptyTerm))
  where makeOptionalParam loc (modifier, readonly, subject, annotation, initializer) = makeTerm loc (TypeScript.Syntax.OptionalParameter [modifier, readonly, annotation] (makeTerm loc (Statement.Assignment [] subject initializer)))

internalModule :: Assignment
internalModule = makeTerm <$> symbol Grammar.InternalModule <*> children (TypeScript.Syntax.InternalModule <$> term (string <|> identifier <|> nestedIdentifier) <*> statements)

module' :: Assignment
module' = makeTerm <$> symbol Module <*> children (TypeScript.Syntax.Module <$> term (string <|> identifier <|> nestedIdentifier) <*> (statements <|> pure []))


statements :: Assignment.Assignment [] Grammar [Term]
statements = symbol StatementBlock *> children (manyTerm statement)

arrowFunction :: Assignment
arrowFunction = makeArrowFun <$> symbol ArrowFunction <*> children ((,,) <$> emptyTerm <*> (((\a b c -> (a, [b], c)) <$> emptyTerm <*> term identifier <*> emptyTerm) <|> callSignatureParts) <*> term (expression <|> statementBlock))
  where makeArrowFun loc (identifier, (typeParams, params, returnTy), body) = makeTerm loc (Declaration.Function [ typeParams, returnTy ] identifier params body)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

ifStatement :: Assignment
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> term parenthesizedExpression <*> term statement <*> (term statement <|> emptyTerm))

whileStatement :: Assignment
whileStatement = makeTerm <$> symbol WhileStatement <*> children (Statement.While <$> term expression <*> term statement)

forStatement :: Assignment
forStatement = makeTerm <$> symbol ForStatement <*> children (Statement.For <$> term (variableDeclaration <|> expressionStatement' <|> emptyStatement) <*> term (expressionStatement' <|> emptyStatement) <*> term (expressions <|> emptyTerm) <*> term statement)

variableDeclaration :: Assignment
variableDeclaration = makeTerm <$> (symbol Grammar.VariableDeclaration <|> symbol Grammar.LexicalDeclaration) <*> children (Declaration.VariableDeclaration <$> manyTerm variableDeclarator)

variableDeclarator :: Assignment
variableDeclarator =
      makeTerm <$> symbol VariableDeclarator <*> children (TypeScript.Syntax.JavaScriptRequire <$> identifier <*> requireCall)
  <|> makeVarDecl <$> symbol VariableDeclarator <*> children ((,,) <$> term (identifier <|> destructuringPattern) <*> (term typeAnnotation' <|> emptyTerm) <*> (term expression <|> emptyTerm))
  where
    makeVarDecl loc (subject, annotations, value) = makeTerm loc (Statement.Assignment [annotations] subject value)

    requireCall = symbol CallExpression *> children ((symbol Identifier <|> symbol Identifier') *> do
      s <- source
      guard (s == "require")
      symbol Arguments *> children (symbol Grammar.String *> (TypeScript.Syntax.importPath <$> source))
      )


parenthesizedExpression :: Assignment
parenthesizedExpression = symbol ParenthesizedExpression *> children (term expressions)

switchStatement :: Assignment
switchStatement = makeTerm <$> symbol SwitchStatement <*> children (Statement.Match <$> term parenthesizedExpression <*> term switchBody)
  where
    switchBody =  symbol SwitchBody *> children (makeTerm <$> location <*> manyTerm switchCase)
    switchCase = makeTerm <$> (symbol SwitchCase <|> symbol SwitchDefault) <*> children (Statement.Pattern <$> (term expressions <|> emptyTerm) <*> (makeTerm <$> location <*> manyTerm statement))

subscriptExpression :: Assignment
subscriptExpression = makeTerm <$> symbol SubscriptExpression <*> children (Expression.Subscript <$> term expression <*> (pure <$> term expressions))

pair :: Assignment
pair = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> term propertyName <*> term expression)

callExpression :: Assignment
callExpression = makeCall <$> (symbol CallExpression <|> symbol CallExpression') <*> children ((,,,) <$> term (expression <|> super <|> function) <*> (typeArguments <|> pure []) <*> (arguments <|> (pure <$> term templateString)) <*> emptyTerm)
  where makeCall loc (subject, typeArgs, args, body) = makeTerm loc (Expression.Call typeArgs subject args body)
        arguments = symbol Arguments *> children (manyTerm (expression <|> spreadElement))
        typeArguments = symbol Grammar.TypeArguments *> children (some (term ty))

tryStatement :: Assignment
tryStatement = makeTry <$> symbol TryStatement <*> children ((,,) <$> term statementBlock <*> optional (term catchClause) <*> optional (term finallyClause))
  where
    makeTry loc (statementBlock', catch, finally) = makeTerm loc (Statement.Try statementBlock' (catMaybes [catch, finally]))
    catchClause = makeTerm <$> symbol CatchClause <*> children (Statement.Catch <$> (identifier <|> emptyTerm) <*> statementBlock)
    finallyClause = makeTerm <$> symbol FinallyClause <*> children (Statement.Finally <$> statementBlock)

binaryExpression  :: Assignment
binaryExpression = makeTerm' <$> symbol BinaryExpression <*> children (infixTerm expression (term expression)
  [ (inject .) . Expression.Plus               <$ symbol AnonPlus
  , (inject .) . Expression.Minus              <$ symbol AnonMinus
  , (inject .) . Expression.Times              <$ symbol AnonStar
  , (inject .) . Expression.DividedBy          <$ symbol AnonSlash
  , (inject .) . Expression.Modulo             <$ symbol AnonPercent
  , (inject .) . Expression.Member             <$ symbol AnonIn
  , (inject .) . Expression.And                <$ symbol AnonAmpersandAmpersand
  , (inject .) . Expression.BAnd               <$ symbol AnonAmpersand
  , (inject .) . Expression.Or                 <$ symbol AnonPipePipe
  , (inject .) . Expression.BOr                <$ symbol AnonPipe
  , (inject .) . Expression.BXOr               <$ symbol AnonCaret
  , (inject .) . Expression.InstanceOf         <$ symbol AnonInstanceof
  , (inject .) . Expression.Equal              <$ symbol AnonEqualEqual
  , (inject .) . Expression.StrictEqual        <$ symbol AnonEqualEqualEqual
  , (inject .) . invert Expression.Equal       <$ symbol AnonBangEqual
  , (inject .) . invert Expression.StrictEqual <$ symbol AnonBangEqualEqual
  , (inject .) . Expression.LShift             <$ symbol AnonLAngleLAngle
  , (inject .) . Expression.RShift             <$ symbol AnonRAngleRAngle
  , (inject .) . Expression.UnsignedRShift     <$ symbol AnonRAngleRAngleRAngle
  , (inject .) . Expression.LessThan           <$ symbol AnonLAngle
  , (inject .) . Expression.GreaterThan        <$ symbol AnonRAngle
  , (inject .) . Expression.LessThanEqual      <$ symbol AnonLAngleEqual
  , (inject .) . Expression.GreaterThanEqual   <$ symbol AnonRAngleEqual
  ])
  where invert cons a b = Expression.Not (makeTerm1 (cons a b))


-- Helpers

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
manyTerm term = many (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

term :: Assignment -> Assignment
term term = contextualize comment (postContextualize comment term)

emptyStatement :: Assignment
emptyStatement = makeTerm <$> symbol EmptyStatement <*> (Syntax.Empty <$ rawSource <|> pure Syntax.Empty)

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment
          -> Assignment
          -> [Assignment.Assignment [] Grammar (Term -> Term -> Sum Syntax Term)]
          -> Assignment.Assignment [] Grammar (Sum Syntax Term)
infixTerm = infixContext comment
