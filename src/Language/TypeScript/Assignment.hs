{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings, RankNTypes, TypeFamilies, TypeOperators #-}
module Language.TypeScript.Assignment
( assignment
, TypeScript.Syntax
, Grammar
, TypeScript.Term(..)
) where

import Assigning.Assignment hiding (Assignment, Error)
import Data.Abstract.Name (name)
import qualified Data.Abstract.ScopeGraph as ScopeGraph (AccessControl(..))
import qualified Assigning.Assignment as Assignment
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
import qualified Language.TypeScript.Syntax as TypeScript.Syntax
import qualified Language.TypeScript.Resolution as TypeScript.Resolution
import Language.TypeScript.Term as TypeScript
import Prologue
import TreeSitter.TypeScript as Grammar

type Assignment = Assignment.Assignment [] Grammar

-- | Assignment from AST in TypeScript’s grammar onto a program in TypeScript’s syntax.
assignment :: Assignment (Term Loc)
assignment = handleError $ makeTerm <$> symbol Program <*> children (Statement.Statements <$> manyTerm statement) <|> parseError

expression :: Assignment (Term Loc)
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
      class',
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

undefined' :: Assignment (Term Loc)
undefined' = makeTerm <$> symbol Grammar.Undefined <*> (TypeScript.Syntax.Undefined <$ rawSource)

assignmentExpression :: Assignment (Term Loc)
assignmentExpression = makeTerm <$> symbol AssignmentExpression <*> children (Statement.Assignment [] <$> term (memberExpression <|> subscriptExpression <|> identifier <|> destructuringPattern) <*> expression)

augmentedAssignmentExpression :: Assignment (Term Loc)
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
  where assign :: (f :< TypeScript.Syntax) => (Term Loc -> Term Loc -> f (Term Loc)) -> Term Loc -> Term Loc -> Sum TypeScript.Syntax (Term Loc)
        assign c l r = inject (Statement.Assignment [] l (makeTerm1 (c l r)))


awaitExpression :: Assignment (Term Loc)
awaitExpression = makeTerm <$> symbol Grammar.AwaitExpression <*> children (Expression.Await <$> term expression)

unaryExpression :: Assignment (Term Loc)
unaryExpression = symbol Grammar.UnaryExpression >>= \ loc ->
  makeTerm loc . Expression.Not <$> children (symbol AnonBang *> term expression)
  <|> makeTerm loc . Expression.Complement <$> children (symbol AnonTilde *> term expression)
  <|> makeTerm loc . Expression.Negate <$> children ((symbol AnonMinus <|> symbol AnonPlus) *> term expression)
  <|> makeTerm loc . Expression.Typeof <$> children (symbol AnonTypeof *> term expression)
  <|> makeTerm loc . Expression.Void <$> children (symbol AnonVoid *> term expression)
  <|> makeTerm loc . Expression.Delete <$> children (symbol AnonDelete *> term expression)

ternaryExpression :: Assignment (Term Loc)
ternaryExpression = makeTerm <$> symbol Grammar.TernaryExpression <*> children (Statement.If <$> term expression <*> term expression <*> term expression)

memberExpression :: Assignment (Term Loc)
memberExpression = makeTerm <$> (symbol Grammar.MemberExpression <|> symbol Grammar.MemberExpression') <*> children (Expression.MemberAccess <$> term expression <*> propertyIdentifier)

newExpression :: Assignment (Term Loc)
newExpression = makeTerm <$> symbol Grammar.NewExpression <*> children (Expression.New  <$> term constructableExpression <*> (typeArguments' <|> emptyTerm) <*> (arguments <|> pure []))

constructableExpression :: Assignment (Term Loc)
constructableExpression = choice [
    this
  , identifier
  , number
  , string
  , templateString
  , regex
  , true
  , false
  , null'
  , undefined'
  , object
  , array
  , function
  , arrowFunction
  , class'
  , parenthesizedExpression
  , subscriptExpression
  , memberExpression
  , metaProperty
  , newExpression
  ]

metaProperty :: Assignment (Term Loc)
metaProperty = makeTerm <$> symbol Grammar.MetaProperty <*> (TypeScript.Syntax.MetaProperty <$ rawSource)

updateExpression :: Assignment (Term Loc)
updateExpression = makeTerm <$> symbol Grammar.UpdateExpression <*> children (TypeScript.Syntax.Update <$> term expression)

yieldExpression :: Assignment (Term Loc)
yieldExpression = makeTerm <$> symbol Grammar.YieldExpression <*> children (Statement.Yield <$> term (expression <|> emptyTerm))

this :: Assignment (Term Loc)
this = makeTerm <$> symbol Grammar.This <*> (Expression.This <$ rawSource)

regex :: Assignment (Term Loc)
regex = makeTerm <$> symbol Grammar.Regex <*> (Literal.Regex <$> source)

null' :: Assignment (Term Loc)
null' = makeTerm <$> symbol Null <*> (Literal.Null <$ rawSource)

abstractClass :: Assignment (Term Loc)
abstractClass = makeTerm <$> symbol Grammar.AbstractClassDeclaration <*> children (TypeScript.Syntax.AbstractClass <$> term typeIdentifier <*> (term typeParameters <|> emptyTerm) <*> (classHeritage' <|> pure []) <*> classBodyStatements)

abstractMethodSignature :: Assignment (Term Loc)
abstractMethodSignature = makeSignature <$> symbol Grammar.AbstractMethodSignature <*> children ((,,) <$> accessibilityModifier' <*> term propertyName <*> callSignatureParts)
  where makeSignature loc (modifier, propertyName, (typeParams, params, annotation)) = makeTerm loc (TypeScript.Syntax.AbstractMethodSignature [typeParams, annotation] propertyName params modifier)

classHeritage' :: Assignment [Term Loc]
classHeritage' = symbol Grammar.ClassHeritage *> children ((mappend `on` toList) <$> optional (term extendsClause) <*> optional (term implementsClause'))

extendsClause :: Assignment (Term Loc)
extendsClause = makeTerm <$> symbol Grammar.ExtendsClause <*> children (TypeScript.Syntax.ExtendsClause <$> manyTerm (typeReference <|> expression))

typeReference :: Assignment (Term Loc)
typeReference = typeIdentifier <|> nestedTypeIdentifier <|> genericType

implementsClause' :: Assignment (Term Loc)
implementsClause' = makeTerm <$> symbol Grammar.ImplementsClause <*> children (TypeScript.Syntax.ImplementsClause <$> manyTerm ty)

super :: Assignment (Term Loc)
super = makeTerm <$> symbol Grammar.Super <*> (TypeScript.Syntax.Super <$ rawSource)

typeAssertion :: Assignment (Term Loc)
typeAssertion = makeTerm <$> symbol Grammar.TypeAssertion <*> children (TypeScript.Syntax.TypeAssertion <$> term typeArguments' <*> term expression)

asExpression :: Assignment (Term Loc)
asExpression = makeTerm <$> symbol AsExpression <*> children (Expression.Cast <$> term expression <*> term (ty <|> templateString))

templateString :: Assignment (Term Loc)
templateString = makeTerm <$> symbol TemplateString <*> children (Literal.String <$> manyTerm templateSubstitution)

templateSubstitution :: Assignment (Term Loc)
templateSubstitution = symbol TemplateSubstitution *> children (term expressions)

nonNullExpression' :: Assignment (Term Loc)
nonNullExpression' = makeTerm <$> symbol Grammar.NonNullExpression <*> children (Expression.NonNullExpression <$> term expression)

importAlias' :: Assignment (Term Loc)
importAlias' = makeTerm <$> symbol Grammar.ImportAlias <*> children (TypeScript.Syntax.ImportAlias <$> term identifier <*> term (identifier <|> nestedIdentifier))

number :: Assignment (Term Loc)
number = makeTerm <$> symbol Grammar.Number <*> (Literal.Float <$> source)

string :: Assignment (Term Loc)
string = makeTerm <$> symbol Grammar.String <*> (Literal.TextElement <$> source)

true :: Assignment (Term Loc)
true = makeTerm <$> symbol Grammar.True <*> (Literal.true <$ rawSource)

false :: Assignment (Term Loc)
false = makeTerm <$> symbol Grammar.False <*> (Literal.false <$ rawSource)

identifier :: Assignment (Term Loc)
identifier = makeTerm <$> symbol Identifier <*> (Syntax.Identifier . name <$> source)

class' :: Assignment (Term Loc)
class' = makeClass <$> (symbol Class <|> symbol ClassDeclaration) <*> children ((,,,,) <$> manyTerm decorator
                                                                                       <*> (term typeIdentifier <|> emptyTerm)
                                                                                       <*> (symbol TypeParameters *> children (manyTerm typeParameter') <|> pure [])
                                                                                       <*> (classHeritage' <|> pure [])
                                                                                       <*> classBodyStatements)
  where makeClass loc (decorators, expression, typeParams, classHeritage, statements) = makeTerm loc (Declaration.Class (decorators <> typeParams) expression classHeritage statements)

object :: Assignment (Term Loc)
object = makeTerm <$> (symbol Object <|> symbol ObjectPattern) <*> children (Literal.Hash <$> manyTerm (pair <|> spreadElement <|> methodDefinition <|> assignmentPattern <|> shorthandPropertyIdentifier))

array :: Assignment (Term Loc)
array = makeTerm <$> (symbol Array <|> symbol ArrayPattern) <*> children (Literal.Array <$> manyTerm (expression <|> spreadElement))

propertyIdentifier :: Assignment (Term Loc)
propertyIdentifier = makeTerm <$> symbol PropertyIdentifier <*> (Syntax.Identifier . name <$> source)

sequenceExpression :: Assignment (Term Loc)
sequenceExpression = makeTerm <$> symbol Grammar.SequenceExpression <*> children (Expression.SequenceExpression <$> term expression <*> term expressions)

expressions :: Assignment (Term Loc)
expressions = annotatedExpression <|> expression <|> sequenceExpression

annotatedExpression :: Assignment (Term Loc)
annotatedExpression = mkAnnotated <$> location <*> expression <*> typeAnnotation'
  where mkAnnotated loc expr ann = makeTerm loc (TypeScript.Syntax.AnnotatedExpression expr ann)

parameter :: Assignment (Term Loc)
parameter =  requiredParameter
         <|> restParameter
         <|> optionalParameter

accessibilityModifier' :: Assignment ScopeGraph.AccessControl
accessibilityModifier' = (symbol AccessibilityModifier >> children (public <|> protected <|> private)) <|> default'
  where public    = symbol AnonPublic    >> pure ScopeGraph.Public
        protected = symbol AnonProtected >> pure ScopeGraph.Protected
        private   = symbol AnonPrivate   >> pure ScopeGraph.Private
        default'  = pure ScopeGraph.Public


destructuringPattern :: Assignment (Term Loc)
destructuringPattern = object <|> array

spreadElement :: Assignment (Term Loc)
spreadElement = symbol SpreadElement *> children (term expression)

readonly' :: Assignment (Term Loc)
readonly' = makeTerm <$> symbol Readonly <*> (Type.Readonly <$ rawSource)

methodDefinition :: Assignment (Term Loc)
methodDefinition = makeMethod <$>
  symbol MethodDefinition
  <*> children ((,,,,,) <$> accessibilityModifier' <*> (term readonly' <|> emptyTerm) <*> emptyTerm <*> term propertyName <*> callSignatureParts <*> term statementBlock)
  where
    makeMethod loc (modifier, readonly, receiver, propertyName', (typeParameters', params, ty'), statements) = makeTerm loc (Declaration.Method [readonly, typeParameters', ty'] receiver propertyName' params statements modifier)

callSignatureParts :: Assignment (Term Loc, [Term Loc], Term Loc)
callSignatureParts = contextualize' <$> Assignment.manyThrough comment (postContextualize' <$> callSignature' <*> many comment)
  where
    callSignature' = (,,) <$> (term typeParameters <|> emptyTerm) <*> formalParameters <*> (term typeAnnotation' <|> emptyTerm)
    contextualize' (cs, (typeParams, formalParams, annotation)) = case nonEmpty cs of
      Just cs -> (makeTerm1 (Syntax.Context cs typeParams), formalParams, annotation)
      Nothing -> (typeParams, formalParams, annotation)
    postContextualize' (typeParams, formalParams, annotation) cs = case nonEmpty cs of
      Just cs -> (typeParams, formalParams, makeTerm1 (Syntax.Context cs annotation))
      Nothing -> (typeParams, formalParams, annotation)

callSignature :: Assignment (Term Loc)
callSignature =  makeTerm <$> symbol Grammar.CallSignature <*> children (TypeScript.Syntax.CallSignature <$> (fromMaybe <$> emptyTerm <*> optional (term typeParameters)) <*> formalParameters <*> (fromMaybe <$> emptyTerm <*> optional (term typeAnnotation')))

constructSignature :: Assignment (Term Loc)
constructSignature = makeTerm <$> symbol Grammar.ConstructSignature <*> children (TypeScript.Syntax.ConstructSignature <$> (fromMaybe <$> emptyTerm <*> optional (term typeParameters)) <*> formalParameters <*> (fromMaybe <$> emptyTerm <*> optional (term typeAnnotation')))

indexSignature :: Assignment (Term Loc)
indexSignature = makeTerm <$> symbol Grammar.IndexSignature <*> children (TypeScript.Syntax.IndexSignature <$> term identifier <*> predefinedTy <*> term typeAnnotation')

methodSignature :: Assignment (Term Loc)
methodSignature = makeMethodSignature <$> symbol Grammar.MethodSignature <*> children ((,,,) <$> accessibilityModifier' <*> (term readonly' <|> emptyTerm) <*> term propertyName <*> callSignatureParts)
  where makeMethodSignature loc (accessControl, readonly, propertyName, (typeParams, params, annotation)) = makeTerm loc (Declaration.MethodSignature [readonly, typeParams, annotation] propertyName params accessControl)

formalParameters :: Assignment [Term Loc]
formalParameters = symbol FormalParameters *> children (contextualize' <$> Assignment.manyThrough comment (postContextualize' <$> (concat <$> many ((\as b -> as <> [b]) <$> manyTerm decorator <*> term parameter)) <*> many comment))
  where
    contextualize' (cs, formalParams) = case nonEmpty cs of
      Just cs -> toList cs <> formalParams
      Nothing -> formalParams
    postContextualize' formalParams cs = case nonEmpty cs of
      Just cs -> formalParams <> toList cs
      Nothing -> formalParams


decorator :: Assignment (Term Loc)
decorator = makeTerm <$> symbol Grammar.Decorator <*> children (TypeScript.Syntax.Decorator <$> term (identifier <|> memberExpression <|> callExpression))

typeParameters :: Assignment (Term Loc)
typeParameters = makeTerm <$> symbol TypeParameters <*> children (Type.TypeParameters <$> manyTerm typeParameter')

typeAnnotation' :: Assignment (Term Loc)
typeAnnotation' = makeTerm <$> symbol TypeAnnotation <*> children (TypeScript.Syntax.Annotation <$> term ty)

typeParameter' :: Assignment (Term Loc)
typeParameter' = makeTerm <$> symbol Grammar.TypeParameter <*> children (TypeScript.Syntax.TypeParameter <$> term typeIdentifier <*> term (constraint <|> emptyTerm) <*> term (defaultType <|> emptyTerm))

defaultType :: Assignment (Term Loc)
defaultType = makeTerm <$> symbol Grammar.DefaultType <*> children (TypeScript.Syntax.DefaultType <$> term ty)

constraint :: Assignment (Term Loc)
constraint = makeTerm <$> symbol Grammar.Constraint <*> children (TypeScript.Syntax.Constraint <$> term ty)

function :: Assignment (Term Loc)
function = makeFunction <$> (symbol Grammar.Function <|> symbol Grammar.FunctionDeclaration <|> symbol Grammar.GeneratorFunction <|> symbol Grammar.GeneratorFunctionDeclaration) <*> children ((,,) <$> term (identifier <|> emptyTerm) <*> callSignatureParts <*> term statementBlock)
  where makeFunction loc (id, (typeParams, params, annotation), statements) = makeTerm loc (Declaration.Function [typeParams, annotation] id params statements)

-- TODO: FunctionSignatures can, but don't have to be ambient functions.
ambientFunction :: Assignment (Term Loc)
ambientFunction = makeAmbientFunction <$> symbol Grammar.FunctionSignature <*> children ((,) <$> term identifier <*> callSignatureParts)
  where makeAmbientFunction loc (id, (typeParams, params, annotation)) = makeTerm loc (TypeScript.Syntax.AmbientFunction [typeParams, annotation] id params)

ty :: Assignment (Term Loc)
ty = primaryType <|> unionType <|> intersectionType <|> functionTy <|> constructorTy

primaryType :: Assignment (Term Loc)
primaryType =  arrayTy
           <|> existentialType
           <|> flowMaybeTy
           <|> genericType
           <|> indexTypeQuery
           <|> literalType
           <|> lookupType
           <|> nestedTypeIdentifier
           <|> objectType
           <|> parenthesizedTy
           <|> predefinedTy
           <|> this
           <|> tupleType
           <|> typeIdentifier
           <|> typePredicate
           <|> typeQuery

parenthesizedTy :: Assignment (Term Loc)
parenthesizedTy = makeTerm <$> symbol Grammar.ParenthesizedType <*> children (TypeScript.Syntax.ParenthesizedType <$> term ty)

predefinedTy :: Assignment (Term Loc)
predefinedTy = makeTerm <$> symbol Grammar.PredefinedType <*> (TypeScript.Syntax.PredefinedType <$> source)

typeIdentifier :: Assignment (Term Loc)
typeIdentifier = makeTerm <$> symbol Grammar.TypeIdentifier <*> (TypeScript.Syntax.TypeIdentifier <$> source)

nestedIdentifier :: Assignment (Term Loc)
nestedIdentifier = makeTerm <$> symbol Grammar.NestedIdentifier <*> children (TypeScript.Syntax.NestedIdentifier <$> term (identifier <|> nestedIdentifier) <*> term identifier)

nestedTypeIdentifier :: Assignment (Term Loc)
nestedTypeIdentifier = makeTerm <$> symbol Grammar.NestedTypeIdentifier <*> children (TypeScript.Syntax.NestedTypeIdentifier <$> term (identifier <|> nestedIdentifier) <*> term typeIdentifier)

genericType :: Assignment (Term Loc)
genericType = makeTerm <$> symbol Grammar.GenericType <*> children (TypeScript.Syntax.GenericType <$> term (typeIdentifier <|> nestedTypeIdentifier) <*> term typeArguments')

typeArguments' :: Assignment (Term Loc)
typeArguments' = makeTerm <$> symbol Grammar.TypeArguments <*> children (TypeScript.Syntax.TypeArguments <$> some (term ty))

typePredicate :: Assignment (Term Loc)
typePredicate = makeTerm <$> symbol Grammar.TypePredicate <*> children (TypeScript.Syntax.TypePredicate <$> term identifier <*> term ty)

objectType :: Assignment (Term Loc)
objectType = makeTerm <$> symbol Grammar.ObjectType <*> children (TypeScript.Syntax.ObjectType <$> manyTerm (exportStatement <|> propertySignature <|> callSignature <|> constructSignature <|> indexSignature <|> methodSignature))

arrayTy :: Assignment (Term Loc)
arrayTy = makeTerm <$> symbol Grammar.ArrayType <*> children (TypeScript.Syntax.ArrayType <$> term ty)

lookupType :: Assignment (Term Loc)
lookupType = makeTerm <$> symbol Grammar.LookupType <*> children (TypeScript.Syntax.LookupType <$> term (typeIdentifier <|> nestedTypeIdentifier) <*> term ty)

flowMaybeTy :: Assignment (Term Loc)
flowMaybeTy = makeTerm <$> symbol Grammar.FlowMaybeType <*> children (TypeScript.Syntax.FlowMaybeType <$> term primaryType)

typeQuery :: Assignment (Term Loc)
typeQuery = makeTerm <$> symbol Grammar.TypeQuery <*> children (TypeScript.Syntax.TypeQuery <$> term (identifier <|> nestedIdentifier))

indexTypeQuery :: Assignment (Term Loc)
indexTypeQuery = makeTerm <$> symbol Grammar.IndexTypeQuery <*> children (TypeScript.Syntax.IndexTypeQuery <$> term (typeIdentifier <|> nestedTypeIdentifier))

existentialType :: Assignment (Term Loc)
existentialType = makeTerm <$> symbol Grammar.ExistentialType <*> (TypeScript.Syntax.ExistentialType <$> source)

literalType :: Assignment (Term Loc)
literalType = makeTerm <$> symbol Grammar.LiteralType <*> children (TypeScript.Syntax.LiteralType <$> term (number <|> string <|> true <|> false))

unionType :: Assignment (Term Loc)
unionType = makeTerm <$> symbol UnionType <*> children (TypeScript.Syntax.Union <$> (term ty <|> emptyTerm) <*> term ty)

intersectionType :: Assignment (Term Loc)
intersectionType = makeTerm <$> symbol IntersectionType <*> children (TypeScript.Syntax.Intersection <$> term ty <*> term ty)

functionTy :: Assignment (Term Loc)
functionTy = makeTerm <$> symbol Grammar.FunctionType <*> children (TypeScript.Syntax.FunctionType <$> (fromMaybe <$> emptyTerm <*> optional (term typeParameters)) <*> formalParameters <*> term ty)

tupleType :: Assignment (Term Loc)
tupleType = makeTerm <$> symbol TupleType <*> children (TypeScript.Syntax.Tuple <$> manyTerm ty)

constructorTy :: Assignment (Term Loc)
constructorTy = makeTerm <$> symbol ConstructorType <*> children (TypeScript.Syntax.Constructor <$> (fromMaybe <$> emptyTerm <*> optional (term typeParameters)) <*> formalParameters <*> term ty)

statementTerm :: Assignment (Term Loc)
statementTerm = makeTerm <$> symbol StatementBlock <*> children (Statement.Statements <$> manyTerm statement)

statementBlock :: Assignment (Term Loc)
statementBlock = makeTerm <$> symbol StatementBlock <*> children (Statement.StatementBlock <$> manyTerm statement)

classBodyStatements :: Assignment (Term Loc)
classBodyStatements = makeTerm'' <$> symbol ClassBody <*> children (contextualize' <$> Assignment.manyThrough comment (postContextualize' <$> (concat <$> many ((\as b -> as <> [b]) <$> manyTerm decorator <*> term (methodDefinition <|> publicFieldDefinition <|> methodSignature <|> indexSignature <|> abstractMethodSignature))) <*> many comment))
  where
    contextualize' (cs, formalParams) = case nonEmpty cs of
      Just cs -> toList cs <> formalParams
      Nothing -> formalParams
    postContextualize' formalParams cs = case nonEmpty cs of
      Just cs -> formalParams <> toList cs
      Nothing -> formalParams

publicFieldDefinition :: Assignment (Term Loc)
publicFieldDefinition = makeField <$> symbol Grammar.PublicFieldDefinition <*> children ((,,,,) <$> accessibilityModifier' <*> (term readonly' <|> emptyTerm) <*> term propertyName <*> (term typeAnnotation' <|> emptyTerm) <*> (term expression <|> emptyTerm))
  where makeField loc (accessControl, readonly, propertyName, annotation, expression) = makeTerm loc (Declaration.PublicFieldDefinition [readonly, annotation] propertyName expression accessControl)


statement :: Assignment (Term Loc)
statement = handleError everything
  where
    everything = choice [
      exportStatement
      , importStatement
      , debuggerStatement
      , expressionStatement'
      , declaration
      , statementTerm
      , ifStatement
      , switchStatement
      , forStatement
      , forInStatement
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

forInStatement :: Assignment (Term Loc)
forInStatement = makeTerm <$> symbol ForInStatement <*> children (Statement.ForEach <$> term expression <*> term expression <*> term statement)

doStatement :: Assignment (Term Loc)
doStatement = makeTerm <$> symbol DoStatement <*> children (flip Statement.DoWhile <$> term statement <*> term parenthesizedExpression)

continueStatement :: Assignment (Term Loc)
continueStatement = makeTerm <$> symbol ContinueStatement <*> children (Statement.Continue <$> (statementIdentifier <|> term emptyTerm))

breakStatement :: Assignment (Term Loc)
breakStatement = makeTerm <$> symbol BreakStatement <*> children (Statement.Break <$> (statementIdentifier <|> term emptyTerm))

withStatement :: Assignment (Term Loc)
withStatement = makeTerm <$> symbol WithStatement <*> children (TypeScript.Syntax.With <$> term parenthesizedExpression <*> term statement)

returnStatement :: Assignment (Term Loc)
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> (term expressions <|> term emptyTerm))

throwStatement :: Assignment (Term Loc)
throwStatement = makeTerm <$> symbol Grammar.ThrowStatement <*> children (Statement.Throw <$> term expressions)

hashBang :: Assignment (Term Loc)
hashBang = makeTerm <$> symbol HashBangLine <*> (Comment.HashBang <$> source)

labeledStatement :: Assignment (Term Loc)
labeledStatement = makeTerm <$> symbol Grammar.LabeledStatement <*> children (TypeScript.Syntax.LabeledStatement <$> statementIdentifier <*> term statement)

statementIdentifier :: Assignment (Term Loc)
statementIdentifier = makeTerm <$> symbol StatementIdentifier <*> (Syntax.Identifier . name <$> source)

importStatement :: Assignment (Term Loc)
importStatement =   makeImportTerm <$> symbol Grammar.ImportStatement <*> children ((,) <$> importClause <*> fromClause)
                <|> makeTerm' <$> symbol Grammar.ImportStatement <*> children (requireImport <|> sideEffectImport)
  where
    -- `import foo = require("./foo")`
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
    makeImportTerm1 loc from (Nothing, symbols) = makeTerm loc (TypeScript.Syntax.Import (uncurry TypeScript.Syntax.Alias <$> symbols) from)
    makeImportTerm loc ([x], from) = makeImportTerm1 loc from x
    makeImportTerm loc (xs, from) = makeTerm loc $ fmap (makeImportTerm1 loc from) xs
    importSymbol = symbol Grammar.ImportSpecifier *> children (makeNameAliasPair <$> rawIdentifier <*> ((Just <$> rawIdentifier) <|> pure Nothing))
    rawIdentifier = symbol Identifier *> (name <$> source)
    makeNameAliasPair from (Just alias) = (from, alias)
    makeNameAliasPair from Nothing = (from, from)

    -- TODO: Need to validate that inline comments are still handled with this change in assigning to Path and not a Term.
    fromClause = symbol Grammar.String *> (TypeScript.Resolution.importPath <$> source)

debuggerStatement :: Assignment (Term Loc)
debuggerStatement = makeTerm <$> symbol Grammar.DebuggerStatement <*> (TypeScript.Syntax.Debugger <$ rawSource)

expressionStatement' :: Assignment (Term Loc)
expressionStatement' = symbol ExpressionStatement *> children (term expressions)

declaration :: Assignment (Term Loc)
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

typeAliasDeclaration :: Assignment (Term Loc)
typeAliasDeclaration = makeTypeAliasDecl <$> symbol Grammar.TypeAliasDeclaration <*> children ((,,) <$> term typeIdentifier <*> (term typeParameters <|> emptyTerm) <*> term ty)
  where makeTypeAliasDecl loc (identifier, typeParams, body) = makeTerm loc (Declaration.TypeAlias [typeParams] identifier body)

enumDeclaration :: Assignment (Term Loc)
enumDeclaration = makeTerm <$> symbol Grammar.EnumDeclaration <*> children (TypeScript.Syntax.EnumDeclaration <$> term identifier <*> (symbol EnumBody *> children (manyTerm (propertyName <|> enumAssignment))))

enumAssignment :: Assignment (Term Loc)
enumAssignment = makeTerm <$> symbol Grammar.EnumAssignment <*> children (Statement.Assignment [] <$> term propertyName <*> term expression)

interfaceDeclaration :: Assignment (Term Loc)
interfaceDeclaration = makeInterfaceDecl <$> symbol Grammar.InterfaceDeclaration <*> children ((,,,) <$> term typeIdentifier <*> (term typeParameters <|> emptyTerm) <*> optional (term extendsClause) <*> term objectType)
  where makeInterfaceDecl loc (identifier, typeParams, clause, objectType) = makeTerm loc (Declaration.InterfaceDeclaration [typeParams] identifier (toList clause) objectType)

ambientDeclaration :: Assignment (Term Loc)
ambientDeclaration = makeTerm <$> symbol Grammar.AmbientDeclaration <*> children (TypeScript.Syntax.AmbientDeclaration <$> term (choice [propertyIdentifier *> ty, declaration, statementBlock]))

exportStatement :: Assignment (Term Loc)
exportStatement = makeTerm <$> symbol Grammar.ExportStatement <*> children (flip TypeScript.Syntax.QualifiedExportFrom <$> exportClause <*> fromClause)
  <|> makeTerm <$> symbol Grammar.ExportStatement <*> children (TypeScript.Syntax.QualifiedExport <$> exportClause)
  <|> makeTerm <$> symbol Grammar.ExportStatement <*> children (TypeScript.Syntax.DefaultExport <$> contextualize decorator (term (declaration <|> expression <|> identifier <|> importAlias')))
  where
    exportClause = symbol Grammar.ExportClause *> children (many exportSymbol)
    exportSymbol = symbol Grammar.ExportSpecifier *> children (makeNameAliasPair <$> rawIdentifier <*> (Just <$> rawIdentifier))
                 <|> symbol Grammar.ExportSpecifier *> children (makeNameAliasPair <$> rawIdentifier <*> pure Nothing)
    makeNameAliasPair from (Just alias) = TypeScript.Syntax.Alias from alias
    makeNameAliasPair from Nothing = TypeScript.Syntax.Alias from from
    rawIdentifier = symbol Identifier *> (name <$> source)
    -- TODO: Need to validate that inline comments are still handled with this change in assigning to Path and not a Term.
    fromClause = symbol Grammar.String *> (TypeScript.Resolution.importPath <$> source)

propertySignature :: Assignment (Term Loc)
propertySignature = makePropertySignature <$> symbol Grammar.PropertySignature <*> children ((,,,) <$> accessibilityModifier' <*> (term readonly' <|> emptyTerm) <*> term propertyName <*> (term typeAnnotation' <|> emptyTerm))
  where makePropertySignature loc (modifier, readonly, propertyName, annotation) = makeTerm loc (TypeScript.Syntax.PropertySignature [readonly, annotation] propertyName modifier)

propertyName :: Assignment (Term Loc)
propertyName = term (propertyIdentifier <|> string <|> number <|> computedPropertyName)

computedPropertyName :: Assignment (Term Loc)
computedPropertyName = makeTerm <$> symbol Grammar.ComputedPropertyName <*> children (TypeScript.Syntax.ComputedPropertyName <$> term expression)

assignmentPattern :: Assignment (Term Loc)
assignmentPattern = makeTerm <$> symbol AssignmentPattern <*> children (Statement.Assignment [] <$> term shorthandPropertyIdentifier <*> term expression)

shorthandPropertyIdentifier :: Assignment (Term Loc)
shorthandPropertyIdentifier = makeTerm <$> symbol Grammar.ShorthandPropertyIdentifier <*> (TypeScript.Syntax.ShorthandPropertyIdentifier <$> source)

requiredParameter :: Assignment (Term Loc)
requiredParameter = makeRequiredParameter
                 <$> symbol Grammar.RequiredParameter
                 <*> children ( (,,,,)
                             <$> accessibilityModifier'
                             <*> (term readonly' <|> emptyTerm)
                             <*> term (identifier <|> destructuringPattern <|> this)
                             <*> (term typeAnnotation' <|> emptyTerm)
                             <*> (term expression <|> emptyTerm))
  where
    makeRequiredParameter loc (modifier, readonly, identifier, annotation, initializer) = makeTerm loc (TypeScript.Syntax.RequiredParameter [readonly, annotation] identifier initializer modifier)

restParameter :: Assignment (Term Loc)
restParameter = makeRestParameter <$> symbol Grammar.RestParameter <*> children ((,) <$> term identifier <*> (term typeAnnotation' <|> emptyTerm))
  where makeRestParameter loc (identifier, annotation) = makeTerm loc (TypeScript.Syntax.RestParameter [annotation] identifier)

optionalParameter :: Assignment (Term Loc)
optionalParameter = makeOptionalParam <$> symbol Grammar.OptionalParameter <*> children ((,,,,) <$> accessibilityModifier' <*> (term readonly' <|> emptyTerm) <*> (term identifier <|> destructuringPattern) <*> (term typeAnnotation' <|> emptyTerm) <*> (term expression <|> emptyTerm))
  where makeOptionalParam loc (modifier, readonly, subject, annotation, initializer) = makeTerm loc (TypeScript.Syntax.OptionalParameter [readonly, annotation] (makeTerm loc (Statement.Assignment [] subject initializer)) modifier)

internalModule :: Assignment (Term Loc)
internalModule = makeTerm <$> symbol Grammar.InternalModule <*> children (TypeScript.Syntax.InternalModule <$> term (string <|> identifier <|> nestedIdentifier) <*> statements)

module' :: Assignment (Term Loc)
module' = makeTerm <$> symbol Module <*> children (TypeScript.Syntax.Module <$> term (string <|> identifier <|> nestedIdentifier) <*> (statements <|> pure []))


statements :: Assignment [Term Loc]
statements = symbol StatementBlock *> children (manyTerm statement)

arrowFunction :: Assignment (Term Loc)
arrowFunction = makeArrowFun <$> symbol ArrowFunction <*> children ((,,) <$> emptyTerm <*> (((\a b c -> (a, [b], c)) <$> emptyTerm <*> term identifier <*> emptyTerm) <|> callSignatureParts) <*> term (expression <|> statementBlock))
  where makeArrowFun loc (identifier, (typeParams, params, returnTy), body) = makeTerm loc (Declaration.Function [ typeParams, returnTy ] identifier params body)

comment :: Assignment (Term Loc)
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

ifStatement :: Assignment (Term Loc)
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> term parenthesizedExpression <*> term statement <*> (term statement <|> emptyTerm))

whileStatement :: Assignment (Term Loc)
whileStatement = makeTerm <$> symbol WhileStatement <*> children (Statement.While <$> term expression <*> term statement)

forStatement :: Assignment (Term Loc)
forStatement = makeTerm <$> symbol ForStatement <*> children (Statement.For <$> term (variableDeclaration <|> expressionStatement' <|> emptyStatement) <*> term (expressionStatement' <|> emptyStatement) <*> term (expressions <|> emptyTerm) <*> term statement)

variableDeclaration :: Assignment (Term Loc)
variableDeclaration = makeTerm <$> (symbol Grammar.VariableDeclaration <|> symbol Grammar.LexicalDeclaration) <*> children (Declaration.VariableDeclaration <$> manyTerm variableDeclarator)

variableDeclarator :: Assignment (Term Loc)
variableDeclarator =
      makeTerm <$> symbol VariableDeclarator <*> children (TypeScript.Syntax.JavaScriptRequire <$> identifier <*> requireCall)
  <|> makeVarDecl <$> symbol VariableDeclarator <*> children ((,,) <$> term (identifier <|> destructuringPattern) <*> (term typeAnnotation' <|> emptyTerm) <*> (term expression <|> emptyTerm))
  where
    makeVarDecl loc (subject, annotations, value) = makeTerm loc (Statement.Assignment [annotations] subject value)

    requireCall = symbol CallExpression *> children (symbol Identifier *> do
      s <- source
      guard (s == "require")
      symbol Arguments *> children (symbol Grammar.String *> (TypeScript.Resolution.importPath <$> source))
      )


parenthesizedExpression :: Assignment (Term Loc)
parenthesizedExpression = symbol ParenthesizedExpression *> children (term expressions)

switchStatement :: Assignment (Term Loc)
switchStatement = makeTerm <$> symbol SwitchStatement <*> children (Statement.Match <$> term parenthesizedExpression <*> term switchBody)
  where
    switchBody =  symbol SwitchBody *> children (makeTerm <$> location <*> manyTerm switchCase)
    switchCase = makeTerm <$> (symbol SwitchCase <|> symbol SwitchDefault) <*> children (Statement.Pattern <$> (term expressions <|> emptyTerm) <*> (makeTerm <$> location <*> manyTerm statement))

subscriptExpression :: Assignment (Term Loc)
subscriptExpression = makeTerm <$> symbol SubscriptExpression <*> children (Expression.Subscript <$> term expression <*> (pure <$> term expressions))

pair :: Assignment (Term Loc)
pair = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> term propertyName <*> term expression)

callExpression :: Assignment (Term Loc)
callExpression = makeCall <$> (symbol CallExpression <|> symbol CallExpression') <*> children ((,,,) <$> term (expression <|> super <|> function) <*> (typeArguments <|> pure []) <*> (arguments <|> (pure <$> term templateString)) <*> emptyTerm)
  where makeCall loc (subject, typeArgs, args, body) = makeTerm loc (Expression.Call typeArgs subject args body)
        typeArguments = symbol Grammar.TypeArguments *> children (some (term ty))

arguments :: Assignment [Term Loc]
arguments = symbol Arguments *> children (manyTerm (expression <|> spreadElement))

tryStatement :: Assignment (Term Loc)
tryStatement = makeTry <$> symbol TryStatement <*> children ((,,) <$> term statementTerm <*> optional (term catchClause) <*> optional (term finallyClause))
  where
    makeTry loc (statementBlock', catch, finally) = makeTerm loc (Statement.Try statementBlock' (catMaybes [catch, finally]))
    catchClause = makeTerm <$> symbol CatchClause <*> children (Statement.Catch <$> (identifier <|> emptyTerm) <*> statementTerm)
    finallyClause = makeTerm <$> symbol FinallyClause <*> children (Statement.Finally <$> statementTerm)

binaryExpression  :: Assignment (Term Loc)
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
manyTerm :: Assignment (Term Loc) -> Assignment [Term Loc]
manyTerm term = many (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

term :: Assignment (Term Loc) -> Assignment (Term Loc)
term term = contextualize comment (postContextualize comment term)

emptyStatement :: Assignment (Term Loc)
emptyStatement = makeTerm <$> symbol EmptyStatement <*> (Syntax.Empty <$ rawSource <|> pure Syntax.Empty)

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment (Term Loc)
          -> Assignment (Term Loc)
          -> [Assignment (Term Loc -> Term Loc -> Sum TypeScript.Syntax (Term Loc))]
          -> Assignment (Sum TypeScript.Syntax (Term Loc))
infixTerm = infixContext comment
