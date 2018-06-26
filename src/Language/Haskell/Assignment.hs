{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.Haskell.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Assigning.Assignment hiding (Assignment, Error, count)
import Data.ByteString.Char8 (count)
import Data.Record
import Data.Sum
import Data.Syntax (emptyTerm, handleError, parseError, makeTerm, makeTerm1, makeTerm', makeTerm'', contextualize, postContextualize)
import Language.Haskell.Grammar as Grammar
import qualified Assigning.Assignment as Assignment
import qualified Data.Abstract.Name as Name
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import qualified Data.Term as Term
import qualified Language.Haskell.Syntax as Syntax
import Prologue

type Syntax = '[
    Comment.Comment
  , Declaration.Constructor
  , Declaration.Datatype
  , Declaration.Function
  , Literal.Array
  , Literal.Character
  , Literal.Float
  , Literal.Integer
  , Literal.TextElement
  , Literal.Tuple
  , Statement.If
  , Statement.Match
  , Statement.Pattern
  , Syntax.AllConstructors
  , Syntax.AnnotatedTypeVariable
  , Syntax.App
  , Syntax.ArithmeticSequence
  , Syntax.AsPattern
  , Syntax.BindPattern
  , Syntax.CaseGuardPattern
  , Syntax.Class
  , Syntax.ConstructorIdentifier
  , Syntax.ConstructorOperator
  , Syntax.ConstructorPattern
  , Syntax.ConstructorSymbol
  , Syntax.Context
  , Syntax.Context'
  , Syntax.CPPDirective
  , Syntax.DefaultDeclaration
  , Syntax.DefaultSignature
  , Syntax.Deriving
  , Syntax.Do
  , Syntax.Empty
  , Syntax.Error
  , Syntax.EqualityConstraint
  , Syntax.Export
  , Syntax.ExpressionTypeSignature
  , Syntax.Field
  , Syntax.FieldBind
  , Syntax.FieldPattern
  , Syntax.Fixity'
  , Syntax.FunctionalDependency
  , Syntax.FunctionConstructor
  , Syntax.FunctionGuardPattern
  , Syntax.FunctionType
  , Syntax.GADT
  , Syntax.GADTConstructor
  , Syntax.Generator
  , Syntax.Guard
  , Syntax.HiddenImport
  , Syntax.Identifier
  , Syntax.InfixConstructorIdentifier
  , Syntax.InfixOperatorApp
  , Syntax.InfixVariableIdentifier
  , Syntax.ImplicitParameterIdentifier
  , Syntax.Import
  , Syntax.ImportAlias
  , Syntax.ImportDeclaration
  , Syntax.InfixDataConstructor
  , Syntax.InfixOperatorPattern
  , Syntax.Instance
  , Syntax.IrrefutablePattern
  , Syntax.Kind
  , Syntax.KindFunctionType
  , Syntax.KindListType
  , Syntax.KindParenthesizedConstructor
  , Syntax.KindSignature
  , Syntax.KindTupleType
  , Syntax.LabeledConstruction
  , Syntax.LabeledPattern
  , Syntax.LabeledUpdate
  , Syntax.Lambda
  , Syntax.LambdaCase
  , Syntax.LeftOperatorSection
  , Syntax.Let
  , Syntax.ListComprehension
  , Syntax.ListConstructor
  , Syntax.ListPattern
  , Syntax.Module
  , Syntax.ModuleExport
  , Syntax.ModuleIdentifier
  , Syntax.NamedFieldPun
  , Syntax.NegativeLiteral
  , Syntax.NewType
  , Syntax.PatternGuard
  , Syntax.Pragma
  , Syntax.PrefixNegation
  , Syntax.PrimitiveConstructorIdentifier
  , Syntax.PrimitiveVariableIdentifier
  , Syntax.PromotedTypeOperator
  , Syntax.QualifiedConstructorIdentifier
  , Syntax.QualifiedInfixVariableIdentifier
  , Syntax.QualifiedModuleIdentifier
  , Syntax.QualifiedImportDeclaration
  , Syntax.QualifiedTypeClassIdentifier
  , Syntax.QualifiedTypeConstructorIdentifier
  , Syntax.QualifiedVariableIdentifier
  , Syntax.QuasiQuotation
  , Syntax.QuasiQuotationDeclaration
  , Syntax.QuasiQuotationExpression
  , Syntax.QuasiQuotationExpressionBody
  , Syntax.QuasiQuotationPattern
  , Syntax.QuasiQuotationQuoter
  , Syntax.QuasiQuotationType
  , Syntax.QuotedName
  , Syntax.RecordDataConstructor
  , Syntax.RecordWildCards
  , Syntax.RightOperatorSection
  , Syntax.ScopedTypeVariables
  , Syntax.Splice
  , Syntax.StandaloneDerivingInstance
  , Syntax.Star
  , Syntax.StrictPattern
  , Syntax.StrictType
  , Syntax.StrictTypeVariable
  , Syntax.Tuple
  , Syntax.TupleConstructor
  , Syntax.TuplePattern
  , Syntax.Type
  , Syntax.TypeApp
  , Syntax.TypeClass
  , Syntax.TypeClassIdentifier
  , Syntax.TypeClassInstance
  , Syntax.TypeConstructorExport
  , Syntax.TypeConstructorIdentifier
  , Syntax.TypeFamily
  , Syntax.TypeInstance
  , Syntax.TypeOperator
  , Syntax.TypePattern
  , Syntax.TypeSignature
  , Syntax.TypeSynonym
  , Syntax.TypeVariableIdentifier
  , Syntax.UnitConstructor
  , Syntax.VariableIdentifier
  , Syntax.VariableOperator
  , Syntax.VariableSymbol
  , Syntax.ViewPattern
  , Syntax.Wildcard
  , Type.TypeParameters
  , []
  ]

type Term = Term.Term (Sum Syntax) (Record Location)
type Assignment = Assignment' Term
type Assignment' a = HasCallStack => Assignment.Assignment [] Grammar a

assignment :: Assignment
assignment = handleError $ module' <|> parseError

algebraicDatatypeDeclaration :: Assignment
algebraicDatatypeDeclaration = makeTerm
                            <$> symbol AlgebraicDatatypeDeclaration
                            <*> children (Declaration.Datatype
                                        <$> (context' <|> emptyTerm)
                                        <*> (makeTerm <$> location <*> (Syntax.Type <$> typeConstructor <*> typeParameters <*> (kindSignature <|> emptyTerm)))
                                        <*> (constructors <|> pure [])
                                        <*> (term derivingClause <|> emptyTerm))
  where
    constructors = symbol Constructors *> children (manyTerm constructor)

allConstructors :: Assignment
allConstructors = makeTerm <$> token AllConstructors <*> pure Syntax.AllConstructors

alternative :: Assignment
alternative = makeTerm <$> symbol Alternative <*> children (Statement.Pattern <$> expression <*> expressions)

annotatedTypeVariable :: Assignment
annotatedTypeVariable = makeTerm <$> symbol AnnotatedTypeVariable <*> children (Syntax.AnnotatedTypeVariable <$> typeVariableIdentifier <* token Annotation <*> expression)

app :: Assignment
app = makeTerm <$> symbol FunctionApplication <*> children (Syntax.App <$> expression <*> (typeApp <|> emptyTerm) <*> expression)

arithmeticSequence :: Assignment
arithmeticSequence = symbol ArithmeticSequence *> children (  enumFrom
                                                          <|> enumFromThen
                                                          <|> enumFromTo
                                                          <|> enumFromThenTo)
  where
    enumFrom = makeTerm <$> symbol EnumFrom <*> children (Syntax.ArithmeticSequence <$> expression <*> pure Nothing <*> pure Nothing)
    enumFromThen = makeTerm <$> symbol EnumFromThen <*> children (Syntax.ArithmeticSequence <$> expression <*> fmap Just expression <*> pure Nothing)
    enumFromTo = makeTerm <$> symbol EnumFromTo <*> children (Syntax.ArithmeticSequence <$> expression <*> fmap Just expression <*> pure Nothing)
    enumFromThenTo = makeTerm <$> symbol EnumFromThenTo <*> children (Syntax.ArithmeticSequence <$> expression <*> fmap Just expression <*> fmap Just expression)

asPattern :: Assignment
asPattern = makeTerm <$> symbol AsPattern <*> children (Syntax.AsPattern <$> expression <*> expression)

bindPattern :: Assignment
bindPattern = makeTerm <$> symbol BindPattern <*> children (Syntax.BindPattern <$> manyTermsTill expression (symbol AnonLAngleMinus) <*> expression)

case' :: Assignment
case' = makeTerm <$> symbol CaseExpression <*> children (Statement.Match <$> expression <*> expressions)

caseGuardPattern :: Assignment
caseGuardPattern = makeTerm <$> symbol CaseGuardPattern <*> children (Syntax.CaseGuardPattern <$> manyTerm expression)

character :: Assignment
character = makeTerm <$> symbol Char <*> (Literal.Character <$> source)

class' :: Assignment
class' = makeTerm <$> symbol Class <*> children (Syntax.Class <$> manyTerm expression)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

conditionalExpression :: Assignment
conditionalExpression = makeTerm <$> symbol ConditionalExpression <*> children (Statement.If <$> expression <*> expression <*> expression)

constructor :: Assignment
constructor =  (makeTerm <$> symbol DataConstructor <*> children (Declaration.Constructor <$> manyTerm (context' <|> scopedTypeVariables) <*> typeConstructor <*> typeParameters))
           <|> term (makeTerm <$> symbol RecordDataConstructor <*> children (Syntax.RecordDataConstructor <$> manyTerm (context' <|> scopedTypeVariables)  <*> constructorIdentifier <*> term fields))
           <|> term (makeTerm <$> symbol InfixDataConstructor <*> children (Syntax.InfixDataConstructor <$> manyTerm (context' <|> scopedTypeVariables) <*> expression <*> expression <*> expression))

constructorIdentifier :: Assignment
constructorIdentifier = makeTerm <$> symbol ConstructorIdentifier <*> (Syntax.ConstructorIdentifier . Name.name <$> source)

constructorOperator :: Assignment
constructorOperator = makeTerm <$> symbol ConstructorOperator <*> children (Syntax.ConstructorOperator <$> expression)

constructorPattern :: Assignment
constructorPattern = makeTerm <$> symbol ConstructorPattern <*> children (Syntax.ConstructorPattern <$> expressions)

constructorSymbol :: Assignment
constructorSymbol = makeTerm <$> symbol ConstructorSymbol <*> (Syntax.ConstructorSymbol . Name.name <$> source)

context' :: Assignment
context' = makeTerm <$> symbol Context <*> children (Syntax.Context' <$> expressions)

contextPattern :: Assignment
contextPattern = symbol ContextPattern *> children expressions

cppDirective :: Assignment
cppDirective = makeTerm <$> symbol CppDirective <*> (Syntax.CPPDirective <$> source)

defaultDeclaration :: Assignment
defaultDeclaration = makeTerm <$> symbol DefaultDeclaration <*> children (Syntax.DefaultDeclaration <$> manyTerm expression)

defaultSignature :: Assignment
defaultSignature = makeTerm <$> symbol DefaultSignature <*> children (Syntax.DefaultSignature <$> manyTermsTill expression (symbol Annotation) <* token Annotation <*> manyTerm (context' <|> scopedTypeVariables) <*> expressions)

derivingClause :: Assignment
derivingClause = makeTerm <$> symbol Deriving <*> children (Syntax.Deriving <$> manyTerm expression)

do' :: Assignment
do' = makeTerm <$> symbol Do <*> children (Syntax.Do <$> manyTerm expression)

equalityConstraint :: Assignment
equalityConstraint = makeTerm <$> symbol EqualityConstraint <*> children (Syntax.EqualityConstraint <$> equalityLhs <*> equalityRhs)
  where
    equalityLhs = symbol EqualityLhs *> children expression
    equalityRhs = symbol EqualityRhs *> children expression

export :: Assignment
export = makeTerm <$> symbol Export <*> children (Syntax.Export <$> expressions)

expression' :: Assignment
expression' = symbol Expression *> children expressions

expressions :: Assignment
expressions = makeTerm'' <$> location <*> manyTerm expression

expression :: Assignment
expression = term (handleError (choice expressionChoices))

expressionChoices :: [Assignment.Assignment [] Grammar Term]
expressionChoices = [
                      algebraicDatatypeDeclaration
                    , allConstructors
                    , alternative
                    , annotatedTypeVariable
                    , app
                    , arithmeticSequence
                    , asPattern
                    , bindPattern
                    , case'
                    , caseGuardPattern
                    , character
                    , class'
                    , comment
                    , conditionalExpression
                    , context'
                    , contextPattern
                    , constructorIdentifier
                    , constructorOperator
                    , constructorPattern
                    , constructorSymbol
                    , cppDirective
                    , defaultDeclaration
                    , defaultSignature
                    , derivingClause
                    , do'
                    , equalityConstraint
                    , expression'
                    , expressionTypeSignature
                    , fields
                    , fieldBind
                    , fieldPattern
                    , fixityDeclaration
                    , float
                    , functionalDependency
                    , functionConstructor
                    , functionDeclaration
                    , functionGuardPattern
                    , functionType
                    , gadtConstructor
                    , gadtDeclaration
                    , generator
                    , guard'
                    , implicitParameterIdentifier
                    , importAlias
                    , importDeclaration
                    , infixConstructorIdentifier
                    , infixOperatorApp
                    , infixOperatorPattern
                    , infixVariableIdentifier
                    , instance'
                    , integer
                    , irrefutablePattern
                    , kind
                    , kindListType
                    , kindFunctionType
                    , kindParenthesizedConstructor
                    , kindSignature
                    , kindTupleType
                    , labeledConstruction
                    , labeledPattern
                    , labeledUpdate
                    , lambda
                    , lambdaCase
                    , letExpression
                    , letStatement
                    , listConstructor
                    , listComprehension
                    , listExpression
                    , listPattern
                    , listType
                    , moduleExport
                    , moduleIdentifier
                    , namedFieldPun
                    , negativeLiteral
                    , newType
                    , operator
                    , operatorSection
                    , parenthesizedConstructorOperator
                    , parenthesizedExpression
                    , parenthesizedPattern
                    , parenthesizedTypePattern
                    , pattern'
                    , patternGuard
                    , pragma
                    , prefixNegation
                    , primitiveConstructorIdentifier
                    , primitiveVariableIdentifier
                    , promotedTypeOperator
                    , qualifiedConstructorIdentifier
                    , qualifiedImportDeclaration
                    , qualifiedInfixVariableIdentifier
                    , qualifiedModuleIdentifier
                    , qualifiedTypeClassIdentifier
                    , qualifiedTypeConstructorIdentifier
                    , qualifiedVariableIdentifier
                    , quasiQuotation
                    , quasiQuotationDeclaration
                    , quasiQuotationExpression
                    , quasiQuotationExpressionBody
                    , quasiQuotationPattern
                    , quasiQuotationQuoter
                    , quasiQuotationType
                    , quotedName
                    , recordWildCards
                    , scopedTypeVariables
                    , splice
                    , standaloneDerivingInstance
                    , star
                    , strictPattern
                    , strictType
                    , string
                    , tuple
                    , tuplePattern
                    , tupleType
                    , type'
                    , type''
                    , typeApp
                    , typeClass
                    , typeClassIdentifier
                    , typeClassInstance
                    , typeConstructor
                    , typeFamily
                    , typeInstance
                    , typePattern
                    , typeConstructorExport
                    , typeConstructorIdentifier
                    , typeOperator
                    , typeSignature
                    , typeSynonymDeclaration
                    , typeVariableIdentifier
                    , tuplingConstructor
                    , unitConstructor
                    , variableIdentifier
                    , variableOperator
                    , variableSymbol
                    , viewPattern
                    , where'
                    , wildcard
                    ]

expressionTypeSignature :: Assignment
expressionTypeSignature = makeTerm <$> symbol ExpressionTypeSignature <*> children (Syntax.ExpressionTypeSignature <$> manyTermsTill expression (symbol Annotation) <* token Annotation <*> manyTerm (context' <|> scopedTypeVariables) <*> expressions)

fields :: Assignment
fields = makeTerm <$> symbol Fields <*> children (manyTerm field)

field :: Assignment
field = makeTerm
     <$> symbol Field
     <*> children (Syntax.Field
                 <$> variableIdentifiers
                 <* token Annotation
                 <*> fieldType)
  where
    fieldType = makeTerm <$> location <*> (Syntax.Type <$> term (type' <|> typeVariableIdentifier) <*> typeParameters <*> (kindSignature <|> emptyTerm))

fieldBind :: Assignment
fieldBind = makeTerm <$> symbol FieldBind <*> children (Syntax.FieldBind <$> expression <*> expression)

fieldPattern :: Assignment
fieldPattern = makeTerm <$> symbol FieldPattern <*> children (Syntax.FieldPattern <$> expression <*> expressions)

fixityDeclaration :: Assignment
fixityDeclaration = makeTerm <$> symbol FixityDeclaration <*> children (Syntax.Fixity' <$> (integer <|> emptyTerm) <*> manyTerm expression)

float :: Assignment
float = makeTerm <$> symbol Float <*> (Literal.Float <$> source)

functionalDependency :: Assignment
functionalDependency = makeTerm <$> symbol FunctionalDependency <*> children (Syntax.FunctionalDependency <$> expressions)

functionBody :: Assignment
functionBody = makeTerm <$> symbol FunctionBody <*> children (manyTerm expression)

functionConstructor :: Assignment
functionConstructor = makeTerm <$> token FunctionConstructor <*> pure Syntax.FunctionConstructor

functionDeclaration :: Assignment
functionDeclaration = makeTerm
                   <$> symbol FunctionDeclaration
                   <*> children (Declaration.Function []
                               <$> term expression
                               <*> (manyTermsTill expression (symbol FunctionBody) <|> pure [])
                               <*> functionBody)

functionGuardPattern :: Assignment
functionGuardPattern = makeTerm <$> symbol FunctionGuardPattern <*> children (Syntax.FunctionGuardPattern <$> manyTerm expression)

functionType :: Assignment
functionType = makeTerm <$> symbol FunctionType <*> children (Syntax.FunctionType <$> expression <*> expression)

gadtConstructor :: Assignment
gadtConstructor = makeTerm
               <$> symbol GadtConstructor
               <*> children (Syntax.GADTConstructor
                           <$> (context' <|> emptyTerm)
                           <*> expression
                           <* token Annotation
                           <*> expressions)

gadtDeclaration :: Assignment
gadtDeclaration = makeTerm
               <$> symbol GadtDeclaration
               <*> children (Syntax.GADT
                           <$> (context' <|> emptyTerm)
                           <*> (makeTerm <$> location <*> (Syntax.Type <$> typeConstructor <*> typeParameters' <*> (kindSignature <|> emptyTerm)))
                           <*> where')
  where
    typeParameters' = makeTerm <$> location <*> manyTermsTill expression (symbol KindSignature <|> symbol Where')

generator :: Assignment
generator = makeTerm <$> symbol Generator <*> children (Syntax.Generator <$> expression <*> expression)

guard' :: Assignment
guard' = makeTerm <$> symbol Guard <*> children (Syntax.Guard <$> expressions)

hiddenImport :: Assignment
hiddenImport = makeTerm <$> symbol Import <*> children (Syntax.HiddenImport <$> expressions)

hiddenImportSpec :: Assignment.Assignment [] Grammar [Term]
hiddenImportSpec = symbol HiddenImportSpec *> children (manyTerm hiddenImport)

implicitParameterIdentifier :: Assignment
implicitParameterIdentifier = makeTerm <$> symbol ImplicitParameterIdentifier <*> (Syntax.ImplicitParameterIdentifier . Name.name <$> source)

import' :: Assignment
import' = makeTerm <$> symbol Import <*> children (Syntax.Import <$> expressions)

importAlias :: Assignment
importAlias = makeTerm <$> symbol ImportAlias <*> children (Syntax.ImportAlias <$> expression <*> expression)

importDeclaration :: Assignment
importDeclaration = makeTerm
                 <$> symbol ImportDeclaration
                 <*> children (Syntax.ImportDeclaration
                             <$> (packageQualifiedImport <|> emptyTerm)
                             <*> expression
                             <*> (importSpec <|> hiddenImportSpec <|> pure []))

importSpec :: Assignment.Assignment [] Grammar [Term]
importSpec = symbol ImportSpec *> children (manyTerm import')

inClause :: Assignment
inClause = symbol InClause *> children expressions

infixConstructorIdentifier :: Assignment
infixConstructorIdentifier = makeTerm <$> symbol InfixConstructorIdentifier <*> children (Syntax.InfixConstructorIdentifier . Name.name <$> source)

infixOperatorApp :: Assignment
infixOperatorApp = makeTerm <$> symbol InfixOperatorApplication <*> children (Syntax.InfixOperatorApp <$> expression <*> (typeApp <|> emptyTerm) <*> expression <*> (expressions <|> emptyTerm))

infixOperatorPattern :: Assignment
infixOperatorPattern = makeTerm <$> symbol InfixOperatorPattern <*> children (Syntax.InfixOperatorPattern <$> expression <*> operator <*> expression)

infixVariableIdentifier :: Assignment
infixVariableIdentifier = makeTerm <$> symbol InfixVariableIdentifier <*> children (Syntax.InfixVariableIdentifier . Name.name <$> source)

instance' :: Assignment
instance' = makeTerm <$> symbol Instance <*> children (Syntax.Instance <$> expressions)

integer :: Assignment
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

irrefutablePattern :: Assignment
irrefutablePattern = makeTerm <$> symbol IrrefutablePattern <*> children (Syntax.IrrefutablePattern <$> expression)

kind :: Assignment
kind = kind'
    <|> kindFunctionType
    <|> kindListType
    <|> kindParenthesizedConstructor
    <|> kindSignature
    <|> kindTupleType
    <|> star

kind' :: Assignment
kind' = makeTerm <$> symbol Kind <*> children (Syntax.Kind <$> expression)

kindFunctionType :: Assignment
kindFunctionType = makeTerm <$> symbol KindFunctionType <*> children (Syntax.KindFunctionType <$> expression <*> expression)

kindListType :: Assignment
kindListType = makeTerm <$> symbol KindListType <*> children (Syntax.KindListType <$> expression)

kindParenthesizedConstructor :: Assignment
kindParenthesizedConstructor = makeTerm <$> symbol KindParenthesizedConstructor <*> children (Syntax.KindParenthesizedConstructor <$> expression)

kindSignature :: Assignment
kindSignature = makeTerm <$> symbol KindSignature <*> children (Syntax.KindSignature <$ token Annotation <*> expression)

kindTupleType :: Assignment
kindTupleType = makeTerm <$> symbol KindTupleType <*> children (Syntax.KindTupleType <$> manyTerm expression)

labeledConstruction :: Assignment
labeledConstruction = makeTerm <$> symbol LabeledConstruction <*> children (Syntax.LabeledConstruction <$> expression <*> manyTerm expression)

labeledPattern :: Assignment
labeledPattern = makeTerm <$> symbol LabeledPattern <*> children (Syntax.LabeledPattern <$> expressions)

labeledUpdate :: Assignment
labeledUpdate = makeTerm <$> symbol LabeledUpdate <*> children (Syntax.LabeledUpdate <$> manyTerm expression)

lambda :: Assignment
lambda = makeTerm <$> symbol Lambda <*> children (Syntax.Lambda <$> lambdaHead <*> lambdaBody)
  where
    lambdaHead = symbol LambdaHead *> children expressions
    lambdaBody = symbol LambdaBody *> children expressions

lambdaCase :: Assignment
lambdaCase = makeTerm <$> symbol LambdaCase <*> children (Syntax.LambdaCase <$> manyTerm expression)

letExpression :: Assignment
letExpression = makeTerm <$> symbol LetExpression <*> children (Syntax.Let <$> manyTermsTill expression (symbol InClause) <*> inClause)

letStatement :: Assignment
letStatement = makeTerm <$> symbol LetStatement <*> children (Syntax.Let <$> manyTerm expression <*> emptyTerm)

listComprehension :: Assignment
listComprehension = makeTerm <$> symbol ListComprehension <*> children (Syntax.ListComprehension <$> expression <*> manyTerm expression)

listConstructor :: Assignment
listConstructor = makeTerm <$> token ListConstructor <*> pure Syntax.ListConstructor

listExpression :: Assignment
listExpression = makeTerm <$> symbol ListExpression <*> children (Literal.Array <$> manyTerm listElement)
  where listElement = symbol Expression *> children expression

listPattern :: Assignment
listPattern = makeTerm <$> symbol ListPattern <*> children (Syntax.ListPattern <$> expressions)

listType :: Assignment
listType = makeTerm <$> symbol ListType <*> children (Literal.Array <$> manyTerm type')

module' :: Assignment
module' =  makeTerm
       <$> symbol Module
       <*> children (Syntax.Module
                   <$> manyTerm (comment <|> pragma)
                   <*> term (moduleIdentifier <|> qualifiedModuleIdentifier <|> emptyTerm)
                   <*> moduleExports
                   <*> term (where' <|> expressions <|> emptyTerm))
  where
    moduleExports = symbol ModuleExports *> children (manyTerm export)
                 <|> pure []

moduleExport :: Assignment
moduleExport = makeTerm <$> symbol ModuleExport <*> children (Syntax.ModuleExport <$> expressions)

moduleIdentifier :: Assignment
moduleIdentifier = makeTerm <$> symbol ModuleIdentifier <*> (Syntax.ModuleIdentifier . Name.name <$> source)

namedFieldPun :: Assignment
namedFieldPun = makeTerm <$> symbol NamedFieldPun <*> children (Syntax.NamedFieldPun <$> expression)

negativeLiteral :: Assignment
negativeLiteral = makeTerm <$> symbol NegativeLiteral <*> children (Syntax.NegativeLiteral <$> expression)

newConstructor :: Assignment
newConstructor = makeTerm <$> symbol NewConstructor <*> children (Declaration.Constructor <$> manyTerm (context' <|> scopedTypeVariables) <*> expression <*> expressions)

newType :: Assignment
newType = makeTerm <$> symbol NewtypeDeclaration <*> children (Syntax.NewType <$> manyTerm (context' <|> scopedTypeVariables) <*> typeLeft <*> newConstructor <*> (derivingClause <|> emptyTerm))
  where
    typeLeft = makeTerm <$> location <*> manyTermsTill expression (symbol NewConstructor)

operator :: Assignment
operator =  constructorOperator
        <|> typeOperator
        <|> promotedTypeOperator
        <|> variableOperator

operatorSection :: Assignment
operatorSection = (makeTerm <$> symbol RightOperatorSection <*> children (Syntax.RightOperatorSection <$> expression <*> expression))
               <|> (makeTerm <$> symbol LeftOperatorSection <*> children (Syntax.LeftOperatorSection <$> expression <*> expression))

packageQualifiedImport :: Assignment
packageQualifiedImport = makeTerm <$> symbol PackageQualifiedImport <*> (Literal.TextElement <$> source)

parenthesizedConstructorOperator :: Assignment
parenthesizedConstructorOperator = symbol ParenthesizedConstructorOperator *> children expression

parenthesizedExpression :: Assignment
parenthesizedExpression = symbol ParenthesizedExpression *> children expressions

parenthesizedPattern :: Assignment
parenthesizedPattern = symbol ParenthesizedPattern *> children expressions

parenthesizedTypePattern :: Assignment
parenthesizedTypePattern = symbol ParenthesizedTypePattern *> children expressions

pattern' :: Assignment
pattern' = symbol Pattern *> children expressions

patternGuard :: Assignment
patternGuard = makeTerm <$> symbol PatternGuard <*> children (Syntax.PatternGuard <$> expression <*> (expression <|> emptyTerm))

pragma :: Assignment
pragma = makeTerm <$> symbol Pragma <*> (Syntax.Pragma <$> source)

prefixNegation :: Assignment
prefixNegation = makeTerm <$> symbol PrefixNegation <*> children (Syntax.PrefixNegation <$> expression)

primitiveConstructorIdentifier :: Assignment
primitiveConstructorIdentifier = makeTerm <$> symbol PrimitiveConstructorIdentifier <*> (Syntax.PrimitiveConstructorIdentifier . Name.name <$> source)

primitiveVariableIdentifier :: Assignment
primitiveVariableIdentifier = makeTerm <$> symbol PrimitiveVariableIdentifier <*> (Syntax.PrimitiveVariableIdentifier . Name.name <$> source)

promotedTypeOperator :: Assignment
promotedTypeOperator = makeTerm <$> symbol PromotedTypeOperator <*> children (Syntax.PromotedTypeOperator <$> expression)

qualifiedConstructorIdentifier :: Assignment
qualifiedConstructorIdentifier = makeTerm <$> symbol QualifiedConstructorIdentifier <*> children (Syntax.QualifiedConstructorIdentifier <$> someTerm' expression)

qualifiedImportDeclaration :: Assignment
qualifiedImportDeclaration = makeTerm
                          <$> symbol QualifiedImportDeclaration
                          <*> children (Syntax.QualifiedImportDeclaration
                                      <$> (packageQualifiedImport <|> emptyTerm)
                                      <*> expression
                                      <*> (importSpec <|> hiddenImportSpec <|> pure []))

qualifiedInfixVariableIdentifier :: Assignment
qualifiedInfixVariableIdentifier = makeTerm <$> symbol QualifiedInfixVariableIdentifier <*> children (Syntax.QualifiedInfixVariableIdentifier <$> someTerm' expression)

qualifiedModuleIdentifier :: Assignment
qualifiedModuleIdentifier = makeTerm <$> symbol QualifiedModuleIdentifier <*> children (Syntax.QualifiedModuleIdentifier <$> someTerm' expression)

qualifiedTypeClassIdentifier :: Assignment
qualifiedTypeClassIdentifier = makeTerm <$> symbol QualifiedTypeClassIdentifier <*> children (Syntax.QualifiedTypeClassIdentifier <$> someTerm' expression)

qualifiedTypeConstructorIdentifier :: Assignment
qualifiedTypeConstructorIdentifier = makeTerm <$> symbol QualifiedTypeConstructorIdentifier <*> children (Syntax.QualifiedTypeConstructorIdentifier <$> someTerm' expression)

qualifiedVariableIdentifier :: Assignment
qualifiedVariableIdentifier = makeTerm <$> symbol QualifiedVariableIdentifier <*> children (Syntax.QualifiedVariableIdentifier <$> someTerm' expression)

quasiQuotation :: Assignment
quasiQuotation = makeTerm <$> symbol QuasiQuotation <*> children (Syntax.QuasiQuotation <$> (expression <|> emptyTerm) <*> expression)

quasiQuotationDeclaration :: Assignment
quasiQuotationDeclaration = makeTerm <$> token QuasiQuotationDeclaration <*> pure Syntax.QuasiQuotationDeclaration

quasiQuotationExpression :: Assignment
quasiQuotationExpression = makeTerm <$> token QuasiQuotationExpression <*> pure Syntax.QuasiQuotationExpression

quasiQuotationExpressionBody :: Assignment
quasiQuotationExpressionBody = makeTerm <$> symbol QuasiQuotationExpressionBody <*> (Syntax.QuasiQuotationExpressionBody . Name.name <$> source)

quasiQuotationPattern :: Assignment
quasiQuotationPattern = makeTerm <$> token QuasiQuotationPattern <*> pure Syntax.QuasiQuotationPattern

quasiQuotationQuoter :: Assignment
quasiQuotationQuoter = makeTerm <$> symbol QuasiQuotationQuoter <*> (Syntax.QuasiQuotationQuoter . Name.name <$> source)

quasiQuotationType :: Assignment
quasiQuotationType = makeTerm <$> token QuasiQuotationType <*> pure Syntax.QuasiQuotationType

quotedName :: Assignment
quotedName = makeTerm <$> symbol QuotedName <*> children (Syntax.QuotedName <$> expression)

recordWildCards :: Assignment
recordWildCards = makeTerm <$> symbol RecordWildCards <*> (Syntax.RecordWildCards <$ source)

scopedTypeVariables :: Assignment
scopedTypeVariables = makeTerm <$> symbol ScopedTypeVariables <*> children (Syntax.ScopedTypeVariables <$> expressions <* token Dot)

splice :: Assignment
splice = makeTerm <$> symbol Splice <*> children (Syntax.Splice <$> expression)

standaloneDerivingInstance :: Assignment
standaloneDerivingInstance = makeTerm <$> symbol StandaloneDerivingDeclaration <*> children (Syntax.StandaloneDerivingInstance <$> manyTerm (context' <|> scopedTypeVariables) <*> expression <*> instance')
  where
    instance' = symbol Instance *> children expressions

star :: Assignment
star = makeTerm <$> token Star <*> pure Syntax.Star

strictPattern :: Assignment
strictPattern = makeTerm <$> symbol StrictPattern <*> children (Syntax.StrictPattern <$> expression)

strictType :: Assignment
strictType = makeTerm'
          <$> symbol StrictType
          <*> children (  (inject <$> (Syntax.StrictType <$> typeConstructor <*> typeParameters))
                      <|> (inject <$> (Syntax.StrictTypeVariable <$> expression)))

string :: Assignment
string = makeTerm <$> symbol String <*> (Literal.TextElement <$> source)

tuple :: Assignment
tuple = makeTerm <$> symbol TupleExpression <*> children (Syntax.Tuple <$> manyTerm expression)

tuplePattern :: Assignment
tuplePattern = makeTerm <$> symbol TuplePattern <*> children (Syntax.TuplePattern <$> manyTerm expression)

tupleType :: Assignment
tupleType = makeTerm <$> symbol TupleType <*> children (Literal.Tuple <$> manyTerm expression)

tuplingConstructor :: Assignment
tuplingConstructor = makeTerm <$> symbol TuplingConstructor <*> (tupleWithArity <$> rawSource)
        -- a tuple (,) has arity two, but only one comma, so apply the successor to the count of commas for the correct arity.
  where tupleWithArity = Syntax.TupleConstructor . succ . count ','

type' :: Assignment
type' =  class'
     <|> fields
     <|> functionType
     <|> parenthesizedTypePattern
     <|> strictType
     <|> type''
     <|> typeConstructor
     <|> typePattern

type'' :: Assignment
type'' = makeTerm
      <$> symbol Type
      <*> children (Syntax.Type <$> expression <*> typeParameters <*> (kindSignature <|> emptyTerm))

typeApp :: Assignment
typeApp = makeTerm <$> symbol TypeApplication <*> children (Syntax.TypeApp <$> expression)

typeClass :: Assignment
typeClass = makeTerm <$> symbol TypeClassDeclaration <*> children (Syntax.TypeClass
                                                                 <$> (context' <|> emptyTerm)
                                                                 <*> expression
                                                                 <*> manyTermsTill expression (symbol Where)
                                                                 <*> where')

typeClassIdentifier :: Assignment
typeClassIdentifier = makeTerm <$> symbol TypeClassIdentifier <*> (Syntax.TypeClassIdentifier . Name.name <$> source)

typeClassInstance :: Assignment
typeClassInstance = makeTerm <$> symbol TypeClassInstanceDeclaration <*> children (Syntax.TypeClassInstance
                                                                      <$> manyTerm (context' <|> scopedTypeVariables)
                                                                      <*> expression
                                                                      <*> expression
                                                                      <*> (where' <|> emptyTerm))

typeConstructor :: Assignment
typeConstructor =  constructorIdentifier
               <|> functionConstructor
               <|> listConstructor
               <|> listType
               <|> qualifiedModuleIdentifier
               <|> qualifiedTypeClassIdentifier
               <|> qualifiedTypeConstructorIdentifier
               <|> quotedName
               <|> tupleType
               <|> tuplingConstructor
               <|> typeClassIdentifier
               <|> typeConstructorIdentifier
               <|> unitConstructor

typeConstructorExport :: Assignment
typeConstructorExport = makeTerm <$> symbol TypeConstructorExport <*> children (Syntax.TypeConstructorExport <$> expression)

typeConstructorIdentifier :: Assignment
typeConstructorIdentifier = makeTerm <$> symbol TypeConstructorIdentifier <*> (Syntax.TypeConstructorIdentifier . Name.name <$> source)

typeFamily :: Assignment
typeFamily = makeTerm <$> symbol TypeFamilyDeclaration <*> children (Syntax.TypeFamily <$> expression <*> manyTermsTill expression typeFamilySeperator <*> (typeSignature <|> kindSignature <|> emptyTerm) <*> (where' <|> emptyTerm))
  where
    typeFamilySeperator =  symbol TypeSignature
                       <|> symbol KindSignature
                       <|> symbol Where

typeInstance :: Assignment
typeInstance = makeTerm <$> symbol TypeInstanceDeclaration <*> children (Syntax.TypeInstance <$> typeInstanceType <*> typeInstanceBody)
  where
    typeInstanceType = makeTerm <$> location <*> manyTermsTill expression (symbol TypeInstanceBody)
    typeInstanceBody = symbol TypeInstanceBody *> children expressions

typeOperator :: Assignment
typeOperator = makeTerm <$> symbol TypeOperator <*> (Syntax.TypeOperator . Name.name <$> source)

typeSignature :: Assignment
typeSignature = makeTerm <$> symbol TypeSignature <*> children (Syntax.TypeSignature <$> manyTermsTill expression (symbol Annotation) <* token Annotation <*> manyTerm (context' <|> scopedTypeVariables) <*> expressions)

typeParameters :: Assignment
typeParameters = makeTerm <$> location <*> (Type.TypeParameters <$> (manyTermsTill expression (symbol Annotation) <|> manyTerm expression))

typePattern :: Assignment
typePattern = makeTerm <$> symbol TypePattern <*> children (Syntax.TypePattern <$> expressions)

typeSynonymDeclaration :: Assignment
typeSynonymDeclaration = makeTerm
                      <$> symbol TypeSynonymDeclaration
                      <*> children (typeSynonym <$> typeLeft <*> typeRight)
  where
    typeLeft = makeTerm <$> location <*> manyTill expression typeRightSeperator
    typeRight = (symbol TypeSynonymBody *> children ((,) <$> manyTerm (context' <|> scopedTypeVariables) <*> expression))
             <|> ((,) [] <$> typeSignature)
             <|> ((,) [] <$> kindSignature)
    typeRightSeperator =  symbol TypeSynonymBody
                      <|> symbol TypeSignature
                      <|> symbol KindSignature
    typeSynonym typeLeft (contexts, typeRight) = Syntax.TypeSynonym typeLeft contexts typeRight

typeVariableIdentifier :: Assignment
typeVariableIdentifier = makeTerm <$> symbol TypeVariableIdentifier <*> (Syntax.TypeVariableIdentifier . Name.name <$> source)

unitConstructor :: Assignment
unitConstructor = makeTerm <$> token UnitConstructor <*> pure Syntax.UnitConstructor

variableIdentifier :: Assignment
variableIdentifier = makeTerm <$> symbol VariableIdentifier <*> (Syntax.VariableIdentifier . Name.name <$> source)

variableOperator :: Assignment
variableOperator = makeTerm <$> symbol VariableOperator <*> children (Syntax.VariableOperator <$> expression)

variableSymbol :: Assignment
variableSymbol = makeTerm <$> (symbol VariableSymbol <|> symbol VariableSymbol') <*> (Syntax.VariableSymbol . Name.name <$> source)

variableIdentifiers :: Assignment
variableIdentifiers = makeTerm <$> location <*> many variableIdentifier

viewPattern :: Assignment
viewPattern = makeTerm <$> symbol ViewPattern <*> children (Syntax.ViewPattern <$> expression <*> expression)

where' :: Assignment
where' = makeTerm <$> (symbol Where <|> symbol Where') <*> children (manyTerm expression)

wildcard :: Assignment
wildcard = makeTerm <$> token Wildcard <*> pure Syntax.Wildcard

-- | Helpers

commentedTerm :: Assignment -> Assignment
commentedTerm term = contextualize (comment <|> pragma) term <|> makeTerm1 <$> (Syntax.Context <$> some1 (comment <|> pragma) <*> emptyTerm)

manyTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
manyTerm = many . commentedTerm

manyTermsTill :: Assignment.Assignment [] Grammar Term -> Assignment.Assignment [] Grammar b -> Assignment.Assignment [] Grammar [Term]
manyTermsTill step = manyTill (step <|> comment)

someTerm' :: Assignment -> Assignment.Assignment [] Grammar (NonEmpty Term)
someTerm' = NonEmpty.some1 . commentedTerm

term :: Assignment -> Assignment
term term = contextualize (comment <|> pragma) (postContextualize (comment <|> pragma) term)
