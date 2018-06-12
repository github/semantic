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
  , Syntax.AllConstructors
  , Syntax.AnnotatedTypeVariable
  , Syntax.App
  , Syntax.ArithmeticSequence
  , Syntax.BindPattern
  , Syntax.Class
  , Syntax.ConstructorPattern
  , Syntax.ConstructorSymbol
  , Syntax.Context
  , Syntax.Context'
  , Syntax.DefaultDeclaration
  , Syntax.Deriving
  , Syntax.Do
  , Syntax.Empty
  , Syntax.EntityIdentifier
  , Syntax.Error
  , Syntax.EqualityConstraint
  , Syntax.Export
  , Syntax.Field
  , Syntax.FunctionConstructor
  , Syntax.FunctionType
  , Syntax.GADT
  , Syntax.GADTConstructor
  , Syntax.Generator
  , Syntax.HiddenImport
  , Syntax.Identifier
  , Syntax.Import
  , Syntax.ImportAlias
  , Syntax.ImportDeclaration
  , Syntax.InfixOperatorPattern
  , Syntax.Kind
  , Syntax.KindFunctionType
  , Syntax.KindListType
  , Syntax.KindSignature
  , Syntax.Lambda
  , Syntax.ListComprehension
  , Syntax.ListConstructor
  , Syntax.Module
  , Syntax.ModuleExport
  , Syntax.NewType
  , Syntax.Operator
  , Syntax.OperatorSection
  , Syntax.Pragma
  , Syntax.PrefixNegation
  , Syntax.QualifiedEntityIdentifier
  , Syntax.QualifiedImportDeclaration
  , Syntax.QuotedName
  , Syntax.RecordDataConstructor
  , Syntax.ScopedTypeVariables
  , Syntax.StandaloneDerivingInstance
  , Syntax.Star
  , Syntax.StrictType
  , Syntax.StrictTypeVariable
  , Syntax.Tuple
  , Syntax.TupleConstructor
  , Syntax.TuplePattern
  , Syntax.Type
  , Syntax.TypeConstructorExport
  , Syntax.TypePattern
  , Syntax.TypeSignature
  , Syntax.TypeSynonym
  , Syntax.UnitConstructor
  , Syntax.VariableSymbol
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
                                        <*> ((symbol Constructors *> children (manyTerm constructor))
                                            <|> pure [])
                                        <*> (derivingClause <|> emptyTerm))

allConstructors :: Assignment
allConstructors = makeTerm <$> token AllConstructors <*> pure Syntax.AllConstructors

annotatedTypeVariable :: Assignment
annotatedTypeVariable = makeTerm <$> symbol AnnotatedTypeVariable <*> children (Syntax.AnnotatedTypeVariable <$> typeVariableIdentifier <* token Annotation <*> (kind <|> type'))

app :: Assignment
app = makeTerm <$> symbol FunctionApplication <*> children (Syntax.App <$> expression <*> expression)

arithmeticSequence :: Assignment
arithmeticSequence = symbol ArithmeticSequence *> children (  enumFrom
                                                          <|> enumFromThen
                                                          <|> enumFromTo
                                                          <|> enumFromThenTo)
  where
    enumFrom = makeTerm <$> symbol EnumFrom <*> children (Syntax.EnumFrom <$> expression)
    enumFromThen = makeTerm <$> symbol EnumFromThen <*> children (Syntax.EnumFromThen <$> expression <*> expression)
    enumFromTo = makeTerm <$> symbol EnumFromTo <*> children (Syntax.EnumFromTo <$> expression <*> expression)
    enumFromThenTo = makeTerm <$> symbol EnumFromThenTo <*> children (Syntax.EnumFromThenTo <$> expression <*> expression <*> expression)

bindPattern :: Assignment
bindPattern = makeTerm <$> symbol BindPattern <*> children (Syntax.BindPattern <$> expression <*> expression)

character :: Assignment
character = makeTerm <$> symbol Char <*> (Literal.Character <$> source)

class' :: Assignment
class' = makeTerm <$> symbol Class <*> children (Syntax.Class <$> manyTerm expression)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

constructor :: Assignment
constructor =  (makeTerm <$> symbol DataConstructor <*> children (Declaration.Constructor <$> manyTerm (context' <|> scopedTypeVariables) <*> typeConstructor <*> typeParameters))
           <|> (makeTerm <$> symbol RecordDataConstructor <*> children (Syntax.RecordDataConstructor <$> constructorIdentifier <*> fields))

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
contextPattern = symbol ContextPattern *> children (expressions)

defaultDeclaration :: Assignment
defaultDeclaration = makeTerm <$> symbol DefaultDeclaration <*> children (Syntax.DefaultDeclaration <$> manyTerm expression)

derivingClause :: Assignment
derivingClause = makeTerm <$> symbol Deriving <*> children (Syntax.Deriving <$> manyTerm typeConstructor)

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
expression' = symbol Expression *> children (expression)

expressions :: Assignment
expressions = makeTerm'' <$> location <*> manyTerm expression

expression :: Assignment
expression = term (handleError (choice expressionChoices))

expressionChoices :: [Assignment.Assignment [] Grammar Term]
expressionChoices = [
                      algebraicDatatypeDeclaration
                    , allConstructors
                    , annotatedTypeVariable
                    , app
                    , arithmeticSequence
                    , bindPattern
                    , character
                    , comment
                    , context'
                    , contextPattern
                    , constructorIdentifier
                    , constructorOperator
                    , constructorPattern
                    , constructorSymbol
                    , defaultDeclaration
                    , derivingClause
                    , do'
                    , equalityConstraint
                    , expression'
                    , float
                    , functionConstructor
                    , functionDeclaration
                    , functionType
                    , gadtConstructor
                    , gadtDeclaration
                    , generator
                    , importAlias
                    , importDeclaration
                    , infixOperatorApp
                    , infixOperatorPattern
                    , infixVariableIdentifier
                    , integer
                    , kind
                    , kindSignature
                    , lambda
                    , listConstructor
                    , listComprehension
                    , listExpression
                    , listType
                    , moduleExport
                    , moduleIdentifier
                    , newType
                    , operator
                    , operatorSection
                    , parenthesizedExpression
                    , parenthesizedPattern
                    , parenthesizedTypePattern
                    , pattern'
                    , pragma
                    , prefixNegation
                    , primitiveConstructorIdentifier
                    , primitiveVariableIdentifier
                    , qualifiedConstructorIdentifier
                    , qualifiedImportDeclaration
                    , qualifiedInfixVariableIdentifier
                    , qualifiedModuleIdentifier
                    , qualifiedTypeConstructorIdentifier
                    , qualifiedVariableIdentifier
                    , quotedName
                    , scopedTypeVariables
                    , standaloneDerivingInstance
                    , star
                    , strictType
                    , string
                    , tuple
                    , tuplePattern
                    , tupleType
                    , type'
                    , type''
                    , typeClassIdentifier
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
                    , where'
                    ]

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

float :: Assignment
float = makeTerm <$> symbol Float <*> (Literal.Float <$> source)

functionBody :: Assignment
functionBody = makeTerm <$> symbol FunctionBody <*> children (manyTerm expression)

functionConstructor :: Assignment
functionConstructor = makeTerm <$> token FunctionConstructor  <*> pure Syntax.FunctionConstructor

functionDeclaration :: Assignment
functionDeclaration = makeTerm
                   <$> symbol FunctionDeclaration
                   <*> children (Declaration.Function
                               <$> pure []
                               <*> variableIdentifier
                               <*> (manyTermsTill expression (symbol FunctionBody) <|> pure [])
                               <*> functionBody)

functionType :: Assignment
functionType = makeTerm <$> symbol FunctionType <*> children (Syntax.FunctionType <$> type' <*> type')

gadtConstructor :: Assignment
gadtConstructor = makeTerm
               <$> symbol GadtConstructor
               <*> children (Syntax.GADTConstructor
                           <$> (context' <|> emptyTerm)
                           <*> typeConstructor
                           <* token Annotation
                           <*> term type')

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

hiddenImport :: Assignment
hiddenImport = makeTerm <$> symbol Import <*> children (Syntax.HiddenImport <$> expressions)

hiddenImportSpec :: Assignment.Assignment [] Grammar [Term]
hiddenImportSpec = symbol HiddenImportSpec *> children (manyTerm hiddenImport)

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

infixOperatorApp :: Assignment
infixOperatorApp = makeTerm <$> symbol InfixOperatorApplication <*> children (Syntax.InfixOperatorApp <$> expression <*> expression <*> expression)

infixOperatorPattern :: Assignment
infixOperatorPattern = makeTerm <$> symbol InfixOperatorPattern <*> children (Syntax.InfixOperatorPattern <$> expression <*> operator <*> expression)

infixVariableIdentifier :: Assignment
infixVariableIdentifier = makeTerm <$> symbol InfixVariableIdentifier <*> children (Syntax.InfixVariableIdentifier . Name.name <$> source)

integer :: Assignment
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

kind :: Assignment
kind = kind'
    <|> kindFunctionType
    <|> kindListType
    <|> star

kind' :: Assignment
kind' = makeTerm <$> symbol Kind <*> children (Syntax.Kind <$> kind)

kindFunctionType :: Assignment
kindFunctionType = makeTerm <$> symbol KindFunctionType <*> children (Syntax.KindFunctionType <$> kind <*> kind)

kindListType :: Assignment
kindListType = makeTerm <$> symbol KindListType <*> children (Syntax.KindListType <$> kind)

kindSignature :: Assignment
kindSignature = makeTerm <$> symbol KindSignature <*> children (Syntax.KindSignature <$ token Annotation <*> kind)

lambda :: Assignment
lambda = makeTerm <$> symbol Lambda <*> children (Syntax.Lambda <$> lambdaHead <*> lambdaBody)
  where
    lambdaHead = symbol LambdaHead *> children (expressions)
    lambdaBody = symbol LambdaBody *> children (expressions)

listComprehension :: Assignment
listComprehension = makeTerm <$> symbol ListComprehension <*> children (Syntax.ListComprehension <$> expression <*> manyTerm expression)

listConstructor :: Assignment
listConstructor = makeTerm <$> token ListConstructor <*> pure Syntax.ListConstructor

listExpression :: Assignment
listExpression = makeTerm <$> symbol ListExpression <*> children (Literal.Array <$> manyTerm listElement)
  where listElement = symbol Expression *> children expression

listType :: Assignment
listType = makeTerm <$> symbol ListType <*> children (Literal.Array <$> manyTerm type')

module' :: Assignment
module' = makeTerm
       <$> symbol Module
       <*> children (Syntax.Module
                   <$> (term moduleIdentifier <|> emptyTerm)
                   <*> moduleExports
                   <*> (where' <|> expressions <|> emptyTerm))
  where
    moduleExports = symbol ModuleExports *> children (manyTerm export)
                 <|> pure []

moduleExport :: Assignment
moduleExport = makeTerm <$> symbol ModuleExport <*> children (Syntax.ModuleExport <$> expressions)

moduleIdentifier :: Assignment
moduleIdentifier = makeTerm <$> symbol ModuleIdentifier <*> (Syntax.ModuleIdentifier . Name.name <$> source)

newConstructor :: Assignment
newConstructor = makeTerm <$> symbol NewConstructor <*> children (Declaration.Constructor <$> manyTerm (context' <|> scopedTypeVariables) <*> typeConstructor <*> typeParameters)

newType :: Assignment
newType = makeTerm <$> symbol NewtypeDeclaration <*> children (Syntax.NewType <$> manyTerm (context' <|> scopedTypeVariables) <*> typeLeft <*> newConstructor <*> (derivingClause <|> emptyTerm))
  where
    typeLeft = makeTerm <$> location <*> manyTermsTill expression (symbol NewConstructor)

operator :: Assignment
operator = typeOperator <|> constructorOperator <|> variableOperator

operatorSection :: Assignment
operatorSection = (makeTerm <$> symbol RightOperatorSection <*> children (Syntax.RightOperatorSection <$> expression <*> expression))
               <|> (makeTerm <$> symbol LeftOperatorSection <*> children (Syntax.LeftOperatorSection <$> expression <*> expression))

packageQualifiedImport :: Assignment
packageQualifiedImport = makeTerm <$> symbol PackageQualifiedImport <*> (Literal.TextElement <$> source)

parenthesizedExpression :: Assignment
parenthesizedExpression = symbol ParenthesizedExpression *> children expression

parenthesizedPattern :: Assignment
parenthesizedPattern = symbol ParenthesizedPattern *> children expressions

parenthesizedTypePattern :: Assignment
parenthesizedTypePattern = symbol ParenthesizedTypePattern *> children expressions

pattern' :: Assignment
pattern' = symbol Pattern *> children (expression)

pragma :: Assignment
pragma = makeTerm <$> symbol Pragma <*> (Syntax.Pragma <$> source)

prefixNegation :: Assignment
prefixNegation = makeTerm <$> symbol PrefixNegation <*> children (Syntax.PrefixNegation <$> expression)

primitiveConstructorIdentifier :: Assignment
primitiveConstructorIdentifier = makeTerm <$> symbol PrimitiveConstructorIdentifier <*> (Syntax.PrimitiveConstructorIdentifier . Name.name <$> source)

primitiveVariableIdentifier :: Assignment
primitiveVariableIdentifier = makeTerm <$> symbol PrimitiveVariableIdentifier <*> (Syntax.PrimitiveVariableIdentifier . Name.name <$> source)

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

qualifiedTypeConstructorIdentifier :: Assignment
qualifiedTypeConstructorIdentifier = makeTerm <$> symbol QualifiedTypeConstructorIdentifier <*> children (Syntax.QualifiedTypeConstructorIdentifier <$> someTerm' expression)

qualifiedVariableIdentifier :: Assignment
qualifiedVariableIdentifier = makeTerm <$> symbol QualifiedVariableIdentifier <*> children (Syntax.QualifiedVariableIdentifier <$> someTerm' expression)

quotedName :: Assignment
quotedName = makeTerm <$> symbol QuotedName <*> children (Syntax.QuotedName <$> expression)

scopedTypeVariables :: Assignment
scopedTypeVariables = makeTerm <$> symbol ScopedTypeVariables <*> children (Syntax.ScopedTypeVariables <$> expressions <* token Dot)

standaloneDerivingInstance :: Assignment
standaloneDerivingInstance = makeTerm <$> symbol StandaloneDerivingDeclaration <*> children (Syntax.StandaloneDerivingInstance <$> manyTerm (context' <|> scopedTypeVariables) <*> expression <*> instance')
  where
    instance' = symbol Instance *> children (expressions)

star :: Assignment
star = makeTerm <$> token Star <*> pure Syntax.Star

strictType :: Assignment
strictType = makeTerm'
          <$> symbol StrictType
          <*> children (  (inject <$> (Syntax.StrictType <$> typeConstructor <*> typeParameters))
                      <|> (inject <$> (Syntax.StrictTypeVariable <$> expression)))

string :: Assignment
string = makeTerm <$> symbol String <*> (Literal.TextElement <$> source)

tuplePattern :: Assignment
tuplePattern = makeTerm <$> symbol TuplePattern <*> children (Syntax.TuplePattern <$> manyTerm expression)

tupleType :: Assignment
tupleType = makeTerm <$> symbol TupleType <*> children (Literal.Tuple <$> manyTerm type')

typeClassIdentifier :: Assignment
typeClassIdentifier = makeTerm <$> symbol TypeClassIdentifier <*> (Syntax.TypeClassIdentifier . Name.name <$> source)

typeConstructorExport :: Assignment
typeConstructorExport = makeTerm <$> symbol TypeConstructorExport <*> children (Syntax.TypeConstructorExport <$> expression)

typeConstructorIdentifier :: Assignment
typeConstructorIdentifier = makeTerm <$> symbol TypeConstructorIdentifier <*> (Syntax.TypeConstructorIdentifier . Name.name <$> source)

typeOperator :: Assignment
typeOperator = makeTerm <$> symbol TypeOperator <*> (Syntax.TypeOperator . Name.name <$> source)

typeSignature :: Assignment
typeSignature = makeTerm <$> symbol TypeSignature <*> children (Syntax.TypeSignature <$> variableIdentifier <* token Annotation <*> manyTerm (context' <|> scopedTypeVariables) <*> expressions)

typeVariableIdentifier :: Assignment
typeVariableIdentifier = makeTerm <$> symbol TypeVariableIdentifier <*> (Syntax.TypeVariableIdentifier . Name.name <$> source)

tuple :: Assignment
tuple = makeTerm <$> symbol TupleExpression <*> children (Syntax.Tuple <$> manyTerm expression)

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

typeParameters :: Assignment
typeParameters = makeTerm <$> location <*> (Type.TypeParameters <$> (manyTermsTill expression (symbol Annotation) <|> manyTerm expression))

typePattern :: Assignment
typePattern = makeTerm <$> symbol TypePattern <*> children (Syntax.TypePattern <$> expressions)

typeConstructor :: Assignment
typeConstructor =  constructorIdentifier
               <|> functionConstructor
               <|> listConstructor
               <|> listType
               <|> qualifiedModuleIdentifier
               <|> qualifiedTypeConstructorIdentifier
               <|> quotedName
               <|> tupleType
               <|> tuplingConstructor
               <|> typeClassIdentifier
               <|> typeConstructorIdentifier
               <|> unitConstructor

typeSynonymDeclaration :: Assignment
typeSynonymDeclaration = makeTerm
                      <$> symbol TypeSynonymDeclaration
                      <*> children (typeSynonym <$> typeLeft <*> typeRight)
  where
    typeLeft = makeTerm <$> location <*> manyTill expression (symbol TypeSynonymBody)
    typeRight = symbol TypeSynonymBody *> children ((,) <$> manyTerm (context' <|> scopedTypeVariables) <*> expression)
    typeSynonym typeLeft (contexts, typeRight) = Syntax.TypeSynonym typeLeft contexts typeRight

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

where' :: Assignment
where' = makeTerm <$> (symbol Where <|> symbol Where') <*> children (manyTerm expression)

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
